# HTTP client -----------------------------------------------------------------

.tg_default_base_url <- function() {
  "https://api.transferegov.gestao.gov.br"
}

#' The API base URL in use
#'
#' Reports the base URL requests are sent to. Set the `transferegovr.base_url`
#' option to point the package at a mirror or a test double.
#'
#' @return A single string.
#' @export
#' @family configuration
#' @examples
#' tg_base_url()
tg_base_url <- function() {
  getOption("transferegovr.base_url", .tg_default_base_url())
}

.tg_timeout <- function() {
  getOption("transferegovr.timeout", 60)
}

.tg_max_tries <- function() {
  getOption("transferegovr.max_tries", 4L)
}

.tg_rate <- function() {
  getOption("transferegovr.requests_per_minute", 60)
}

.tg_user_agent <- function() {
  getOption(
    "transferegovr.user_agent",
    paste0(
      "transferegovr/",
      utils::packageVersion("transferegovr"),
      " (https://github.com/StrategicProjects/transferegovr)"
    )
  )
}

.tg_check_base_url <- function(base_url, call = rlang::caller_env()) {
  if (
    !is.character(base_url) ||
      length(base_url) != 1L ||
      is.na(base_url) ||
      !grepl("^https?://", base_url)
  ) {
    cli::cli_abort(
      "{.arg .base_url} must be a single HTTP or HTTPS URL.",
      class = "transferegovr_url_error",
      call = call
    )
  }
  invisible(NULL)
}

# `query` is a named list whose names may repeat: PostgREST reads two parameters
# with the same column name as two conditions combined with AND, which is how
# `col = list(gte(1), lte(5))` is expressed.
.tg_request <- function(module, table, query, count = FALSE, base_url) {
  .tg_check_base_url(base_url)

  req <- httr2::request(sub("/+$", "", base_url)) |>
    httr2::req_url_path_append(module, table) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent(.tg_user_agent()) |>
    httr2::req_timeout(seconds = .tg_timeout()) |>
    httr2::req_throttle(
      capacity = .tg_rate(),
      fill_time_s = 60,
      realm = "transferegov"
    ) |>
    httr2::req_retry(
      max_tries = .tg_max_tries(),
      retry_on_failure = TRUE,
      is_transient = .tg_is_transient,
      backoff = .tg_backoff
    ) |>
    # Status is inspected by `.tg_abort_for_status()`, which reads the PostgREST
    # error body. Letting httr2 raise first would discard it.
    httr2::req_error(is_error = function(resp) FALSE)

  if (isTRUE(count)) {
    # Without this header PostgREST reports the total as "*" and multi-page
    # collection has nothing to bound itself with.
    req <- httr2::req_headers(req, Prefer = "count=exact")
  }

  if (length(query) > 0L) {
    req <- httr2::req_url_query(req, !!!query, .multi = "explode")
  }

  .check_url_length(req)

  req
}

# A filter built with `in_()` over a few thousand identifiers produces a URL the
# service cannot accept, and the failure it produces is not readable: curl
# reports "Error in the HTTP2 framing layer", which says nothing about the
# query. Failing here names the cause instead.
.tg_max_url <- 7000L

.check_url_length <- function(req, call = rlang::caller_env()) {
  length <- nchar(req$url, type = "bytes")

  if (length <= .tg_max_url) {
    return(invisible(NULL))
  }

  # Assigned to a local first: cli reads `{.foo}` as a style, so interpolating
  # a name that starts with a dot errors.
  maximum <- .tg_max_url

  cli::cli_abort(
    c(
      "The request URL is {length} bytes, over the {maximum} the service
       accepts.",
      "i" = "A filter built with {.fn in_} over a long vector is the usual
             cause.",
      "i" = "Split the values into batches of a few hundred and bind the
             results."
    ),
    class = "transferegovr_url_error",
    call = call
  )
}

# 5xx from the gateway and 429 from the service are worth retrying. A 400 is
# PostgREST rejecting the query itself and will fail identically every time.
.tg_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
}

.tg_backoff <- function(tries) {
  min(60, 2^tries) * stats::runif(1, 0.5, 1.5)
}

.tg_perform <- function(req, call = rlang::caller_env()) {
  response <- httr2::req_perform(req)
  .tg_abort_for_status(response, call = call)

  body <- tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(error) {
      cli::cli_abort(
        "The TransfereGov API returned a body that is not valid JSON.",
        class = "transferegovr_response_error",
        parent = error,
        call = call
      )
    }
  )

  if (!is.list(body) || !is.null(names(body))) {
    cli::cli_abort(
      c(
        "The TransfereGov API returned an unexpected payload.",
        "i" = "A table query must answer with a JSON array of rows."
      ),
      class = "transferegovr_response_error",
      call = call
    )
  }

  list(
    rows = body,
    range = .tg_parse_content_range(
      httr2::resp_header(response, "Content-Range")
    ),
    status = httr2::resp_status(response)
  )
}

# PostgREST reports errors as a JSON object carrying the Postgres SQLSTATE and
# message, which name the offending column. Surfacing them turns an opaque 400
# into something the caller can act on.
.tg_abort_for_status <- function(response, call = rlang::caller_env()) {
  status <- httr2::resp_status(response)

  if (status < 400L) {
    return(invisible(response))
  }

  body <- tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(error) NULL
  )

  message <- paste0(
    "The TransfereGov API returned HTTP ", status,
    " (", httr2::resp_status_desc(response), ")."
  )

  # A list, not a character vector: `detail[["message"]]` on an empty character
  # vector aborts with "subscript out of bounds" rather than returning NULL,
  # which would replace the status being reported with an unrelated error.
  detail <- list()
  if (is.list(body)) {
    for (field in c("message", "details", "hint")) {
      value <- body[[field]]
      if (is.character(value) && length(value) == 1L && nzchar(value)) {
        detail[[field]] <- value
      }
    }
  }

  bullets <- c(message)
  for (field in c("message", "details", "hint")) {
    if (is.null(detail[[field]])) {
      next
    }
    # The text is escaped and placed in the bullet rather than interpolated
    # from a variable: cli evaluates `{...}` when the condition is raised, by
    # which point a loop variable holds only its last value, so all three
    # bullets would show the same text.
    bullets <- c(bullets, stats::setNames(
      .tg_escape_braces(detail[[field]]),
      if (field == "message") "x" else "i"
    ))
  }
  if (status == 400L && is.null(detail[["hint"]])) {
    bullets <- c(
      bullets,
      "i" = "Check the column names with {.fn tg_fields}."
    )
  }

  cli::cli_abort(
    bullets,
    class = c(
      "transferegovr_http_error",
      paste0("transferegovr_http_", status)
    ),
    call = call
  )
}

# `Content-Range` carries the pagination state: "0-99/6176" with an exact count,
# "0-99/*" without one, and "*/0" for an empty result.
# cli interpolates `{...}` in every bullet, so text coming from the API has to
# be escaped or a message carrying a brace would be evaluated as an expression.
.tg_escape_braces <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}

.tg_parse_content_range <- function(header) {
  empty <- list(first = NA_integer_, last = NA_integer_, total = NA_real_)

  if (is.null(header) || is.na(header) || !nzchar(header)) {
    return(empty)
  }

  header <- trimws(sub("^items\\s+", "", header))
  match <- regmatches(
    header,
    regexec("^(\\*|([0-9]+)-([0-9]+))/(\\*|[0-9]+)$", header)
  )[[1L]]

  if (length(match) == 0L) {
    return(empty)
  }

  list(
    first = if (match[[2L]] == "*") NA_integer_ else as.integer(match[[3L]]),
    last = if (match[[2L]] == "*") NA_integer_ else as.integer(match[[4L]]),
    # Kept as a double: a table with more rows than .Machine$integer.max would
    # become NA as an integer and silently disable the completeness check.
    total = if (match[[5L]] == "*") NA_real_ else as.numeric(match[[5L]])
  )
}
