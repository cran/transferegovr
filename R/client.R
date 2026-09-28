# HTTP client -----------------------------------------------------------------
#
# The four modules are FastAPI services. A table query is a GET on the
# endpoint, filters are typed query parameters, and the answer is an envelope
# carrying the rows under `data` alongside the pagination state.

.tg_default_base_url <- function() {
  "https://api-publica.transferegov.gestao.gov.br"
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

# `query` is a named list of single values; a list-taking parameter's several
# values arrive already joined by commas. Unlike the PostgREST services these
# replaced, repeating a parameter here does not combine two conditions: the
# service keeps the last occurrence and discards the rest without saying so.
# `.tg_eval_filters()` is what guarantees each name appears once.
.tg_request <- function(path, query, base_url) {
  .tg_check_base_url(base_url)

  req <- httr2::request(sub("/+$", "", base_url)) |>
    httr2::req_url_path_append(path) |>
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
    # Status is inspected by `.tg_abort_for_status()`, which reads the
    # validation errors FastAPI reports. Letting httr2 raise first would
    # discard them.
    httr2::req_error(is_error = function(resp) FALSE)

  if (length(query) > 0L) {
    req <- httr2::req_url_query(req, !!!query, .multi = "explode")
  }

  .check_url_length(req)

  req
}

# A filter over a few thousand identifiers produces a URL the service cannot
# accept, and the failure it produces is not readable: curl reports "Error in
# the HTTP2 framing layer", which says nothing about the query. Failing here
# names the cause instead.
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
      "i" = "A very long filter value is the usual cause."
    ),
    class = "transferegovr_url_error",
    call = call
  )
}

# 5xx from the gateway and 429 from the service are worth retrying. A 422 is
# the service rejecting the query itself and will fail identically every time.
.tg_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
}

.tg_backoff <- function(tries) {
  min(60, 2^tries) * stats::runif(1, 0.5, 1.5)
}

# Table responses --------------------------------------------------------------

# The envelope every table endpoint answers with. `total_items` is what bounds
# multi-page collection, and its absence has to be an error rather than a
# silent switch to unbounded paging.
.tg_envelope_fields <- c(
  "data", "total_pages", "total_items", "page_number", "page_size"
)

.tg_perform <- function(req, call = rlang::caller_env()) {
  body <- .tg_perform_json(req, call = call)

  missing <- setdiff(.tg_envelope_fields, names(body))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "The TransfereGov API returned an unexpected payload.",
        "x" = "Its response carried no {.field {missing}}.",
        "i" = "A table query must answer with a paginated envelope."
      ),
      class = "transferegovr_response_error",
      call = call
    )
  }

  if (!is.list(body$data) || !is.null(names(body$data))) {
    cli::cli_abort(
      c(
        "The TransfereGov API returned an unexpected payload.",
        "i" = "Its {.field data} field must be a JSON array of rows."
      ),
      class = "transferegovr_response_error",
      call = call
    )
  }

  list(
    rows = body$data,
    # Kept as a double: a table with more rows than .Machine$integer.max would
    # become NA as an integer and silently disable the completeness check.
    total = .tg_envelope_number(body$total_items, "total_items", call),
    page = .tg_envelope_number(body$page_number, "page_number", call),
    page_size = .tg_envelope_number(body$page_size, "page_size", call)
  )
}

.tg_envelope_number <- function(value, field, call) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
    cli::cli_abort(
      c(
        "The TransfereGov API reported no usable {.field {field}}.",
        "i" = "Multi-page collection has nothing to bound itself with."
      ),
      class = "transferegovr_response_error",
      call = call
    )
  }

  as.numeric(value)
}

.tg_perform_json <- function(req, call = rlang::caller_env()) {
  response <- httr2::req_perform(req)
  .tg_abort_for_status(response, call = call)

  tryCatch(
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
}

# FastAPI reports a rejected query as `detail`, which is a list of objects
# naming the offending parameter for a 422 and a bare string otherwise.
# Surfacing them turns an opaque status into something the caller can act on.
.tg_abort_for_status <- function(response, call = rlang::caller_env()) {
  status <- httr2::resp_status(response)

  if (status < 400L) {
    return(invisible(response))
  }

  body <- tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(error) NULL
  )

  bullets <- paste0(
    "The TransfereGov API returned HTTP ", status,
    " (", httr2::resp_status_desc(response), ")."
  )

  for (detail in .tg_error_details(body)) {
    # The text is escaped and placed in the bullet rather than interpolated
    # from a variable: cli evaluates `{...}` when the condition is raised, by
    # which point a loop variable holds only its last value, so every bullet
    # would show the same text.
    bullets <- c(bullets, stats::setNames(.tg_escape_braces(detail), "x"))
  }

  if (status == 422L) {
    bullets <- c(
      bullets,
      "i" = "Check the parameter names and values with {.fn tg_params}."
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

# `detail` is a string for a 404 and a list of validation objects for a 422,
# each carrying `loc` as a path like list("query", "situacao_proposta").
.tg_error_details <- function(body) {
  if (!is.list(body) || is.null(body$detail)) {
    return(character())
  }

  detail <- body$detail

  if (is.character(detail) && length(detail) == 1L && nzchar(detail)) {
    return(detail)
  }

  if (!is.list(detail)) {
    return(character())
  }

  messages <- vapply(
    detail,
    function(item) {
      if (!is.list(item)) {
        return(NA_character_)
      }

      message <- item$msg
      if (!is.character(message) || length(message) != 1L) {
        return(NA_character_)
      }

      where <- unlist(item$loc, use.names = FALSE)
      where <- where[!where %in% "query"]

      if (length(where) == 0L) {
        message
      } else {
        paste0(paste(where, collapse = "."), ": ", message)
      }
    },
    character(1)
  )

  messages[!is.na(messages)]
}

# cli interpolates `{...}` in every bullet, so text coming from the API has to
# be escaped or a message carrying a brace would be evaluated as an expression.
.tg_escape_braces <- function(x) {
  gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE), fixed = TRUE)
}
