# Query parameters ------------------------------------------------------------
#
# A filter is one of the endpoint's own query parameters. There is no operator
# vocabulary: the services compare for equality, combine parameters with AND,
# and on some identifier parameters accept a list meaning "is one of".
#
# Every name is checked against the frozen parameter list before the request
# goes out. That check is load-bearing rather than a convenience. These
# services answer 200 and ignore a parameter they do not recognize, so
# `situacao_proposta` misspelt as `in_situacao_proposta` returns the whole
# table. Without the check, a typo reads as "no rows matched that
# restriction" — the answer looks plausible and is wrong.

#' List the parameters a table accepts as filters
#'
#' Every parameter may be passed to [tg_get()] and [tg_count()] as a named
#' argument. Parameter names and their permitted values are in Portuguese
#' because they belong to the API.
#'
#' @inheritParams tg_fields
#'
#' @return A tibble with one row per parameter: its name, the R type a value
#'   should have, the type the API declares, the permitted values when the
#'   parameter is enumerated, the pattern a value must match when it has one,
#'   its description, whether it accepts several values (`multiple`), and how
#'   many at most (`max_values`).
#' @export
#' @family discovery
#' @examples
#' tg_params("parcerias", "proposta")
#'
#' # Which parameters accept only a fixed set of values?
#' params <- tg_params("parcerias", "proposta")
#' params[lengths(params$values) > 0, c("param", "values")]
#'
#' # Which accept several values at once?
#' params[params$multiple, c("param", "max_values")]
tg_params <- function(module, table) {
  module <- .tg_match_module(module)
  table <- .tg_match_table(module, table)
  .tg_table_params(module, table)
}

#' @rdname tg_params
#' @export
tg_parametros <- function(modulo, tabela) {
  tg_params(modulo, tabela)
}

.tg_table_params <- function(module, table) {
  .tg_schema[[module]]$tables[[table]]$params
}

# Evaluation ------------------------------------------------------------------

# Filters supplied through `...` are captured unevaluated only so that a bare
# name in the caller's environment cannot be mistaken for a column name; there
# is no data mask, because there are no operator constructors to shadow.
.tg_eval_filters <- function(quosures, params, call = rlang::caller_env()) {
  if (length(quosures) == 0L) {
    return(list())
  }

  names <- names(quosures)
  if (is.null(names) || !all(nzchar(names))) {
    cli::cli_abort(
      c(
        "Every filter in {.arg ...} must be named after a parameter.",
        "i" = "For example {.code situacao_proposta = \"Aprovada\"}.",
        "i" = "See {.fn tg_params} for the parameters this table accepts."
      ),
      class = "transferegovr_filter_error",
      call = call
    )
  }

  duplicated <- unique(names[duplicated(names)])
  if (length(duplicated) > 0L) {
    cli::cli_abort(
      c(
        "Filter{?s} {.val {duplicated}} {?was/were} given more than once.",
        "i" = "The API keeps the last value of a repeated parameter and
               discards the rest without reporting it."
      ),
      class = "transferegovr_filter_error",
      call = call
    )
  }

  .tg_validate_params(names, params, call)

  # `lapply()`, not `purrr::map()`: purrr wraps errors in `purrr_error_indexed`,
  # which would strip the condition class raised below.
  values <- lapply(seq_along(quosures), function(i) {
    rlang::eval_tidy(quosures[[i]])
  })
  names(values) <- names

  values <- values[!vapply(values, is.null, logical(1))]

  out <- lapply(names(values), function(name) {
    .tg_encode_filter(values[[name]], name, params, call)
  })
  names(out) <- names(values)

  out
}

.tg_validate_params <- function(names, params, call = rlang::caller_env()) {
  if (length(names) == 0L || !.tg_validate()) {
    return(invisible(NULL))
  }

  unknown <- setdiff(unique(names), params$param)

  if (length(unknown) == 0L) {
    return(invisible(NULL))
  }

  message <- c(
    "Unknown filter{?s}: {.val {unknown}}.",
    "x" = "The API ignores a parameter it does not recognize and returns
           every row, so this would look like a query that matched nothing
           in particular."
  )

  suggestions <- .tg_suggest(unknown, params$param)
  if (length(suggestions) > 0L) {
    message <- c(message, "i" = "Did you mean {.val {suggestions}}?")
  }

  cli::cli_abort(
    c(
      message,
      "i" = "See {.fn tg_params} for the parameters this table accepts.",
      "i" = "The packaged schema is from {.val {format(tg_schema_date())}}. If
             the API has gained a parameter since, set
             {.code options(transferegovr.validate = FALSE)}."
    ),
    class = "transferegovr_filter_error",
    call = call
  )
}

# The closest known parameter to each unknown one, when it is close enough to
# be worth offering. Names here are long and share prefixes, so the tolerance
# scales with length rather than being a fixed number of edits.
.tg_suggest <- function(unknown, known) {
  if (length(known) == 0L) {
    return(character())
  }

  distances <- utils::adist(unknown, known)

  suggestions <- vapply(seq_along(unknown), function(i) {
    best <- which.min(distances[i, ])
    tolerance <- max(2, floor(nchar(unknown[[i]]) / 3))

    if (distances[i, best] <= tolerance) known[[best]] else NA_character_
  }, character(1))

  unique(suggestions[!is.na(suggestions)])
}

# Encoding --------------------------------------------------------------------

.tg_encode_filter <- function(value, name, params, call) {
  if (length(value) == 0L) {
    cli::cli_abort(
      "Filter {.arg {name}} is empty.",
      class = "transferegovr_filter_error",
      call = call
    )
  }

  if (is.list(value)) {
    cli::cli_abort(
      "Filter {.arg {name}} must be a single value, not a list.",
      class = "transferegovr_filter_error",
      call = call
    )
  }

  index <- match(name, params$param)
  multiple <- !is.na(index) && isTRUE(params$multiple[index])

  if (length(value) > 1L && multiple) {
    return(.tg_encode_list(value, name, params$max_values[[index]], call))
  }

  # Elsewhere there is no way to express "is one of" in one request: the
  # parameter takes one value, and a repeated parameter silently keeps the
  # last. The honest answer is to refuse and say what to do instead, rather
  # than issue several requests behind a signature that promises one.
  if (length(value) > 1L) {
    cli::cli_abort(
      c(
        "Filter {.arg {name}} has {length(value)} values, and the API accepts
         one.",
        "i" = "Query each value and bind the results, for example
               {.code purrr::list_rbind(lapply(values, function(v)
               tg_get(module, table, {name} = v)))}.",
        "i" = "Only the parameters {.fn tg_params} marks as {.field multiple}
               take several values in one request."
      ),
      class = "transferegovr_filter_error",
      call = call
    )
  }

  if (is.na(value)) {
    cli::cli_abort(
      c(
        "Filter {.arg {name}} must not be missing.",
        "i" = "These APIs cannot filter for a null column."
      ),
      class = "transferegovr_filter_error",
      call = call
    )
  }

  encoded <- .tg_encode_value(value)
  .tg_validate_value(encoded, name, params, call)

  encoded
}

# A list-taking parameter wants whole numbers separated by commas, and matches
# rows holding any of them. The service rejects anything else with a 400 and
# caps the count -- 100 in `especiais`, 200 elsewhere -- so both are checked
# before the request rather than discovered after it.
.tg_encode_list <- function(value, name, max_values, call) {
  if (anyNA(value)) {
    cli::cli_abort(
      c(
        "Filter {.arg {name}} must not contain missing values.",
        "i" = "These APIs cannot filter for a null column."
      ),
      class = "transferegovr_filter_error",
      call = call
    )
  }

  whole <- if (is.numeric(value)) {
    all(is.finite(value) & value >= 0 & value %% 1 == 0)
  } else {
    is.character(value) && all(grepl("^[0-9]+$", value))
  }
  if (!whole) {
    cli::cli_abort(
      "Filter {.arg {name}} takes whole numbers when given several values.",
      class = "transferegovr_filter_error",
      call = call
    )
  }

  value <- unique(value)
  if (length(value) > max_values) {
    cli::cli_abort(
      c(
        "Filter {.arg {name}} has {length(value)} values, and the API accepts
         at most {max_values}.",
        "i" = "Split them into groups of {max_values} and bind the results."
      ),
      class = "transferegovr_filter_error",
      call = call
    )
  }

  paste(
    vapply(value, .tg_encode_value, character(1), USE.NAMES = FALSE),
    collapse = ","
  )
}

.tg_encode_value <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
  if (is.logical(x)) {
    return(if (x) "true" else "false")
  }
  if (is.numeric(x)) {
    # `format()` would render 1e+05 for a plain integer-valued double, which
    # the service rejects as an integer.
    return(format(x, scientific = FALSE, trim = TRUE, digits = 15))
  }

  as.character(x)
}

# An enumerated parameter is checked here rather than left to the service. The
# service does reject a bad value with a 422, but only after a request, and its
# message does not say which of the fifty-odd parameters is enumerated.
.tg_validate_value <- function(encoded, name, params, call) {
  if (!.tg_validate()) {
    return(invisible(NULL))
  }

  index <- match(name, params$param)
  if (is.na(index)) {
    return(invisible(NULL))
  }

  permitted <- params$values[[index]]
  if (length(permitted) == 0L || encoded %in% permitted) {
    return(invisible(NULL))
  }

  suggestions <- .tg_suggest(encoded, permitted)
  message <- c(
    "{.val {encoded}} is not a permitted value for {.arg {name}}."
  )
  if (length(suggestions) > 0L) {
    message <- c(message, "i" = "Did you mean {.val {suggestions}}?")
  }

  cli::cli_abort(
    c(message, "i" = "It accepts {.val {permitted}}."),
    class = "transferegovr_filter_error",
    call = call
  )
}
