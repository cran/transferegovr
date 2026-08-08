# Filter operators ------------------------------------------------------------
#
# The TransfereGov APIs are PostgREST services, so a filter is a query parameter
# whose name is the column and whose value is `operator.operand`. These helpers
# build that value while keeping the escaping rules in one place.
#
# They are evaluated inside a data mask (see `.tg_eval_filters()`), so a user
# who has attached a package exporting `gt()`, `like()` or `not()` still gets
# these.

#' Filter operators
#'
#' Constructors for the comparison operators the 'PostgREST' services behind the
#' TransfereGov APIs accept. Pass them as named arguments to [tg_get()] or
#' [tg_count()], where the name is the column being filtered.
#'
#' A bare value is shorthand for `eq()`, and a bare vector of length greater
#' than one is shorthand for `in_()`, so `aa_ano_plano_acao = 2024` and
#' `aa_ano_plano_acao = c(2024, 2025)` both work. Pass a list of operators to
#' apply several conditions to the same column, which the API combines with AND:
#' `dt_inicio_vigencia = list(gte("2024-01-01"), lt("2025-01-01"))`.
#'
#' In `like()` and `ilike()` the wildcard may be written as `*` or `%`. In
#' `re_match()` and `re_imatch()` the operand is a POSIX regular expression.
#'
#' @param x A single value to compare against. `Date` and `POSIXct` values are
#'   formatted for the API; logicals become `true` and `false`.
#' @param values A vector of values for `in_()`.
#' @param pattern A pattern for `like()`, `ilike()`, `re_match()` and
#'   `re_imatch()`.
#' @param filter A filter built by one of the other operators, to be negated.
#'
#' @return An object of class `tg_filter`.
#' @name filters
#' @family filters
#' @examples
#' gte(2024)
#' in_(c("PE", "PB"))
#' not(is_null())
#'
#' if (interactive()) {
#'   tg_get("ted", "plano_acao", aa_ano_plano_acao = gte(2024))
#' }
NULL

.tg_filter <- function(op, value = NULL, negate = FALSE, quote_all = FALSE) {
  structure(
    list(op = op, value = value, negate = negate, quote_all = quote_all),
    class = "tg_filter"
  )
}

.tg_check_scalar <- function(x, call = rlang::caller_env()) {
  if (length(x) != 1L || is.list(x)) {
    cli::cli_abort(
      "The operand must be a single value, not {.obj_type_friendly {x}} of
       length {length(x)}.",
      class = "transferegovr_filter_error",
      call = call
    )
  }
  if (is.na(x)) {
    cli::cli_abort(
      c(
        "The operand must not be missing.",
        "i" = "Use {.fn is_null} to match rows where the column is null."
      ),
      class = "transferegovr_filter_error",
      call = call
    )
  }
  invisible(NULL)
}

.tg_check_pattern <- function(x, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort(
      "The pattern must be a single string.",
      class = "transferegovr_filter_error",
      call = call
    )
  }
  invisible(NULL)
}

#' @rdname filters
#' @export
eq <- function(x) {
  .tg_check_scalar(x)
  .tg_filter("eq", x)
}

#' @rdname filters
#' @export
neq <- function(x) {
  .tg_check_scalar(x)
  .tg_filter("neq", x)
}

#' @rdname filters
#' @export
gt <- function(x) {
  .tg_check_scalar(x)
  .tg_filter("gt", x)
}

#' @rdname filters
#' @export
gte <- function(x) {
  .tg_check_scalar(x)
  .tg_filter("gte", x)
}

#' @rdname filters
#' @export
lt <- function(x) {
  .tg_check_scalar(x)
  .tg_filter("lt", x)
}

#' @rdname filters
#' @export
lte <- function(x) {
  .tg_check_scalar(x)
  .tg_filter("lte", x)
}

#' @rdname filters
#' @export
like <- function(pattern) {
  .tg_check_pattern(pattern)
  .tg_filter("like", pattern)
}

#' @rdname filters
#' @export
ilike <- function(pattern) {
  .tg_check_pattern(pattern)
  .tg_filter("ilike", pattern)
}

#' @rdname filters
#' @export
re_match <- function(pattern) {
  .tg_check_pattern(pattern)
  .tg_filter("match", pattern)
}

#' @rdname filters
#' @export
re_imatch <- function(pattern) {
  .tg_check_pattern(pattern)
  .tg_filter("imatch", pattern)
}

#' @rdname filters
#' @export
in_ <- function(values) {
  if (length(values) == 0L || is.list(values)) {
    cli::cli_abort(
      "{.fn in_} needs a non-empty atomic vector.",
      class = "transferegovr_filter_error"
    )
  }
  if (anyNA(values)) {
    cli::cli_abort(
      c(
        "{.fn in_} values must not be missing.",
        "i" = "Use {.fn is_null} to match rows where the column is null."
      ),
      class = "transferegovr_filter_error"
    )
  }
  # Every element is quoted: an unquoted comma inside a value would be read as
  # a separator and silently widen the set being matched.
  .tg_filter("in", values, quote_all = TRUE)
}

#' @rdname filters
#' @export
is_null <- function() {
  .tg_filter("is", "null")
}

#' @rdname filters
#' @export
is_true <- function() {
  .tg_filter("is", "true")
}

#' @rdname filters
#' @export
is_false <- function() {
  .tg_filter("is", "false")
}

#' @rdname filters
#' @export
not <- function(filter) {
  if (!inherits(filter, "tg_filter")) {
    cli::cli_abort(
      c(
        "{.fn not} needs a filter built by another operator.",
        "i" = "For example {.code not(eq(1))} or {.code not(is_null())}."
      ),
      class = "transferegovr_filter_error"
    )
  }
  if (isTRUE(filter$negate)) {
    cli::cli_abort(
      "A filter cannot be negated twice.",
      class = "transferegovr_filter_error"
    )
  }
  filter$negate <- TRUE
  filter
}

#' @export
format.tg_filter <- function(x, ...) {
  .tg_filter_string(x)
}

#' @export
print.tg_filter <- function(x, ...) {
  # `cat()`, not cli: cli redirects to stderr whenever stdout is sunk, which is
  # exactly what testthat does, and a print method belongs on stdout.
  cat("<tg_filter> ", .tg_filter_string(x), "\n", sep = "")
  invisible(x)
}

# Serialization ---------------------------------------------------------------

# The set of operator constructors, used both to build the evaluation mask and
# to document the vocabulary in `tg_operators()`. A list, not an environment:
# `eval_tidy()` deprecated bare environments as masks.
.tg_operator_mask <- function() {
  list(
    eq = eq, neq = neq, gt = gt, gte = gte, lt = lt, lte = lte,
    like = like, ilike = ilike, re_match = re_match, re_imatch = re_imatch,
    in_ = in_, is_null = is_null, is_true = is_true, is_false = is_false,
    not = not
  )
}

.tg_operator_table <- tibble::tibble(
  operator = c(
    "eq", "neq", "gt", "gte", "lt", "lte", "like", "ilike",
    "re_match", "re_imatch", "in_", "is_null", "is_true", "is_false", "not"
  ),
  postgrest = c(
    "eq", "neq", "gt", "gte", "lt", "lte", "like", "ilike",
    "match", "imatch", "in", "is.null", "is.true", "is.false", "not"
  ),
  meaning = c(
    "equals", "does not equal", "greater than", "greater than or equal to",
    "less than", "less than or equal to", "matches pattern, case sensitive",
    "matches pattern, case insensitive",
    "matches regular expression, case sensitive",
    "matches regular expression, case insensitive",
    "is one of", "is null", "is true", "is false", "negates another operator"
  )
)

# PostgREST reads the first "." as the operator separator and treats "," "(" ")"
# as structure, so a value carrying any of them, or leading or trailing spaces,
# has to be double quoted. Values that need no quoting are left alone: quoting a
# `like` pattern would be a behaviour change, not just a formatting one.
.tg_needs_quotes <- function(x) {
  grepl("[,()\"\\\\]", x) || grepl("^\\s|\\s$", x) || !nzchar(x)
}

.tg_quote <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  paste0("\"", x, "\"")
}

.tg_encode_value <- function(x, quote_all = FALSE) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
  }
  if (is.logical(x)) {
    return(ifelse(x, "true", "false"))
  }
  if (is.numeric(x)) {
    # `format()` would render 1e+05 for a plain integer-valued double, which the
    # API compares as text for some column types.
    return(format(x, scientific = FALSE, trim = TRUE, digits = 15))
  }

  x <- as.character(x)
  needs <- quote_all |
    vapply(x, .tg_needs_quotes, logical(1), USE.NAMES = FALSE)
  ifelse(needs, vapply(x, .tg_quote, character(1), USE.NAMES = FALSE), x)
}

.tg_filter_string <- function(filter) {
  encoded <- .tg_encode_value(filter$value, filter$quote_all)

  operand <- if (identical(filter$op, "in")) {
    paste0("(", paste(encoded, collapse = ","), ")")
  } else {
    encoded
  }

  paste0(
    if (isTRUE(filter$negate)) "not." else "",
    filter$op, ".", operand
  )
}

# Filters supplied through `...` are captured unevaluated so that the operator
# mask takes priority over anything the user has attached, then converted to the
# repeated query parameters PostgREST expects.
.tg_eval_filters <- function(quosures, call = rlang::caller_env()) {
  if (length(quosures) == 0L) {
    return(list())
  }

  names <- names(quosures)
  if (is.null(names) || !all(nzchar(names))) {
    cli::cli_abort(
      c(
        "Every filter in {.arg ...} must be named after a column.",
        "i" = "For example {.code aa_ano_plano_acao = gte(2024)}."
      ),
      class = "transferegovr_filter_error",
      call = call
    )
  }

  mask <- .tg_operator_mask()

  # `lapply()`, not `purrr::map()`: purrr wraps errors in `purrr_error_indexed`,
  # which would strip the condition class an operator raised.
  values <- lapply(seq_along(quosures), function(i) {
    rlang::eval_tidy(quosures[[i]], data = mask)
  })
  names(values) <- names

  values <- values[!vapply(values, is.null, logical(1))]

  out <- list()
  for (column in names(values)) {
    for (string in .tg_column_filters(values[[column]], column, call)) {
      out[[length(out) + 1L]] <- string
      names(out)[length(out)] <- column
    }
  }

  out
}

# A column may carry one filter, a bare value, a bare vector, or a list of
# filters to be ANDed. Each becomes one query parameter; PostgREST ANDs repeated
# parameters on the same column.
.tg_column_filters <- function(value, column, call) {
  if (inherits(value, "tg_filter")) {
    return(.tg_filter_string(value))
  }

  if (is.list(value)) {
    if (length(value) == 0L) {
      cli::cli_abort(
        "Filter {.arg {column}} is an empty list.",
        class = "transferegovr_filter_error",
        call = call
      )
    }
    if (!all(vapply(value, inherits, logical(1), "tg_filter"))) {
      cli::cli_abort(
        c(
          "Filter {.arg {column}} must be a value, a vector, or a list of
           operators.",
          "i" = "See {.fn tg_operators} for the available operators."
        ),
        class = "transferegovr_filter_error",
        call = call
      )
    }
    return(vapply(value, .tg_filter_string, character(1)))
  }

  if (length(value) == 0L) {
    cli::cli_abort(
      "Filter {.arg {column}} is empty.",
      class = "transferegovr_filter_error",
      call = call
    )
  }

  if (anyNA(value)) {
    cli::cli_abort(
      c(
        "Filter {.arg {column}} must not be missing.",
        "i" = "Use {.code {column} = is_null()} to match null values."
      ),
      class = "transferegovr_filter_error",
      call = call
    )
  }

  .tg_filter_string(if (length(value) == 1L) eq(value) else in_(value))
}

#' List the available filter operators
#'
#' @return A tibble with the exported operator, the 'PostgREST' operator it
#'   sends, and what it means.
#' @export
#' @family filters
#' @examples
#' tg_operators()
tg_operators <- function() {
  .tg_operator_table
}

#' @rdname tg_operators
#' @export
tg_operadores <- tg_operators
