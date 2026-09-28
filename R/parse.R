# JSON to tibble --------------------------------------------------------------
#
# The services answer with an array of objects under `data`, one per row, with
# a JSON `null` wherever the column is NULL. Columns are typed from the frozen
# schema rather than guessed, so a column made entirely of nulls on one page
# does not come back logical while the next page returns it as character.
#
# A few columns hold an array of objects rather than a scalar. They become list
# columns; `tg_fields(nested = )` describes what is inside.

.tg_rows_to_tibble <- function(rows, fields) {
  names <- .tg_row_names(rows) %||% fields$field

  if (length(names) == 0L) {
    return(tibble::tibble())
  }

  values <- lapply(names, function(name) {
    .tg_coerce(
      lapply(rows, function(row) row[[name]]),
      .tg_type_of(name, fields),
      name
    )
  })
  names(values) <- names

  tibble::as_tibble(values, .name_repair = "minimal")
}

# The column set comes from the response, not the schema: `.select` narrows it,
# and a column added upstream since the schema was frozen must still come
# through. Every key seen is kept, in the order the first row presents them.
.tg_row_names <- function(rows) {
  if (length(rows) == 0L) {
    return(NULL)
  }

  names <- unique(unlist(lapply(rows, names), use.names = FALSE))

  if (length(names) == 0L) {
    return(NULL)
  }

  names
}

.tg_type_of <- function(name, fields) {
  index <- match(name, fields$field)

  # A column the frozen schema does not know is typed by inspection rather than
  # dropped, so the package keeps working when the API gains a column.
  if (is.na(index)) NA_character_ else fields$r_type[[index]]
}

.tg_coerce <- function(values, type, name) {
  # A column the schema declares as an array stays a list column even when
  # every row happens to hold an empty one, so its class does not depend on
  # which page was fetched.
  if (identical(type, "list")) {
    return(lapply(values, function(v) if (is.null(v)) list() else v))
  }

  # A JSON object or array in a column the schema did not declare as one: keep
  # it whole in a list column rather than flatten it into something that no
  # longer round-trips.
  nested <- vapply(values, function(v) is.list(v) && length(v) > 0L, logical(1))
  if (any(nested)) {
    return(lapply(values, function(v) if (is.null(v)) NA else v))
  }

  missing <- vapply(
    values, function(v) is.null(v) || length(v) == 0L, logical(1)
  )

  if (all(missing)) {
    return(.tg_na_vector(type, length(values)))
  }

  flat <- rep(NA_character_, length(values))
  flat[!missing] <- vapply(
    values[!missing],
    function(v) as.character(v)[[1L]],
    character(1)
  )

  if (is.na(type)) {
    type <- .tg_infer_type(values[!missing])
  }

  switch(type,
    logical = .tg_as_logical(values, missing),
    integer = .tg_as_integer(flat, name),
    double = .tg_as_double(flat, name),
    Date = .tg_as_date(flat, name),
    POSIXct = .tg_as_datetime(flat, name),
    flat
  )
}

.tg_na_vector <- function(type, n) {
  if (is.na(type)) {
    return(rep(NA, n))
  }

  switch(type,
    logical = rep(NA, n),
    integer = rep(NA_integer_, n),
    double = rep(NA_real_, n),
    Date = as.Date(rep(NA_character_, n)),
    POSIXct = as.POSIXct(rep(NA_character_, n), tz = "UTC"),
    rep(NA_character_, n)
  )
}

.tg_infer_type <- function(present) {
  if (all(vapply(present, is.logical, logical(1)))) {
    return("logical")
  }
  if (all(vapply(present, is.numeric, logical(1)))) {
    return("double")
  }
  "character"
}

.tg_as_logical <- function(values, missing) {
  out <- rep(NA, length(values))
  out[!missing] <- vapply(
    values[!missing],
    function(v) isTRUE(as.logical(v)[[1L]]),
    logical(1)
  )
  out
}

# `as.numeric()` turns anything it cannot parse into NA with a warning that is
# easy to miss, which would empty a column rather than report it. That is not
# hypothetical here: the API declares
# `codigo_conta_beneficiario_subtransacao_gestao_financeira` as an integer and
# then sends "***" in it, a masked account number. Left unchecked the column
# comes back all NA and reads as "the API has no value", which is wrong.
.tg_as_double <- function(flat, name) {
  parsed <- suppressWarnings(as.numeric(flat))

  if (any(is.na(parsed) & !is.na(flat))) {
    return(.tg_warn_unparsed(flat, name, "numeric"))
  }

  parsed
}

# `as.integer()` turns anything beyond .Machine$integer.max into NA with a
# warning that is easy to miss, which would quietly empty an identifier column.
# The schema types those columns as double, so reaching this branch means the
# API sent something wider than it declared.
.tg_as_integer <- function(flat, name) {
  numeric <- suppressWarnings(as.numeric(flat))

  if (any(is.na(numeric) & !is.na(flat))) {
    return(.tg_warn_unparsed(flat, name, "numeric"))
  }

  present <- !is.na(numeric)
  overflow <- present & abs(numeric) > .Machine$integer.max

  if (any(overflow)) {
    cli::cli_warn(
      c(
        "Column {.field {name}} holds values too large for an integer.",
        "i" = "Returning it as a double instead."
      ),
      class = "transferegovr_type_warning"
    )
    return(numeric)
  }

  as.integer(numeric)
}

# The shape is checked with an anchored regex and parsed with an explicit
# format. Neither alone is safe: `as.Date()` without a format errors on
# "2026-02-30", while `as.Date(x, format = "%Y-%m-%d")` ignores trailing text,
# so "2026-01-02T03:04:05" would silently become a date and lose its time.
.tg_as_date <- function(flat, name) {
  present <- !is.na(flat)
  shaped <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", flat[present])

  if (!all(shaped)) {
    return(.tg_warn_unparsed(flat, name, "date"))
  }

  parsed <- as.Date(flat, format = "%Y-%m-%d")

  if (any(is.na(parsed) & present)) {
    return(.tg_warn_unparsed(flat, name, "date"))
  }

  parsed
}

.tg_as_datetime <- function(flat, name) {
  pattern <- paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}[T ][0-9]{2}:[0-9]{2}:[0-9]{2}",
    "([.][0-9]+)?(Z|[+-][0-9]{2}:?[0-9]{2})?$"
  )

  present <- !is.na(flat)
  shaped <- grepl(pattern, flat[present])

  if (!all(shaped)) {
    return(.tg_warn_unparsed(flat, name, "timestamp"))
  }

  # The API declares these columns as "timestamp without time zone" and sends
  # them without an offset, but a value that carries one has to be converted
  # rather than read as if it were UTC.
  has_offset <- grepl("[+-][0-9]{2}:?[0-9]{2}$", flat)
  normalized <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", flat)
  normalized <- sub("Z$", "", normalized)
  normalized <- sub("T", " ", normalized, fixed = TRUE)

  parsed <- rep(as.POSIXct(NA_character_, tz = "UTC"), length(flat))
  parsed[!has_offset] <- as.POSIXct(
    normalized[!has_offset],
    format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"
  )
  parsed[has_offset] <- as.POSIXct(
    normalized[has_offset],
    format = "%Y-%m-%d %H:%M:%OS%z", tz = "UTC"
  )

  if (any(is.na(parsed) & present)) {
    return(.tg_warn_unparsed(flat, name, "timestamp"))
  }

  parsed
}

.tg_warn_unparsed <- function(flat, name, what) {
  cli::cli_warn(
    c(
      "Column {.field {name}} holds {what} values the package cannot parse.",
      "i" = "Returning it as character."
    ),
    class = "transferegovr_type_warning"
  )
  flat
}
