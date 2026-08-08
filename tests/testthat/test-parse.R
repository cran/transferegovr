fields <- function(...) {
  spec <- c(...)
  tibble::tibble(
    field = names(spec),
    r_type = unname(spec),
    pg_type = NA_character_,
    primary_key = FALSE,
    description = NA_character_
  )
}

# Literal JSON, parsed as the client parses it: an R list round-tripped through
# jsonlite cannot express a JSON `null`, which is what these APIs send for every
# empty column.
rows_from <- function(json) {
  skip_if_not_installed("jsonlite")
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

test_that("columns are typed from the schema, not guessed", {
  rows <- rows_from('[{"a":"1","b":1,"c":1,"d":true}]')
  out <- .tg_rows_to_tibble(
    rows,
    fields(a = "character", b = "integer", c = "double", d = "logical")
  )

  expect_type(out$a, "character")
  expect_type(out$b, "integer")
  expect_type(out$c, "double")
  expect_type(out$d, "logical")
})

test_that("a JSON null becomes NA of the column's own type", {
  rows <- rows_from('[{"a":null,"b":null,"c":null,"d":null}]')
  out <- .tg_rows_to_tibble(
    rows,
    fields(a = "character", b = "integer", c = "Date", d = "logical")
  )

  expect_identical(out$a, NA_character_)
  expect_identical(out$b, NA_integer_)
  expect_s3_class(out$c, "Date")
  expect_true(is.na(out$c))
  expect_identical(out$d, NA)
})

test_that("an all-null column keeps its declared class", {
  # Left to inference this column would come back logical on a page where every
  # value is null and character on the next, changing class between pages.
  rows <- rows_from('[{"a":null},{"a":null}]')
  out <- .tg_rows_to_tibble(rows, fields(a = "Date"))

  expect_s3_class(out$a, "Date")
  expect_length(out$a, 2L)
})

test_that("nulls mixed with values keep the column's type", {
  rows <- rows_from('[{"a":"2024-01-02"},{"a":null},{"a":"2024-03-04"}]')
  out <- .tg_rows_to_tibble(rows, fields(a = "Date"))

  expect_s3_class(out$a, "Date")
  expect_equal(out$a, as.Date(c("2024-01-02", NA, "2024-03-04")))
})

test_that("an impossible date leaves the column as character with a warning", {
  # `as.Date()` without a format errors outright on "2026-02-30", which would
  # abort the whole download over one bad value.
  rows <- rows_from('[{"a":"2026-02-30"}]')

  expect_warning(
    out <- .tg_rows_to_tibble(rows, fields(a = "Date")),
    class = "transferegovr_type_warning"
  )
  expect_type(out$a, "character")
  expect_equal(out$a, "2026-02-30")
})

test_that("a timestamp in a date column is not silently truncated", {
  # `as.Date(x, format = "%Y-%m-%d")` ignores trailing text, so this would
  # become 2024-01-02 and lose its time without the anchored regex.
  rows <- rows_from('[{"a":"2024-01-02T03:04:05"}]')

  expect_warning(
    out <- .tg_rows_to_tibble(rows, fields(a = "Date")),
    class = "transferegovr_type_warning"
  )
  expect_type(out$a, "character")
})

test_that("timestamps parse to UTC", {
  rows <- rows_from('[{"a":"2022-05-17T18:05:42"},{"a":"2022-05-17 18:05:42"}]')
  out <- .tg_rows_to_tibble(rows, fields(a = "POSIXct"))

  expect_s3_class(out$a, "POSIXct")
  expect_equal(format(out$a[[1]], tz = "UTC"), "2022-05-17 18:05:42")
  expect_equal(out$a[[1]], out$a[[2]])
})

test_that("a timestamp carrying an offset is converted, not read as UTC", {
  rows <- rows_from('[{"a":"2022-05-17T18:05:42-03:00"}]')
  out <- .tg_rows_to_tibble(rows, fields(a = "POSIXct"))

  expect_s3_class(out$a, "POSIXct")
  formatted <- format(out$a, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  expect_equal(formatted, "2022-05-17 21:05:42")
})

test_that("an unparseable timestamp warns instead of aborting", {
  rows <- rows_from('[{"a":"2022-05-17T18:05:42junk"}]')

  expect_warning(
    out <- .tg_rows_to_tibble(rows, fields(a = "POSIXct")),
    class = "transferegovr_type_warning"
  )
  expect_type(out$a, "character")
})

test_that("an integer column too wide for an integer falls back to double", {
  big <- .Machine$integer.max + 1
  rows <- rows_from(sprintf('[{"a":%.0f}]', big))

  expect_warning(
    out <- .tg_rows_to_tibble(rows, fields(a = "integer")),
    class = "transferegovr_type_warning"
  )
  expect_type(out$a, "double")
  expect_equal(out$a, big)
})

test_that("a column the schema does not know is typed by inspection", {
  rows <- rows_from('[{"novo":1},{"novo":2}]')
  out <- .tg_rows_to_tibble(rows, fields(a = "character"))

  expect_named(out, "novo")
  expect_type(out$novo, "double")
})

test_that("a column set by .select shapes the result", {
  rows <- rows_from('[{"a":1,"b":2}]')
  out <- .tg_rows_to_tibble(rows, fields(a = "integer", b = "integer"),
    columns = "a"
  )

  expect_named(out, "a")
})

test_that("an empty result still carries the table's columns and types", {
  # A zero-row tibble with no columns would break any code that binds pages or
  # selects a column, so an empty answer keeps the schema's shape.
  out <- .tg_rows_to_tibble(list(), fields(a = "Date", b = "integer"))

  expect_equal(nrow(out), 0L)
  expect_named(out, c("a", "b"))
  expect_s3_class(out$a, "Date")
  expect_type(out$b, "integer")
})

test_that("an empty result narrowed by .select keeps only those columns", {
  out <- .tg_rows_to_tibble(list(), fields(a = "Date", b = "integer"),
    columns = "b"
  )

  expect_named(out, "b")
  expect_type(out$b, "integer")
})

test_that("rows presenting columns in different orders are still aligned", {
  rows <- rows_from('[{"a":1,"b":2},{"b":20,"a":10}]')
  out <- .tg_rows_to_tibble(rows, fields(a = "integer", b = "integer"))

  expect_equal(out$a, c(1L, 10L))
  expect_equal(out$b, c(2L, 20L))
})

test_that("a nested value is kept whole in a list-column", {
  rows <- rows_from('[{"a":{"x":1}},{"a":null}]')
  out <- .tg_rows_to_tibble(rows, fields(a = "character"))

  expect_type(out$a, "list")
  expect_equal(out$a[[1]], list(x = 1))
})

test_that("Content-Range is read in every form the service sends", {
  expect_equal(.tg_parse_content_range("0-99/6176")$total, 6176)
  expect_equal(.tg_parse_content_range("0-99/6176")$first, 0L)
  expect_equal(.tg_parse_content_range("0-99/6176")$last, 99L)
  expect_true(is.na(.tg_parse_content_range("0-2/*")$total))
  expect_equal(.tg_parse_content_range("*/0")$total, 0)
  expect_true(is.na(.tg_parse_content_range("*/0")$first))
  expect_equal(.tg_parse_content_range("items 0-9/10")$total, 10)
})

test_that("a missing or malformed Content-Range reports no total", {
  expect_true(is.na(.tg_parse_content_range(NULL)$total))
  expect_true(is.na(.tg_parse_content_range("")$total))
  expect_true(is.na(.tg_parse_content_range("nonsense")$total))
  expect_true(is.na(.tg_parse_content_range("0-99")$total))
})

test_that("a total beyond integer range survives as a double", {
  # As an integer this would become NA and silently disable the completeness
  # check that bounds multi-page collection.
  expect_equal(.tg_parse_content_range("0-9/3000000000")$total, 3e9)
})
