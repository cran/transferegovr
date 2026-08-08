test_that("a bare value means equals and a bare vector means is-one-of", {
  filters <- filter_string(aa_ano_plano_acao = 2024)
  expect_equal(filters[["aa_ano_plano_acao"]], "eq.2024")
  expect_equal(
    filter_string(uf = c("PE", "PB"))[["uf"]],
    'in.("PE","PB")'
  )
})

test_that("each operator serialises to its PostgREST form", {
  expect_equal(format(eq(1)), "eq.1")
  expect_equal(format(neq(1)), "neq.1")
  expect_equal(format(gt(1)), "gt.1")
  expect_equal(format(gte(1)), "gte.1")
  expect_equal(format(lt(1)), "lt.1")
  expect_equal(format(lte(1)), "lte.1")
  expect_equal(format(like("*a*")), "like.*a*")
  expect_equal(format(ilike("*a*")), "ilike.*a*")
  expect_equal(format(re_match("^a")), "match.^a")
  expect_equal(format(re_imatch("^a")), "imatch.^a")
  expect_equal(format(is_null()), "is.null")
  expect_equal(format(is_true()), "is.true")
  expect_equal(format(is_false()), "is.false")
  expect_equal(format(not(eq(1))), "not.eq.1")
  expect_equal(format(not(is_null())), "not.is.null")
})

test_that("every documented operator appears in tg_operators()", {
  exported <- getNamespaceExports("transferegovr")
  expect_true(all(tg_operators()$operator %in% exported))
})

test_that("dates and times are formatted the way the API reads them", {
  expect_equal(format(gte(as.Date("2024-03-01"))), "gte.2024-03-01")
  expect_equal(
    format(lt(as.POSIXct("2024-03-01 10:20:30", tz = "UTC"))),
    "lt.2024-03-01T10:20:30"
  )
})

test_that("logicals become true and false, not TRUE and FALSE", {
  expect_equal(format(eq(TRUE)), "eq.true")
  expect_equal(format(eq(FALSE)), "eq.false")
})

test_that("large and fractional numbers avoid scientific notation", {
  # "1e+05" would be compared as text against a numeric column and match
  # nothing, silently returning an empty result rather than an error.
  expect_equal(format(eq(100000)), "eq.100000")
  expect_equal(format(gte(1234567890123)), "gte.1234567890123")
  expect_equal(format(eq(0.5)), "eq.0.5")
})

test_that("values carrying PostgREST structure are quoted", {
  # An unquoted comma would be read as a separator and widen the match.
  expect_equal(format(eq("a,b")), 'eq."a,b"')
  expect_equal(format(eq("(x)")), 'eq."(x)"')
  expect_equal(format(eq(" pad ")), 'eq." pad "')
  expect_equal(format(eq("")), 'eq.""')
})

test_that("quotes and backslashes inside a value are escaped", {
  expect_equal(format(eq('say "hi"')), 'eq."say \\"hi\\""')
  expect_equal(format(eq("back\\slash")), 'eq."back\\\\slash"')
})

test_that("a value needing no quoting is left alone", {
  # Quoting a `like` pattern would change what it matches, not just how it
  # looks, so quoting must stay narrow.
  expect_equal(format(like("*saude*")), "like.*saude*")
  expect_equal(format(eq("2024-01-01")), "eq.2024-01-01")
  expect_equal(format(eq("a.b")), "eq.a.b")
})

test_that("every element of in_() is quoted", {
  expect_equal(format(in_(c("a", "b,c"))), 'in.("a","b,c")')
  expect_equal(format(in_(c(1, 2))), "in.(1,2)")
})

test_that("a list of operators becomes repeated parameters on one column", {
  filters <- .tg_eval_filters(rlang::quos(
    dt = list(gte("2024-01-01"), lt("2025-01-01"))
  ))

  expect_length(filters, 2L)
  expect_equal(names(filters), c("dt", "dt"))
  expect_equal(
    unlist(filters, use.names = FALSE),
    c("gte.2024-01-01", "lt.2025-01-01")
  )
})

test_that("operators win over a same-named function in the calling scope", {
  # A user with data.table or gt attached must still get these operators, so
  # filters are evaluated in a mask rather than the caller's environment.
  gte <- function(x) stop("the caller's gte() was used")
  expect_equal(filter_string(a = gte(1))[["a"]], "gte.1")
})

test_that("filters see variables from the calling environment", {
  year <- 2024
  expect_equal(filter_string(a = gte(year))[["a"]], "gte.2024")
})

test_that("a NULL filter is dropped rather than sent", {
  expect_length(.tg_eval_filters(rlang::quos(a = NULL)), 0L)
})

test_that("unnamed filters are rejected", {
  expect_error(
    .tg_eval_filters(rlang::quos(2024)),
    class = "transferegovr_filter_error"
  )
})

test_that("missing values are rejected with a pointer to is_null()", {
  expect_error(eq(NA), class = "transferegovr_filter_error")
  expect_error(in_(c(1, NA)), class = "transferegovr_filter_error")
  expect_error(
    .tg_eval_filters(rlang::quos(a = NA)),
    class = "transferegovr_filter_error"
  )
})

test_that("operators reject operands of the wrong shape", {
  expect_error(eq(c(1, 2)), class = "transferegovr_filter_error")
  expect_error(eq(list(1)), class = "transferegovr_filter_error")
  expect_error(like(1), class = "transferegovr_filter_error")
  expect_error(in_(character()), class = "transferegovr_filter_error")
  expect_error(in_(list(1, 2)), class = "transferegovr_filter_error")
})

test_that("not() needs a filter and refuses to double-negate", {
  expect_error(not(1), class = "transferegovr_filter_error")
  expect_error(not(not(eq(1))), class = "transferegovr_filter_error")
})

test_that("a column given neither a value nor an operator errors", {
  expect_error(
    .tg_eval_filters(rlang::quos(a = list(1, 2))),
    class = "transferegovr_filter_error"
  )
  expect_error(
    .tg_eval_filters(rlang::quos(a = list())),
    class = "transferegovr_filter_error"
  )
  expect_error(
    .tg_eval_filters(rlang::quos(a = character())),
    class = "transferegovr_filter_error"
  )
})

test_that("a filter prints as the string it will send", {
  expect_output(print(gte(2024)), "gte.2024")
})
