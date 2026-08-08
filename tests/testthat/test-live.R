# Integration tests against the real APIs. Skipped unless
# TRANSFEREGOVR_LIVE_TESTS is set, so neither CRAN nor a routine
# `devtools::test()` reaches the government's servers.

skip_unless_live <- function() {
  testthat::skip_if_offline()
  testthat::skip_if_not(
    nzchar(Sys.getenv("TRANSFEREGOVR_LIVE_TESTS")),
    "set TRANSFEREGOVR_LIVE_TESTS to run live tests"
  )
}

test_that("every module answers and its schema still matches", {
  skip_unless_live()

  for (module in tg_modules()$module) {
    tables <- tg_tables(module)

    result <- tg_get(module, tables$table[[1]], .limit = 1, .cache = FALSE)
    expect_s3_class(result, "tbl_df")
  }
})

test_that("every table still exists and still has the columns we froze", {
  skip_unless_live()

  tables <- tg_tables()

  for (i in seq_len(nrow(tables))) {
    module <- tables$module[[i]]
    table <- tables$table[[i]]

    result <- tg_get(module, table, .limit = 1, .cache = FALSE)
    known <- tg_fields(module, table)$field

    expect_true(
      all(names(result) %in% known),
      label = paste0(module, "/", table, " has unknown columns")
    )
  }
})

test_that("the row count matches what a full fetch returns", {
  skip_unless_live()

  total <- tg_count(
    "transferenciasespeciais", "programa_especial",
    .cache = FALSE
  )
  rows <- tg_get("transferenciasespeciais", "programa_especial",
    .limit = Inf, .cache = FALSE, .progress = FALSE
  )

  expect_equal(nrow(rows), total)
})

test_that("paging is stable: the same rows come back whatever the page size", {
  skip_unless_live()

  key <- function(x) {
    flat <- as.data.frame(lapply(x, as.character))
    sort(apply(flat, 1, paste, collapse = "|"))
  }

  one <- tg_get("fundoafundo", "programa",
    .limit = 1000, .cache = FALSE,
    .progress = FALSE
  )
  many <- tg_get("fundoafundo", "programa",
    .limit = Inf, .page_size = 25,
    .cache = FALSE, .progress = FALSE
  )

  expect_gt(tg_metadata(many)$pages, 1L)
  expect_equal(key(one), key(many))
})

test_that("the service still caps a page at 1000 rows", {
  # `.page_size` is capped at 1000 because the service silently truncates
  # anything larger. If that ever changes, the cap should be revisited.
  skip_unless_live()

  result <- tg_get("ted", "plano_acao_etapa",
    .select = "id_etapa",
    .limit = 1000, .page_size = 1000, .cache = FALSE
  )

  expect_equal(nrow(result), 1000L)
})

test_that("filters reach the service and narrow the result", {
  skip_unless_live()

  all_years <- tg_count("ted", "plano_acao", .cache = FALSE)
  one_year <- tg_count("ted", "plano_acao",
    aa_ano_plano_acao = 2024,
    .cache = FALSE
  )

  expect_lt(one_year, all_years)

  rows <- tg_get("ted", "plano_acao",
    aa_ano_plano_acao = 2024, .limit = 50,
    .cache = FALSE
  )
  expect_true(all(rows$aa_ano_plano_acao == 2024))
})

test_that("date and timestamp columns come back typed", {
  skip_unless_live()

  plans <- tg_get("ted", "plano_acao", .limit = 20, .cache = FALSE)
  expect_s3_class(plans$dt_inicio_vigencia, "Date")

  financial <- tg_get("ted", "programacao_financeira",
    .limit = 20,
    .cache = FALSE
  )
  expect_s3_class(financial$dh_recebimento_programacao, "POSIXct")
})

test_that("an unknown column is rejected by the service with its own message", {
  skip_unless_live()
  withr::local_options(transferegovr.validate = FALSE)

  expect_error(
    tg_get("ted", "plano_acao", coluna_inexistente = 1, .cache = FALSE),
    class = "transferegovr_http_400"
  )
})
