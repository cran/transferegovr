test_that("the frozen schema covers all three modules and forty-eight tables", {
  expect_equal(nrow(tg_modules()), 3L)
  expect_equal(nrow(tg_tables()), 48L)
  expect_equal(sum(tg_tables()$columns), 599L)
})

test_that("module names, aliases, case and punctuation all resolve", {
  expect_equal(.tg_match_module("ted"), "ted")
  expect_equal(.tg_match_module("TED"), "ted")
  expect_equal(.tg_match_module("fundoafundo"), "fundoafundo")
  expect_equal(.tg_match_module("fundo_a_fundo"), "fundoafundo")
  expect_equal(.tg_match_module("fundo a fundo"), "fundoafundo")
  expect_equal(.tg_match_module("fund_to_fund"), "fundoafundo")
  especiais <- "transferenciasespeciais"
  expect_equal(.tg_match_module("transferenciasespeciais"), especiais)
  expect_equal(.tg_match_module("transferencias_especiais"), especiais)
  expect_equal(.tg_match_module(" especiais "), "transferenciasespeciais")
})

test_that("an unknown module is rejected rather than looked up blindly", {
  # `[[` on a named character vector aborts on a missing name instead of
  # returning NULL, which turned every non-alias module name into an
  # "subscript out of bounds" error.
  expect_error(.tg_match_module("xpto"), class = "transferegovr_schema_error")
  schema_error <- "transferegovr_schema_error"
  expect_error(.tg_match_module(c("ted", "ted")), class = schema_error)
  expect_error(.tg_match_module(NA_character_), class = schema_error)
  expect_error(.tg_match_module(1), class = "transferegovr_schema_error")
})

test_that("a table missing from one module points at the module that has it", {
  # `plano_acao` exists in fundoafundo and ted with different columns, so a
  # miss is usually the wrong module rather than a typo.
  expect_error(
    tg_fields("transferenciasespeciais", "plano_acao"),
    "fundoafundo",
    class = "transferegovr_schema_error"
  )
})

test_that("an unknown table is rejected", {
  schema_error <- "transferegovr_schema_error"
  expect_error(tg_fields("ted", "nao_existe"), class = schema_error)
  expect_error(tg_fields("ted", NA_character_), class = schema_error)
})

test_that("tables carrying the same name differ between modules", {
  ted <- tg_fields("ted", "plano_acao")$field
  fundo <- tg_fields("fundoafundo", "plano_acao")$field

  expect_false(identical(ted, fundo))
})

test_that("column types cover every Postgres type the APIs publish", {
  fields <- purrr::list_rbind(lapply(seq_len(nrow(tg_tables())), function(i) {
    row <- tg_tables()[i, ]
    tg_fields(row$module, row$table)
  }))

  expect_setequal(
    unique(fields$r_type),
    c("character", "integer", "double", "logical", "Date", "POSIXct")
  )
  expect_false(anyNA(fields$r_type))
})

test_that("bigint is typed as double, not integer", {
  # A bigint beyond .Machine$integer.max would become NA as an integer, and a
  # column must not change class between pages because one page happened to fit.
  fields <- tg_fields("ted", "plano_acao")
  bigint <- fields[!is.na(fields$pg_type) & fields$pg_type == "bigint", ]

  expect_gt(nrow(bigint), 0L)
  expect_true(all(bigint$r_type == "double"))
})

test_that("the default order names real columns of the table", {
  tables <- tg_tables()

  for (i in seq_len(nrow(tables))) {
    module <- tables$module[[i]]
    table <- tables$table[[i]]
    order <- .tg_default_order(module, table)

    expect_gt(length(order), 0L)
    expect_true(all(grepl("\\.asc$", order)))
    columns <- sub("\\.asc$", "", order)
    expect_true(all(columns %in% tg_fields(module, table)$field))
  }
})

test_that("the default order prefers a declared primary key", {
  expect_equal(
    .tg_default_order("fundoafundo", "plano_acao"),
    "id_plano_acao.asc"
  )
  # ted/plano_acao declares no primary key, so identifier columns are used.
  expect_true(length(.tg_default_order("ted", "plano_acao")) > 1L)
})

test_that("tg_tables() lists one module or all of them", {
  expect_setequal(unique(tg_tables("ted")$module), "ted")
  expect_equal(nrow(tg_tables("ted")), 13L)
  expect_setequal(unique(tg_tables()$module), tg_modules()$module)
})

test_that("Portuguese aliases are the same functions", {
  expect_identical(tg_modulos, tg_modules)
  expect_identical(tg_metadados, tg_metadata)
  expect_identical(tg_contar, tg_count)
  expect_identical(tg_obter, tg_get)
  expect_equal(tg_tabelas("ted"), tg_tables("ted"))
  expect_equal(tg_campos("ted", "plano_acao"), tg_fields("ted", "plano_acao"))
})

test_that("the schema records when it was built", {
  expect_s3_class(tg_schema_date(), "Date")
  expect_false(is.na(tg_schema_date()))
})

test_that("counts = FALSE makes no request and matches the plain call", {
  # The schema half of `tg_tables()` must stay usable offline; only `counts`
  # touches the network.
  expect_equal(tg_tables(), tg_tables(counts = FALSE))
  expect_false("rows" %in% names(tg_tables()))
})

test_that("counts = TRUE adds one row count per table", {
  # Without this the previous test's cached counts are served and no request
  # is made at all -- which is correct behaviour, and useless for this test.
  withr::local_options(transferegovr.cache = FALSE)
  recorded <- local_recorded_requests(
    rep(list(mock_json_response("[]", content_range = "*/42")), 13)
  )

  result <- tg_tables("ted", counts = TRUE)

  expect_length(recorded$requests, 13L)
  expect_true("rows" %in% names(result))
  expect_equal(result$rows, rep(42, 13))
})

test_that("counts is validated before any request", {
  expect_error(tg_tables("ted", counts = "yes"))
  expect_error(tg_tables("ted", counts = NA))
  expect_error(tg_tables("ted", counts = c(TRUE, TRUE)))
})

test_that("the Portuguese alias takes the counts argument too", {
  withr::local_options(transferegovr.cache = FALSE)
  recorded <- local_recorded_requests(
    rep(list(mock_json_response("[]", content_range = "*/7")), 13)
  )

  expect_equal(tg_tabelas("ted", contagens = TRUE)$rows, rep(7, 13))
  expect_length(recorded$requests, 13L)
})

test_that("a repeated count is served from the cache", {
  # 48 counts is 48 requests the first time and none the next, which is what
  # makes `tg_tables(counts = TRUE)` usable more than once in a session.
  dir <- withr::local_tempdir()
  withr::local_options(
    transferegovr.cache = TRUE,
    transferegovr.cache_dir = dir
  )
  recorded <- local_recorded_requests(
    rep(list(mock_json_response("[]", content_range = "*/9")), 13)
  )

  first <- tg_tables("ted", counts = TRUE)
  second <- tg_tables("ted", counts = TRUE)

  expect_length(recorded$requests, 13L)
  expect_equal(first$rows, second$rows)
})
