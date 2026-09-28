test_that("the frozen schema covers four modules and seventy-four tables", {
  expect_equal(nrow(tg_modules()), 4L)
  expect_equal(nrow(tg_tables()), 74L)
  expect_equal(sum(tg_tables()$columns), 1045L)
  expect_equal(sum(tg_tables()$params), 1059L)
})

test_that("module names, aliases, case and punctuation all resolve", {
  expect_equal(.tg_match_module("parcerias"), "parcerias")
  expect_equal(.tg_match_module("PARCERIAS"), "parcerias")
  expect_equal(.tg_match_module("fundoafundo"), "fundoafundo")
  expect_equal(.tg_match_module("fundo_a_fundo"), "fundoafundo")
  expect_equal(.tg_match_module("fundo a fundo"), "fundoafundo")
  expect_equal(.tg_match_module("fund_to_fund"), "fundoafundo")
  expect_equal(.tg_match_module(" especiais "), "especiais")
  expect_equal(.tg_match_module("ted"), "ted")
  expect_equal(.tg_match_module("TED"), "ted")
  expect_equal(.tg_match_module("decentralized credit"), "ted")
})

test_that("the old module name still resolves to the module that replaced it", {
  # The package used to query `transferenciasespeciais` on the PostgREST
  # service; the same data is `especiais` here.
  expect_equal(.tg_match_module("transferenciasespeciais"), "especiais")
  expect_equal(.tg_match_module("transferencias_especiais"), "especiais")
})

test_that("an unknown module is rejected rather than looked up blindly", {
  # `[[` on a named character vector aborts on a missing name instead of
  # returning NULL, which turned every non-alias module name into a
  # "subscript out of bounds" error.
  schema_error <- "transferegovr_schema_error"
  expect_error(.tg_match_module("xpto"), class = schema_error)
  expect_error(
    .tg_match_module(c("parcerias", "parcerias")),
    class = schema_error
  )
  expect_error(.tg_match_module(NA_character_), class = schema_error)
  expect_error(.tg_match_module(1), class = schema_error)
})

test_that("a table missing from one module points at the module that has it", {
  # `programa` exists in parcerias and fundoafundo-adjacent naming with
  # different columns, so a miss is often the wrong module rather than a typo.
  expect_error(
    tg_fields("especiais", "programa"),
    "parcerias",
    class = "transferegovr_schema_error"
  )
})

test_that("an unknown table is rejected", {
  schema_error <- "transferegovr_schema_error"
  expect_error(tg_fields("parcerias", "nao_existe"), class = schema_error)
  expect_error(tg_fields("parcerias", NA_character_), class = schema_error)
})

test_that("a table may be named with a hyphen as the endpoint spells it", {
  expect_equal(
    tg_fields("parcerias", "meta-proposta"),
    tg_fields("parcerias", "meta_proposta")
  )
})

test_that("tables carrying the same name differ between modules", {
  parcerias <- tg_fields("parcerias", "programa")$field
  # `programas` in fundoafundo is the nearest equivalent and is not the same.
  fundo <- tg_fields("fundoafundo", "programas")$field

  expect_false(identical(parcerias, fundo))
})

test_that("column types cover every JSON type the APIs publish", {
  fields <- purrr::list_rbind(lapply(seq_len(nrow(tg_tables())), function(i) {
    row <- tg_tables()[i, ]
    tg_fields(row$module, row$table)
  }))

  expect_setequal(
    unique(fields$r_type),
    c("character", "double", "logical", "Date", "POSIXct", "list")
  )
  expect_false(anyNA(fields$r_type))
})

test_that("integers are typed as double, not integer", {
  # These documents declare no `format`, so int32 and int64 cannot be told
  # apart, and identifiers here genuinely exceed .Machine$integer.max.
  fields <- tg_fields("parcerias", "parceria")
  integers <- fields[!is.na(fields$api_type) & fields$api_type == "integer", ]

  expect_gt(nrow(integers), 0L)
  expect_true(all(integers$r_type == "double"))
})

test_that("every table records the endpoint path it maps to", {
  tables <- tg_tables()

  expect_false(anyNA(tables$path))
  # The two spellings differ wherever the endpoint uses a hyphen.
  expect_true(any(tables$path != tables$table))
  expect_true(all(gsub("-", "_", tables$path) == tables$table))
})

test_that("a nested column can be described", {
  fields <- tg_fields("parcerias", "programa")
  nested <- fields$field[!is.na(fields$nested)]

  expect_gt(length(nested), 0L)

  inner <- tg_fields("parcerias", "programa", nested = nested[[1]])
  expect_s3_class(inner, "tbl_df")
  expect_gt(nrow(inner), 0L)
})

test_that("asking for a nested column that is not one is an error", {
  expect_error(
    tg_fields("parcerias", "programa", nested = "id_programa"),
    class = "transferegovr_schema_error"
  )
  expect_error(
    tg_fields("parcerias", "parceria", nested = "nao_existe"),
    class = "transferegovr_schema_error"
  )
})

test_that("tg_tables() lists one module or all of them", {
  expect_setequal(unique(tg_tables("parcerias")$module), "parcerias")
  expect_equal(nrow(tg_tables("parcerias")), 17L)
  expect_setequal(unique(tg_tables()$module), tg_modules()$module)
})

test_that("tg_modules() reports each module's own base URL", {
  modules <- tg_modules()

  expect_true(all(grepl("^https://api-publica\\.", modules$url)))
  expect_equal(sum(modules$tables), 74L)
})

test_that("each module reports the page limit its service declares", {
  # Not one number: especiais and parcerias answer 422 above 200 rows a page,
  # fundoafundo and ted above 1000.
  limits <- stats::setNames(tg_modules()$max_page_size, tg_modules()$module)

  expect_equal(
    limits[c("especiais", "fundoafundo", "parcerias", "ted")],
    c(especiais = 200L, fundoafundo = 1000L, parcerias = 200L, ted = 1000L)
  )
})

test_that("Portuguese aliases are the same functions", {
  expect_identical(tg_modulos, tg_modules)
  expect_identical(tg_metadados, tg_metadata)
  expect_identical(tg_contar, tg_count)
  expect_identical(tg_obter, tg_get)
  expect_identical(tg_atualizado_em, tg_updated_at)
  expect_equal(tg_tabelas("parcerias"), tg_tables("parcerias"))
  expect_equal(
    tg_campos("parcerias", "parceria"),
    tg_fields("parcerias", "parceria")
  )
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
    rep(list(mock_envelope("[]", total = 42)), 17)
  )

  result <- tg_tables("parcerias", counts = TRUE)

  expect_length(recorded$requests, 17L)
  expect_true("rows" %in% names(result))
  expect_equal(result$rows, rep(42, 17))
})

test_that("counts is validated before any request", {
  expect_error(tg_tables("parcerias", counts = "yes"))
  expect_error(tg_tables("parcerias", counts = NA))
  expect_error(tg_tables("parcerias", counts = c(TRUE, TRUE)))
})

test_that("the Portuguese alias takes the counts argument too", {
  withr::local_options(transferegovr.cache = FALSE)
  recorded <- local_recorded_requests(
    rep(list(mock_envelope("[]", total = 7)), 17)
  )

  expect_equal(tg_tabelas("parcerias", contagens = TRUE)$rows, rep(7, 17))
  expect_length(recorded$requests, 17L)
})

test_that("a repeated count is served from the cache", {
  # 74 counts is 74 requests the first time and none the next, which is what
  # makes `tg_tables(counts = TRUE)` usable more than once in a session.
  dir <- withr::local_tempdir()
  withr::local_options(
    transferegovr.cache = TRUE,
    transferegovr.cache_dir = dir
  )
  recorded <- local_recorded_requests(
    rep(list(mock_envelope("[]", total = 9)), 17)
  )

  first <- tg_tables("parcerias", counts = TRUE)
  second <- tg_tables("parcerias", counts = TRUE)

  expect_length(recorded$requests, 17L)
  expect_equal(first$rows, second$rows)
})
