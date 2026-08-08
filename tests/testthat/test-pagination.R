test_that("a request under the page size takes exactly one round trip", {
  recorded <- local_recorded_requests(mock_page(5, total = 5))

  result <- tg_get("ted", "plano_acao", .limit = 5, .cache = FALSE)

  expect_length(recorded$requests, 1L)
  expect_equal(nrow(result), 5L)
  expect_equal(tg_metadata(result)$pages, 1L)
})

test_that("rows beyond the page size are collected across pages", {
  recorded <- local_recorded_requests(list(
    mock_page(2, from = 1, total = 5),
    mock_page(2, from = 3, total = 5, first = 2),
    mock_page(1, from = 5, total = 5, first = 4)
  ))

  result <- tg_get("ted", "plano_acao",
    .limit = 5, .page_size = 2, .cache = FALSE,
    .progress = FALSE
  )

  expect_length(recorded$requests, 3L)
  expect_equal(nrow(result), 5L)
  expect_equal(result$id_plano_acao, 1:5)
  expect_equal(tg_metadata(result)$pages, 3L)
})

test_that("each page asks for the offset that follows the last one", {
  recorded <- local_recorded_requests(list(
    mock_page(2, from = 1, total = 5),
    mock_page(2, from = 3, total = 5, first = 2),
    mock_page(1, from = 5, total = 5, first = 4)
  ))

  tg_get("ted", "plano_acao",
    .limit = 5, .page_size = 2, .cache = FALSE,
    .progress = FALSE
  )

  offsets <- vapply(
    recorded$requests,
    function(r) request_query(r)[["offset"]],
    character(1)
  )
  expect_equal(offsets, c("0", "2", "4"))
})

test_that("the last page asks only for the rows still missing", {
  # Asking for a full page and discarding the surplus would make the service do
  # work the caller never sees.
  recorded <- local_recorded_requests(list(
    mock_page(2, from = 1, total = 5),
    mock_page(1, from = 3, total = 5, first = 2)
  ))

  tg_get("ted", "plano_acao",
    .limit = 3, .page_size = 2, .cache = FALSE,
    .progress = FALSE
  )

  limits <- vapply(
    recorded$requests,
    function(r) request_query(r)[["limit"]],
    character(1)
  )
  expect_equal(limits, c("2", "1"))
})

test_that(".limit = Inf collects every row the API reports", {
  recorded <- local_recorded_requests(list(
    mock_page(2, from = 1, total = 3),
    mock_page(1, from = 3, total = 3, first = 2)
  ))

  result <- tg_get("ted", "plano_acao",
    .limit = Inf, .page_size = 2,
    .cache = FALSE, .progress = FALSE
  )

  expect_equal(nrow(result), 3L)
  expect_equal(tg_metadata(result)$total_rows, 3)
})

test_that("collection stops at the total even when the limit is higher", {
  recorded <- local_recorded_requests(mock_page(3, total = 3))

  result <- tg_get("ted", "plano_acao",
    .limit = 100, .page_size = 10,
    .cache = FALSE, .progress = FALSE
  )

  expect_length(recorded$requests, 1L)
  expect_equal(nrow(result), 3L)
})

test_that("the offset is subtracted from what remains to collect", {
  # Without this the loop would try to collect `total` rows starting from the
  # offset and run past the end of the table.
  recorded <- local_recorded_requests(
    mock_page(2, from = 9, total = 10, first = 8)
  )

  result <- tg_get("ted", "plano_acao",
    .limit = Inf, .offset = 8,
    .page_size = 5, .cache = FALSE, .progress = FALSE
  )

  expect_length(recorded$requests, 1L)
  expect_equal(nrow(result), 2L)
  expect_equal(tg_metadata(result)$offset, 8)
})

test_that("an offset past the end returns no rows without looping", {
  local_recorded_requests(mock_json_response("[]", content_range = "*/5"))

  result <- tg_get("ted", "plano_acao",
    .limit = Inf, .offset = 99,
    .cache = FALSE, .progress = FALSE
  )

  expect_equal(nrow(result), 0L)
})

test_that("a page that comes back empty stops collection instead of looping", {
  # The loop is bounded by the total the API reported. If the table shrinks
  # mid-collection that total is stale, and without this break the loop would
  # keep asking for rows that no longer exist.
  recorded <- local_recorded_requests(list(
    mock_page(2, from = 1, total = 10),
    mock_json_response("[]", content_range = "2-1/10")
  ))

  expect_warning(
    result <- tg_get("ted", "plano_acao",
      .limit = Inf, .page_size = 2,
      .cache = FALSE, .progress = FALSE
    ),
    class = "transferegovr_incomplete_warning"
  )
  expect_length(recorded$requests, 2L)
  expect_equal(nrow(result), 2L)
})

test_that("collecting fewer rows than reported is reported as a warning", {
  local_recorded_requests(list(
    mock_page(2, from = 1, total = 10),
    mock_page(1, from = 3, total = 10, first = 2),
    mock_json_response("[]", content_range = "3-2/10")
  ))

  expect_warning(
    tg_get("ted", "plano_acao",
      .limit = Inf, .page_size = 2, .cache = FALSE,
      .progress = FALSE
    ),
    "Collected 3 rows where the API reported 10",
    class = "transferegovr_incomplete_warning"
  )
})

test_that("a complete collection warns about nothing", {
  local_recorded_requests(list(
    mock_page(2, from = 1, total = 3),
    mock_page(1, from = 3, total = 3, first = 2)
  ))

  expect_no_warning(
    tg_get("ted", "plano_acao",
      .limit = Inf, .page_size = 2, .cache = FALSE,
      .progress = FALSE
    )
  )
})

test_that("a missing total leaves the limit as the only bound", {
  local_recorded_requests(list(
    mock_page(2, from = 1),
    mock_page(2, from = 3, first = 2)
  ))

  result <- tg_get("ted", "plano_acao",
    .limit = 4, .page_size = 2,
    .cache = FALSE, .progress = FALSE
  )

  expect_equal(nrow(result), 4L)
  expect_true(is.na(tg_metadata(result)$total_rows))
})

test_that("metadata reports what was actually retrieved", {
  local_recorded_requests(list(
    mock_page(2, from = 1, total = 5),
    mock_page(2, from = 3, total = 5, first = 2)
  ))

  result <- tg_get("ted", "plano_acao",
    .limit = 4, .page_size = 2,
    .select = c("id_plano_acao"), .cache = FALSE, .progress = FALSE
  )
  meta <- tg_metadata(result)

  expect_equal(meta$module, "ted")
  expect_equal(meta$table, "plano_acao")
  expect_equal(meta$total_rows, 5)
  expect_equal(meta$rows_returned, 4L)
  expect_equal(meta$pages, 2L)
  expect_equal(meta$page_size, 2L)
  expect_equal(meta$select, "id_plano_acao")
  expect_false(meta$cached)
  expect_s3_class(meta$retrieved_at, "POSIXct")
})

test_that("tg_metadata() is NULL for anything else", {
  expect_null(tg_metadata(tibble::tibble()))
  expect_null(tg_metadata(1))
})

test_that("tg_count() reads the total without fetching rows", {
  recorded <- local_recorded_requests(
    mock_json_response("[]", content_range = "*/6176")
  )

  expect_equal(tg_count("ted", "plano_acao", .cache = FALSE), 6176)

  query <- request_query(recorded$requests[[1]])
  expect_equal(query[["select"]], "")
  expect_equal(query[["limit"]], "1")
  expect_equal(recorded$requests[[1]]$headers$Prefer, "count=exact")
})

test_that("tg_count() applies filters", {
  recorded <- local_recorded_requests(
    mock_json_response("[]", content_range = "*/12")
  )

  tg_count("ted", "plano_acao", aa_ano_plano_acao = 2024, .cache = FALSE)

  query <- request_query(recorded$requests[[1]])
  expect_equal(query[["aa_ano_plano_acao"]], "eq.2024")
})

test_that("tg_count() aborts when the API reports no total", {
  # Returning 0 here would read as "nothing matches" when the truth is
  # "the service did not say".
  local_recorded_requests(mock_json_response("[]", content_range = "0-0/*"))

  expect_error(
    tg_count("ted", "plano_acao", .cache = FALSE),
    class = "transferegovr_response_error"
  )
})

test_that("pagination arguments are validated before any request", {
  expect_error(tg_get("ted", "plano_acao", .limit = 0, .cache = FALSE))
  expect_error(tg_get("ted", "plano_acao", .limit = -1, .cache = FALSE))
  expect_error(tg_get("ted", "plano_acao", .limit = 1.5, .cache = FALSE))
  expect_error(tg_get("ted", "plano_acao", .limit = NA, .cache = FALSE))
  expect_error(tg_get("ted", "plano_acao", .limit = "5", .cache = FALSE))
  expect_error(tg_get("ted", "plano_acao", .limit = -Inf, .cache = FALSE))
  expect_error(tg_get("ted", "plano_acao", .offset = -1, .cache = FALSE))
  expect_error(tg_get("ted", "plano_acao", .offset = Inf, .cache = FALSE))
  expect_error(tg_get("ted", "plano_acao", .page_size = 0, .cache = FALSE))
  # The service caps a page at 1000 rows however many are asked for, so a
  # larger page size would silently return fewer rows than requested.
  expect_error(tg_get("ted", "plano_acao", .page_size = 1001, .cache = FALSE))
})

test_that(".offset = 0 is accepted while .limit = 0 is not", {
  local_recorded_requests(mock_page(1, total = 1))
  expect_no_error(
    tg_get("ted", "plano_acao", .limit = 1, .offset = 0, .cache = FALSE)
  )
})

test_that("selected and ordering columns are checked against the schema", {
  expect_error(
    tg_get("ted", "plano_acao", .select = "nao_existe", .cache = FALSE),
    class = "transferegovr_schema_error"
  )
  expect_error(
    tg_get("ted", "plano_acao", .order = "nao_existe.desc", .cache = FALSE),
    class = "transferegovr_schema_error"
  )
  expect_error(
    tg_get("ted", "plano_acao", nao_existe = 1, .cache = FALSE),
    class = "transferegovr_schema_error"
  )
})

test_that("order direction and null placement are stripped before checking", {
  local_recorded_requests(mock_page(1, total = 1))

  expect_no_error(
    tg_get("ted", "plano_acao",
      .limit = 1, .cache = FALSE,
      .order = c(
        "id_plano_acao.desc.nullslast", "id_programa.asc", "aa_instrumento"
      )
    )
  )
})

test_that("validation can be turned off for a column added upstream", {
  withr::local_options(transferegovr.validate = FALSE)
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  expect_no_error(
    tg_get("ted", "plano_acao", coluna_nova = 1, .limit = 1, .cache = FALSE)
  )
  expect_equal(request_query(recorded$requests[[1]])[["coluna_nova"]], "eq.1")
})

test_that("a select carrying PostgREST syntax is passed through unchecked", {
  local_recorded_requests(mock_page(1, total = 1))

  expect_no_error(
    tg_get("ted", "plano_acao",
      .select = "apelido:id_plano_acao", .limit = 1,
      .cache = FALSE
    )
  )
})

test_that("module shortcuts are the module they name", {
  recorded <- local_recorded_requests(list(
    mock_page(1, total = 1), mock_page(1, total = 1), mock_page(1, total = 1)
  ))

  tg_ted("plano_acao", .limit = 1, .cache = FALSE)
  tg_fundo_a_fundo("programa", .limit = 1, .cache = FALSE)
  tg_transferencias_especiais("programa_especial", .limit = 1, .cache = FALSE)

  paths <- vapply(
    recorded$requests,
    function(r) httr2::url_parse(r$url)$path,
    character(1)
  )
  expect_equal(paths, c(
    "/ted/plano_acao",
    "/fundoafundo/programa",
    "/transferenciasespeciais/programa_especial"
  ))
})
