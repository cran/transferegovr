# Pagination is by page number. The properties that matter are that the page
# sequence is right, that `.limit` and `.offset` keep meaning rows rather than
# pages, and that a short read is reported instead of passing as complete.

test_that("a single page is one request", {
  recorded <- local_recorded_requests(mock_page(3, total = 3))

  result <- tg_get("parcerias", "parceria", .limit = 10, .progress = FALSE)

  expect_equal(nrow(result), 3L)
  expect_length(recorded$requests, 1L)
  expect_equal(tg_metadata(result)$pages, 1L)
})

test_that("the endpoint path is the one the schema records", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("fundoafundo", "planos_acao", .limit = 1, .progress = FALSE)

  # The table is `planos_acao` in R and `planos-acao` in the URL.
  expect_equal(request_path(recorded$requests[[1]]), "fundoafundo/planos-acao")
})

test_that("the first request asks for page one at the requested size", {
  recorded <- local_recorded_requests(mock_page(2, total = 2))

  tg_get("parcerias", "parceria", .limit = 2, .page_size = 50,
         .progress = FALSE)

  query <- request_query(recorded$requests[[1]])
  expect_equal(query[["pagina"]], "1")
  expect_equal(query[["tamanho_da_pagina"]], "50")
})

test_that("a limit under the page size still asks for a whole page", {
  # There is no way to ask for three rows: the page size is the only lever, and
  # shrinking it to the limit would change what the page numbers mean.
  recorded <- local_recorded_requests(mock_page(200, total = 1000))

  result <- tg_get("parcerias", "parceria", .limit = 3, .page_size = 200,
                   .progress = FALSE)

  expect_equal(nrow(result), 3L)
  expect_equal(request_query(recorded$requests[[1]])[["tamanho_da_pagina"]],
               "200")
})

test_that("successive pages are requested by number", {
  recorded <- local_recorded_requests(list(
    mock_page(2, from = 1, total = 6, page = 1, page_size = 2),
    mock_page(2, from = 3, total = 6, page = 2, page_size = 2),
    mock_page(2, from = 5, total = 6, page = 3, page_size = 2)
  ))

  result <- tg_get("parcerias", "parceria", .limit = 6, .page_size = 2,
                   .progress = FALSE)

  expect_equal(nrow(result), 6L)
  expect_equal(result$id_parceria, 1:6)
  expect_equal(
    vapply(recorded$requests, function(r) request_query(r)[["pagina"]],
           character(1)),
    c("1", "2", "3")
  )
})

test_that("collection stops at the limit rather than at the total", {
  recorded <- local_recorded_requests(list(
    mock_page(2, from = 1, total = 100, page = 1, page_size = 2),
    mock_page(2, from = 3, total = 100, page = 2, page_size = 2)
  ))

  result <- tg_get("parcerias", "parceria", .limit = 4, .page_size = 2,
                   .progress = FALSE)

  expect_equal(nrow(result), 4L)
  expect_length(recorded$requests, 2L)
})

test_that("a page carrying more rows than were wanted is trimmed", {
  local_recorded_requests(list(
    mock_page(2, from = 1, total = 100, page = 1, page_size = 2),
    mock_page(2, from = 3, total = 100, page = 2, page_size = 2)
  ))

  result <- tg_get("parcerias", "parceria", .limit = 3, .page_size = 2,
                   .progress = FALSE)

  expect_equal(nrow(result), 3L)
  expect_equal(result$id_parceria, 1:3)
})

test_that("Inf collects every matching row", {
  local_recorded_requests(list(
    mock_page(2, from = 1, total = 5, page = 1, page_size = 2),
    mock_page(2, from = 3, total = 5, page = 2, page_size = 2),
    mock_page(1, from = 5, total = 5, page = 3, page_size = 2)
  ))

  result <- tg_get("parcerias", "parceria", .limit = Inf, .page_size = 2,
                   .progress = FALSE)

  expect_equal(nrow(result), 5L)
  expect_equal(result$id_parceria, 1:5)
})

# Offset ----------------------------------------------------------------------

test_that("an offset on a page boundary starts at that page", {
  recorded <- local_recorded_requests(
    mock_page(2, from = 5, total = 10, page = 3, page_size = 2)
  )

  result <- tg_get("parcerias", "parceria", .limit = 2, .offset = 4,
                   .page_size = 2, .progress = FALSE)

  expect_equal(request_query(recorded$requests[[1]])[["pagina"]], "3")
  expect_equal(result$id_parceria, 5:6)
})

test_that("an offset inside a page drops the rows before it", {
  # 5 rows to skip at a page size of 4 means fetching page 2 and dropping its
  # first row, so `.offset` keeps meaning rows whatever `.page_size` is.
  recorded <- local_recorded_requests(list(
    mock_page(4, from = 5, total = 12, page = 2, page_size = 4),
    mock_page(4, from = 9, total = 12, page = 3, page_size = 4)
  ))

  result <- tg_get("parcerias", "parceria", .limit = 4, .offset = 5,
                   .page_size = 4, .progress = FALSE)

  expect_equal(request_query(recorded$requests[[1]])[["pagina"]], "2")
  expect_equal(result$id_parceria, 6:9)
})

test_that("an offset past the end returns nothing without erroring", {
  local_recorded_requests(mock_envelope("[]", total = 10, page = 99))

  expect_equal(
    nrow(tg_get("parcerias", "parceria", .offset = 500, .progress = FALSE)),
    0L
  )
})

test_that("the rows left after the offset bound the collection", {
  local_recorded_requests(
    mock_page(2, from = 9, total = 10, page = 5, page_size = 2)
  )

  result <- tg_get("parcerias", "parceria", .limit = Inf, .offset = 8,
                   .page_size = 2, .progress = FALSE)

  expect_equal(nrow(result), 2L)
})

# Short reads -----------------------------------------------------------------

test_that("a collection short of the reported total warns", {
  local_recorded_requests(list(
    mock_page(2, from = 1, total = 6, page = 1, page_size = 2),
    mock_envelope("[]", total = 6, page = 2, page_size = 2)
  ))

  expect_warning(
    tg_get("parcerias", "parceria", .limit = 6, .page_size = 2,
           .progress = FALSE),
    class = "transferegovr_incomplete_warning"
  )
})

test_that("an empty first page is not a short read", {
  local_recorded_requests(mock_envelope("[]", total = 0))

  expect_no_warning(
    result <- tg_get("parcerias", "parceria", .progress = FALSE)
  )
  expect_equal(nrow(result), 0L)
})

# Envelope --------------------------------------------------------------------

test_that("a response without the envelope names every field it lacks", {
  # A bare array is what the older PostgREST service answered with, so this is
  # the shape a misconfigured base URL produces. The message is asserted, not
  # just the class: the per-field checks downstream would also raise, but they
  # would blame one field instead of reporting that the shape is wrong.
  local_recorded_requests(mock_json_response("[]"))

  expect_error(
    tg_get("parcerias", "parceria", .progress = FALSE),
    "carried no.*total_pages",
    class = "transferegovr_response_error"
  )
})

test_that("an envelope whose data is not an array is an error", {
  local_recorded_requests(mock_envelope("{}", total = 1))

  expect_error(
    tg_get("parcerias", "parceria", .progress = FALSE),
    class = "transferegovr_response_error"
  )
})

test_that("an envelope missing a pagination field is an error", {
  # `data` is a well-formed array here, so this reaches the envelope check and
  # nothing else. Without it the absent `total_items` would only be noticed
  # once collection tried to bound itself.
  local_recorded_requests(mock_json_response(
    '{"data":[],"total_pages":1,"page_number":1,"page_size":200}'
  ))

  expect_error(
    tg_get("parcerias", "parceria", .progress = FALSE),
    "total_items",
    class = "transferegovr_response_error"
  )
})

test_that("an envelope without a usable total is an error", {
  local_recorded_requests(mock_json_response(
    paste0(
      '{"data":[],"total_pages":1,"total_items":null,',
      '"page_number":1,"page_size":200}'
    )
  ))

  expect_error(
    tg_get("parcerias", "parceria", .progress = FALSE),
    class = "transferegovr_response_error"
  )
})

# Metadata --------------------------------------------------------------------

test_that("metadata reports what was collected and how", {
  local_recorded_requests(list(
    mock_page(2, from = 1, total = 9, page = 1, page_size = 2),
    mock_page(2, from = 3, total = 9, page = 2, page_size = 2)
  ))

  result <- tg_get("parcerias", "parceria", in_situacao_parceria = "Aprovada",
                   .limit = 4, .page_size = 2, .progress = FALSE)
  meta <- tg_metadata(result)

  expect_equal(meta$module, "parcerias")
  expect_equal(meta$table, "parceria")
  expect_equal(meta$total_rows, 9)
  expect_equal(meta$rows_returned, 4L)
  expect_equal(meta$pages, 2L)
  expect_equal(meta$page_size, 2L)
  expect_equal(meta$filters, list(in_situacao_parceria = "Aprovada"))
  expect_false(meta$cached)
})

test_that("tg_metadata() is NULL for anything else", {
  expect_null(tg_metadata(tibble::tibble()))
  expect_null(tg_metadata(1))
})

# Counting --------------------------------------------------------------------

test_that("tg_count() reads the total from a one-row page", {
  recorded <- local_recorded_requests(mock_page(1, total = 6176))

  expect_equal(tg_count("parcerias", "parceria"), 6176)

  query <- request_query(recorded$requests[[1]])
  expect_equal(query[["tamanho_da_pagina"]], "1")
  expect_equal(query[["pagina"]], "1")
})

test_that("tg_count() sends the filters it was given", {
  recorded <- local_recorded_requests(mock_page(1, total = 3))

  tg_count("parcerias", "parceria", in_situacao_parceria = "Aprovada")

  expect_equal(
    request_query(recorded$requests[[1]])[["in_situacao_parceria"]],
    "Aprovada"
  )
})

# Arguments -------------------------------------------------------------------

test_that("the page size is bounded by what the service accepts", {
  # The message is asserted, not just the failure: without a mocked response an
  # out-of-range page size would error anyway when the request went out, so a
  # bare `expect_error()` passes even with the bound removed.
  expect_error(
    tg_get("parcerias", "parceria", .page_size = 201),
    "between 1 and 200"
  )
  expect_error(
    tg_get("parcerias", "parceria", .page_size = 0),
    "between 1 and 200"
  )

  # And nothing is sent: validation happens before the first request.
  recorded <- local_recorded_requests(list())
  expect_error(tg_get("parcerias", "parceria", .page_size = 201))
  expect_length(recorded$requests, 0L)
})

test_that("the page limit is the module's own, not one number for all", {
  expect_error(
    tg_get("fundoafundo", "planos_acao", .page_size = 1001),
    "between 1 and 1000"
  )
  expect_error(
    tg_get("ted", "termos_execucao", .page_size = 1001),
    "between 1 and 1000"
  )

  recorded <- local_recorded_requests(mock_page(2, total = 2, page_size = 1000))
  tg_get("ted", "termos_execucao", .page_size = 1000, .progress = FALSE)
  expect_equal(
    request_query(recorded$requests[[1]])[["tamanho_da_pagina"]], "1000"
  )
})

test_that("the default page size is the largest the module serves", {
  recorded <- local_recorded_requests(list(
    mock_page(2, total = 2, page_size = 1000),
    mock_page(2, total = 2, page_size = 200)
  ))

  tg_get("fundoafundo", "planos_acao", .progress = FALSE)
  tg_get("parcerias", "parceria", .progress = FALSE)

  expect_equal(
    request_query(recorded$requests[[1]])[["tamanho_da_pagina"]], "1000"
  )
  expect_equal(
    request_query(recorded$requests[[2]])[["tamanho_da_pagina"]], "200"
  )
})

test_that("limit and offset must be whole numbers", {
  expect_error(tg_get("parcerias", "parceria", .limit = 1.5))
  expect_error(tg_get("parcerias", "parceria", .offset = -1))
  expect_error(tg_get("parcerias", "parceria", .limit = "10"))
})

test_that("an infinite offset is refused", {
  expect_error(tg_get("parcerias", "parceria", .offset = Inf))
})
