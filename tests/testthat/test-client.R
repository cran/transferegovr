test_that("the request targets the module and table under the base URL", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE)

  url <- httr2::url_parse(recorded$requests[[1]]$url)
  expect_equal(url$hostname, "api.transferegov.gestao.gov.br")
  expect_equal(url$path, "/ted/plano_acao")
})

test_that("the first request asks for an exact count", {
  # Without `Prefer: count=exact` the service reports the total as "*" and
  # multi-page collection has nothing to bound itself with.
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE)

  expect_equal(recorded$requests[[1]]$headers$Prefer, "count=exact")
})

test_that("filters, select and order all reach the query string", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get(
    "ted", "plano_acao",
    aa_ano_plano_acao = gte(2024),
    .select = c("id_plano_acao", "aa_ano_plano_acao"),
    .order = "id_plano_acao.desc",
    .limit = 1,
    .cache = FALSE
  )

  query <- request_query(recorded$requests[[1]])
  expect_equal(query[["aa_ano_plano_acao"]], "gte.2024")
  expect_equal(query[["select"]], "id_plano_acao,aa_ano_plano_acao")
  expect_equal(query[["order"]], "id_plano_acao.desc")
})

test_that("two conditions on one column become two query parameters", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get(
    "ted", "plano_acao",
    dt_inicio_vigencia = list(gte("2024-01-01"), lt("2025-01-01")),
    .limit = 1,
    .cache = FALSE
  )

  query <- request_query(recorded$requests[[1]])
  expect_equal(sum(names(query) == "dt_inicio_vigencia"), 2L)
  expect_setequal(
    unname(query[names(query) == "dt_inicio_vigencia"]),
    c("gte.2024-01-01", "lt.2025-01-01")
  )
})

test_that("an order is always sent, so paging has a defined row order", {
  # Offset pagination over an unordered query has no defined order in Postgres
  # and could repeat or skip rows across page boundaries.
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE)

  expect_true("order" %in% names(request_query(recorded$requests[[1]])))
})

test_that(".params reach the query verbatim", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get(
    "ted", "plano_acao",
    .params = list(
      or = "(aa_ano_plano_acao.eq.2024,aa_ano_plano_acao.eq.2025)"
    ),
    .limit = 1,
    .cache = FALSE
  )

  query <- request_query(recorded$requests[[1]])
  expect_equal(
    query[["or"]],
    "(aa_ano_plano_acao.eq.2024,aa_ano_plano_acao.eq.2025)"
  )
})

test_that(".params cannot hijack the pagination parameters", {
  expect_error(
    tg_get("ted", "plano_acao", .params = list(limit = 5), .cache = FALSE),
    "\\.limit"
  )
  expect_error(
    tg_get("ted", "plano_acao", .params = list(offset = 5), .cache = FALSE)
  )
})

test_that(".params is checked before any request is made", {
  expect_error(tg_get("ted", "plano_acao", .params = list(1), .cache = FALSE))
  expect_error(
    tg_get("ted", "plano_acao", .params = list(a = c(1, 2)), .cache = FALSE)
  )
  expect_error(
    tg_get("ted", "plano_acao", .params = list(a = NA), .cache = FALSE)
  )
})

test_that("a PostgREST error body is surfaced, not swallowed", {
  local_recorded_requests(mock_json_response(
    paste0(
      '{"code":"42703","details":null,"hint":null,',
      '"message":"column plano_acao.x does not exist"}'
    ),
    status = 400L
  ))

  expect_error(
    tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE),
    "does not exist",
    class = "transferegovr_http_400"
  )
})

test_that("hint and details from the API are shown when present", {
  local_recorded_requests(mock_json_response(
    paste0(
      '{"code":"PGRST100","details":"unexpected end of input",',
      '"hint":"try again","message":"parse error"}'
    ),
    status = 400L
  ))

  expect_error(
    tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE),
    "unexpected end of input",
    class = "transferegovr_http_error"
  )
})

test_that("an error status carries a class naming the status", {
  local_recorded_requests(mock_json_response("{}", status = 404L))

  expect_error(
    tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE),
    class = "transferegovr_http_404"
  )
})

test_that("an error body that is not an object still reports the status", {
  # `$` on an atomic value would error and mask the status being reported.
  local_recorded_requests(mock_json_response("\"boom\"", status = 500L))

  expect_error(
    tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE),
    "HTTP 500",
    class = "transferegovr_http_error"
  )
})

test_that("an unparseable body is reported as such", {
  local_recorded_requests(mock_json_response("not json at all"))

  expect_error(
    tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE),
    class = "transferegovr_response_error"
  )
})

test_that("a JSON object where an array of rows belongs is rejected", {
  # Reading `{"a":1}` as a single row would turn an error page into data.
  local_recorded_requests(mock_json_response('{"message":"surprise"}'))

  expect_error(
    tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE),
    class = "transferegovr_response_error"
  )
})

test_that("only transient statuses are retried", {
  expect_true(.tg_is_transient(mock_json_response("[]", status = 429L)))
  expect_true(.tg_is_transient(mock_json_response("[]", status = 503L)))
  expect_true(.tg_is_transient(mock_json_response("[]", status = 504L)))
  # A 400 is PostgREST rejecting the query and will fail identically every time.
  expect_false(.tg_is_transient(mock_json_response("[]", status = 400L)))
  expect_false(.tg_is_transient(mock_json_response("[]", status = 404L)))
})

test_that("backoff grows with the attempt and stays bounded", {
  expect_true(all(vapply(1:10, function(i) .tg_backoff(i) <= 90, logical(1))))
  expect_gt(
    mean(vapply(1:50, function(i) .tg_backoff(4), numeric(1))),
    mean(vapply(1:50, function(i) .tg_backoff(1), numeric(1)))
  )
})

test_that("the base URL must be an HTTP or HTTPS URL", {
  expect_error(
    tg_get("ted", "plano_acao", .base_url = "ftp://x", .cache = FALSE),
    class = "transferegovr_url_error"
  )
  expect_error(
    tg_get(
      "ted", "plano_acao",
      .base_url = c("https://a", "https://b"), .cache = FALSE
    ),
    class = "transferegovr_url_error"
  )
  expect_error(
    tg_get("ted", "plano_acao", .base_url = NA_character_, .cache = FALSE),
    class = "transferegovr_url_error"
  )
})

test_that("a trailing slash on the base URL does not double up in the path", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("ted", "plano_acao",
    .limit = 1, .cache = FALSE,
    .base_url = "https://api.transferegov.gestao.gov.br/"
  )

  url <- httr2::url_parse(recorded$requests[[1]]$url)
  expect_equal(url$path, "/ted/plano_acao")
})

test_that("the base URL option is honoured", {
  withr::local_options(transferegovr.base_url = "https://example.org")
  expect_equal(tg_base_url(), "https://example.org")

  recorded <- local_recorded_requests(mock_page(1, total = 1))
  tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE)

  url <- httr2::url_parse(recorded$requests[[1]]$url)
  expect_equal(url$hostname, "example.org")
})

test_that("the user agent identifies the package", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE)

  expect_match(recorded$requests[[1]]$options$useragent, "transferegovr")
})

test_that("an over-long URL is reported as such, not as a network failure", {
  # curl answers a URL this long with "Error in the HTTP2 framing layer", which
  # says nothing about the query that caused it.
  expect_error(
    tg_get("ted", "plano_acao", id_plano_acao = in_(1:2000), .cache = FALSE),
    "in_",
    class = "transferegovr_url_error"
  )
})

test_that("a normal request is nowhere near the URL limit", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("ted", "plano_acao",
    id_plano_acao = in_(1:50), .limit = 1,
    .cache = FALSE
  )

  expect_lt(nchar(recorded$requests[[1]]$url), .tg_max_url)
})
