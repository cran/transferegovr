# The request builder and the error translation. These services report a
# rejected query as a list of validation objects naming the offending
# parameter, which is worth surfacing rather than reducing to a status code.

test_that("the base URL is the public API and can be overridden", {
  expect_equal(tg_base_url(), "https://api-publica.transferegov.gestao.gov.br")

  withr::local_options(transferegovr.base_url = "https://example.org")
  expect_equal(tg_base_url(), "https://example.org")
})

test_that("the option redirects requests", {
  withr::local_options(transferegovr.base_url = "https://example.org")
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("parcerias", "parceria", .limit = 1, .progress = FALSE)

  expect_match(recorded$requests[[1]]$url, "^https://example\\.org/")
})

test_that(".base_url takes precedence over the option", {
  withr::local_options(transferegovr.base_url = "https://example.org")
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("parcerias", "parceria", .limit = 1, .progress = FALSE,
         .base_url = "https://elsewhere.test")

  expect_match(recorded$requests[[1]]$url, "^https://elsewhere\\.test/")
})

test_that("a base URL that is not a URL is refused", {
  expect_error(
    tg_get("parcerias", "parceria", .base_url = "not a url"),
    class = "transferegovr_url_error"
  )
  expect_error(
    tg_get("parcerias", "parceria", .base_url = c("a", "b")),
    class = "transferegovr_url_error"
  )
})

test_that("a trailing slash on the base URL does not double up", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("parcerias", "parceria", .limit = 1, .progress = FALSE,
         .base_url = "https://example.org/")

  expect_false(grepl("org//", recorded$requests[[1]]$url))
})

test_that("the request carries a user agent naming the package", {
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("parcerias", "parceria", .limit = 1, .progress = FALSE)

  expect_match(
    recorded$requests[[1]]$options$useragent,
    "transferegovr"
  )
})

# Errors ----------------------------------------------------------------------

test_that("a 422 surfaces the parameter the service objected to", {
  local_recorded_requests(mock_json_response(
    paste0(
      '{"detail":[{"type":"literal_error",',
      '"loc":["query","situacao_proposta"],',
      '"msg":"Input should be \'Aprovada\'"}]}'
    ),
    status = 422L
  ))

  expect_snapshot(
    error = TRUE,
    tg_get("parcerias", "proposta", .progress = FALSE)
  )
})

test_that("a 404 surfaces its plain-string detail", {
  local_recorded_requests(
    mock_json_response('{"detail":"Not Found"}', status = 404L)
  )

  expect_error(
    tg_get("parcerias", "parceria", .progress = FALSE),
    "Not Found",
    class = "transferegovr_http_404"
  )
})

test_that("an error body that is not JSON still reports the status", {
  local_recorded_requests(
    mock_json_response("<html>gateway</html>", status = 502L,
                       content_type = "text/html")
  )

  expect_error(
    tg_get("parcerias", "parceria", .progress = FALSE),
    class = "transferegovr_http_502"
  )
})

test_that("a detail carrying braces is not read as cli markup", {
  local_recorded_requests(mock_json_response(
    '{"detail":"unexpected {token} here"}',
    status = 400L
  ))

  expect_error(
    tg_get("parcerias", "parceria", .progress = FALSE),
    "\\{token\\}",
    class = "transferegovr_http_error"
  )
})

test_that("every validation error is reported, not only the first", {
  local_recorded_requests(mock_json_response(
    paste0(
      '{"detail":[',
      '{"loc":["query","a"],"msg":"first problem"},',
      '{"loc":["query","b"],"msg":"second problem"}]}'
    ),
    status = 422L
  ))

  message <- tryCatch(
    tg_get("parcerias", "parceria", .progress = FALSE),
    error = conditionMessage
  )

  expect_match(message, "first problem")
  expect_match(message, "second problem")
})

test_that("a body that is not JSON at all is reported as such", {
  local_recorded_requests(mock_json_response("not json"))

  expect_error(
    tg_get("parcerias", "parceria", .progress = FALSE),
    class = "transferegovr_response_error"
  )
})

# Transient failures ----------------------------------------------------------

test_that("only the statuses worth retrying are transient", {
  transient <- function(status) {
    .tg_is_transient(httr2::response(status_code = status))
  }

  expect_true(transient(429L))
  expect_true(transient(503L))
  expect_false(transient(422L))
  expect_false(transient(404L))
})

test_that("backoff stays inside its bounds", {
  values <- vapply(1:10, .tg_backoff, numeric(1))

  expect_true(all(values > 0))
  expect_true(all(values <= 90))
})

# URL length ------------------------------------------------------------------

test_that("an over-long URL is refused with a readable message", {
  # curl reports this as "Error in the HTTP2 framing layer", which says nothing
  # about the query that caused it.
  expect_error(
    tg_get(
      "parcerias", "proposta",
      ds_objeto = strrep("x", .tg_max_url),
      .progress = FALSE
    ),
    class = "transferegovr_url_error"
  )
})

# Freshness -------------------------------------------------------------------

test_that("tg_updated_at() parses the module's timestamp", {
  local_recorded_requests(
    mock_json_response('{"data_ultima_atualizacao":"2026-08-03T00:00:00"}')
  )

  expect_equal(
    tg_updated_at("parcerias"),
    as.POSIXct("2026-08-03 00:00:00", tz = "UTC")
  )
})

test_that("tg_updated_at() asks the module's own endpoint", {
  recorded <- local_recorded_requests(
    mock_json_response('{"data_ultima_atualizacao":"2026-08-03T00:00:00"}')
  )

  tg_updated_at("fundo_a_fundo")

  expect_equal(
    request_path(recorded$requests[[1]]),
    "fundoafundo/data-atualizacao"
  )
})

test_that("a response without the timestamp is an error", {
  local_recorded_requests(mock_json_response("{}"))

  expect_error(
    tg_updated_at("parcerias"),
    class = "transferegovr_response_error"
  )
})
