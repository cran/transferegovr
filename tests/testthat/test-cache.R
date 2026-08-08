local_cache <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_options(
    transferegovr.cache = TRUE,
    transferegovr.cache_dir = dir,
    .local_envir = env
  )
  dir
}

test_that("a repeated request is served from the cache", {
  local_cache()
  recorded <- local_recorded_requests(mock_page(2, total = 2))

  first <- tg_get("ted", "plano_acao", .limit = 2)
  second <- tg_get("ted", "plano_acao", .limit = 2)

  expect_length(recorded$requests, 1L)
  expect_equal(first, second, ignore_attr = TRUE)
  expect_false(tg_metadata(first)$cached)
  expect_true(tg_metadata(second)$cached)
})

test_that("a different query is a different cache entry", {
  local_cache()
  recorded <- local_recorded_requests(list(
    mock_page(1, total = 1), mock_page(1, total = 1)
  ))

  tg_get("ted", "plano_acao", .limit = 1)
  tg_get("ted", "plano_acao", aa_ano_plano_acao = 2024, .limit = 1)

  expect_length(recorded$requests, 2L)
})

test_that("the cache is skipped when it is turned off", {
  local_cache()
  recorded <- local_recorded_requests(list(
    mock_page(1, total = 1), mock_page(1, total = 1)
  ))

  tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE)
  tg_get("ted", "plano_acao", .limit = 1, .cache = FALSE)

  expect_length(recorded$requests, 2L)
})

test_that("the .cache argument overrides the option", {
  local_cache()
  withr::local_options(transferegovr.cache = FALSE)
  recorded <- local_recorded_requests(mock_page(1, total = 1))

  tg_get("ted", "plano_acao", .limit = 1, .cache = TRUE)
  tg_get("ted", "plano_acao", .limit = 1, .cache = TRUE)

  expect_length(recorded$requests, 1L)
})

test_that("an entry past its time to live is refetched", {
  local_cache()
  withr::local_options(transferegovr.cache_ttl = 0)
  recorded <- local_recorded_requests(list(
    mock_page(1, total = 1), mock_page(1, total = 1)
  ))

  tg_get("ted", "plano_acao", .limit = 1)
  Sys.sleep(0.01)
  tg_get("ted", "plano_acao", .limit = 1)

  expect_length(recorded$requests, 2L)
})

test_that("a corrupt cache file is a miss, not a failure", {
  dir <- local_cache()
  recorded <- local_recorded_requests(list(
    mock_page(1, total = 1), mock_page(1, total = 1)
  ))

  tg_get("ted", "plano_acao", .limit = 1)
  file <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  writeBin(as.raw(c(1, 2, 3)), file[[1]])

  expect_no_error(tg_get("ted", "plano_acao", .limit = 1))
  expect_length(recorded$requests, 2L)
})

test_that("a cache entry missing its fields is a miss", {
  dir <- local_cache()
  recorded <- local_recorded_requests(list(
    mock_page(1, total = 1), mock_page(1, total = 1)
  ))

  tg_get("ted", "plano_acao", .limit = 1)
  file <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  saveRDS(list(created = Sys.time()), file[[1]])

  expect_no_error(tg_get("ted", "plano_acao", .limit = 1))
  expect_length(recorded$requests, 2L)
})

test_that("an entry stamped in the future is a miss", {
  dir <- local_cache()
  recorded <- local_recorded_requests(list(
    mock_page(1, total = 1), mock_page(1, total = 1)
  ))

  tg_get("ted", "plano_acao", .limit = 1)
  file <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  entry <- readRDS(file[[1]])
  entry$created <- Sys.time() + 86400
  saveRDS(entry, file[[1]])

  tg_get("ted", "plano_acao", .limit = 1)
  expect_length(recorded$requests, 2L)
})

test_that("tg_cache_clear() empties the directory", {
  dir <- local_cache()
  local_recorded_requests(mock_page(1, total = 1))

  tg_get("ted", "plano_acao", .limit = 1)
  expect_length(list.files(dir, pattern = "\\.rds$"), 1L)

  expect_message(removed <- tg_cache_clear())
  expect_equal(removed, 1L)
  expect_length(list.files(dir, pattern = "\\.rds$"), 0L)
})

test_that("clearing a directory that does not exist is not an error", {
  withr::local_options(
    transferegovr.cache_dir = file.path(tempdir(), "transferegovr-absent")
  )
  expect_equal(tg_cache_clear(), 0L)
})

test_that("the cache defaults to the session's temporary directory", {
  withr::local_options(transferegovr.cache_dir = NULL)
  withr::local_envvar(TRANSFEREGOVR_CACHE_DIR = "")

  # Nothing is written to the user's filesystem unless they ask for it.
  expect_equal(tg_cache_dir(), file.path(tempdir(), "transferegovr-cache"))
})

test_that("the environment variable sets a persistent cache directory", {
  withr::local_options(transferegovr.cache_dir = NULL)
  withr::local_envvar(TRANSFEREGOVR_CACHE_DIR = "/tmp/transferegovr-test")

  expect_equal(tg_cache_dir(), "/tmp/transferegovr-test")
})

test_that("the option takes priority over the environment variable", {
  withr::local_envvar(TRANSFEREGOVR_CACHE_DIR = "/tmp/from-env")
  withr::local_options(transferegovr.cache_dir = "/tmp/from-option")

  expect_equal(tg_cache_dir(), "/tmp/from-option")
})

test_that("setting the cache directory creates it", {
  dir <- file.path(withr::local_tempdir(), "nested", "cache")
  withr::defer(options(transferegovr.cache_dir = NULL))

  tg_cache_dir(dir)

  expect_true(dir.exists(dir))
  expect_equal(tg_cache_dir(), dir)
})

test_that("tg_cache_dir() rejects a path that is not a single string", {
  expect_error(tg_cache_dir(1))
  expect_error(tg_cache_dir(c("a", "b")))
})

test_that("malformed cache options are rejected", {
  withr::local_options(transferegovr.cache = "yes")
  expect_error(.tg_cache_enabled())

  withr::local_options(transferegovr.cache = TRUE, transferegovr.cache_ttl = -1)
  expect_error(.tg_cache_ttl())
})

test_that(".cache must be TRUE or FALSE", {
  expect_error(tg_get("ted", "plano_acao", .limit = 1, .cache = "yes"))
})

test_that("a cached multi-page collection reports itself as cached", {
  local_cache()
  local_recorded_requests(list(
    mock_page(2, from = 1, total = 4),
    mock_page(2, from = 3, total = 4, first = 2)
  ))

  args <- list(
    "ted", "plano_acao",
    .limit = 4, .page_size = 2, .progress = FALSE
  )
  first <- do.call(tg_get, args)
  second <- do.call(tg_get, args)

  expect_false(tg_metadata(first)$cached)
  expect_true(tg_metadata(second)$cached)
  expect_equal(first$id_plano_acao, second$id_plano_acao)
})
