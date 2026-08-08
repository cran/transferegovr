# Response cache --------------------------------------------------------------
#
# The APIs send no `ETag`, `Cache-Control` or `Last-Modified` header, so
# `httr2::req_cache()` has nothing to work with and would store nothing. This is
# a small cache of its own, keyed on the request URL.
#
# It writes to the session's temporary directory by default, so nothing is left
# on the user's filesystem without being asked for. Set the
# `transferegovr.cache_dir` option or the `TRANSFEREGOVR_CACHE_DIR` environment
# variable to keep responses between sessions;
# `tools::R_user_dir("transferegovr", "cache")` is the conventional place.

.tg_cache_default_dir <- function() {
  file.path(tempdir(), "transferegovr-cache")
}

.tg_cache_enabled <- function() {
  enabled <- getOption("transferegovr.cache", TRUE)

  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    cli::cli_abort(
      "Option {.code transferegovr.cache} must be {.code TRUE} or
       {.code FALSE}."
    )
  }

  enabled
}

.tg_cache_ttl <- function() {
  ttl <- getOption("transferegovr.cache_ttl", 3600)

  if (
    !is.numeric(ttl) || length(ttl) != 1L || is.na(ttl) || ttl < 0
  ) {
    cli::cli_abort(
      "Option {.code transferegovr.cache_ttl} must be a non-negative number of
       seconds."
    )
  }

  ttl
}

#' Where cached responses are stored
#'
#' Called with no argument, reports the directory in use. Called with a path,
#' switches to it for the rest of the session.
#'
#' By default responses are cached in the session's temporary directory, so they
#' are discarded when R exits. To keep them between sessions, set this to a
#' persistent path, for example `tg_cache_dir(tools::R_user_dir("transferegovr",
#' "cache"))`, or set the `TRANSFEREGOVR_CACHE_DIR` environment variable in your
#' `.Renviron`.
#'
#' Caching is controlled by the `transferegovr.cache` option (`TRUE` by default)
#' and entries expire after `transferegovr.cache_ttl` seconds (3600 by default).
#' The data behind these APIs is refreshed daily.
#'
#' @param path A directory to cache responses in, or `NULL` to report the
#'   current one. The directory is created if it does not exist.
#'
#' @return The cache directory, invisibly when setting it.
#' @export
#' @family cache
#' @examples
#' tg_cache_dir()
tg_cache_dir <- function(path = NULL) {
  if (is.null(path)) {
    configured <- getOption("transferegovr.cache_dir")

    if (is.null(configured)) {
      env <- Sys.getenv("TRANSFEREGOVR_CACHE_DIR", unset = "")
      configured <- if (nzchar(env)) env else .tg_cache_default_dir()
    }

    return(configured)
  }

  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    cli::cli_abort("{.arg path} must be a single string.")
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  options(transferegovr.cache_dir = path)

  invisible(path)
}

#' @rdname tg_cache_dir
#' @export
tg_cache_pasta <- tg_cache_dir

#' Delete cached responses
#'
#' @return The number of files removed, invisibly.
#' @export
#' @family cache
#' @examples
#' tg_cache_clear()
tg_cache_clear <- function() {
  dir <- tg_cache_dir()

  if (!dir.exists(dir)) {
    return(invisible(0L))
  }

  files <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  removed <- sum(file.remove(files))

  cli::cli_inform("Removed {removed} cached response{?s} from {.path {dir}}.")

  invisible(removed)
}

#' @rdname tg_cache_clear
#' @export
tg_cache_limpar <- tg_cache_clear

.tg_cache_path <- function(key) {
  file.path(tg_cache_dir(), paste0(key, ".rds"))
}

.tg_cache_key <- function(url) {
  rlang::hash(list(url = url, version = 1L))
}

.tg_cache_read <- function(key) {
  path <- .tg_cache_path(key)

  if (!file.exists(path)) {
    return(NULL)
  }

  entry <- tryCatch(readRDS(path), error = function(error) NULL)

  # A truncated or foreign file is a cache miss, not a failure: the request will
  # simply be made again.
  if (!is.list(entry) || is.null(entry$created) || is.null(entry$value)) {
    return(NULL)
  }

  age <- as.numeric(difftime(Sys.time(), entry$created, units = "secs"))
  if (!is.finite(age) || age < 0 || age > .tg_cache_ttl()) {
    return(NULL)
  }

  entry$value
}

.tg_cache_write <- function(key, value) {
  dir <- tg_cache_dir()

  if (!dir.exists(dir)) {
    created <- dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!created && !dir.exists(dir)) {
      return(invisible(FALSE))
    }
  }

  path <- .tg_cache_path(key)
  # Write to a temporary name and rename, so a crash mid-write cannot leave a
  # half-written file that a later session would read as a hit.
  temporary <- paste0(path, ".", Sys.getpid(), ".tmp")

  ok <- tryCatch(
    {
      saveRDS(list(created = Sys.time(), value = value), temporary)
      file.rename(temporary, path)
    },
    error = function(error) FALSE
  )

  if (!isTRUE(ok)) {
    unlink(temporary)
  }

  invisible(isTRUE(ok))
}
