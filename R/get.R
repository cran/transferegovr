# Query verbs -----------------------------------------------------------------

#' Retrieve rows from a TransfereGov table
#'
#' Queries one of the seventy-four tables published by the TransfereGov open
#' data APIs and returns them as a tibble, with columns typed from the API's own
#' schema.
#'
#' # Filters
#'
#' Name each filter after one of the table's query parameters and give it a
#' value. Parameters are combined with AND:
#'
#' ```r
#' tg_get("parcerias", "proposta", situacao_proposta = "Aprovada")
#' tg_get(
#'   "parcerias", "proposta",
#'   sg_uf_recebedor = "PE", ano_proposta = 2025
#' )
#' ```
#'
#' The services compare for equality: there is no greater-than and no pattern
#' match. Most parameters take one value. Some identifier parameters take
#' several and match any of them — [tg_params()] marks them as `multiple`, with
#' the most each accepts in `max_values`:
#'
#' ```r
#' tg_get("ted", "planos_acao_metas", id_plano_acao = c(3, 4))
#' ```
#'
#' For any other parameter, query each value and bind the results.
#'
#' Parameter names, and the permitted values of the enumerated ones, are in
#' Portuguese because they belong to the API. Use [tg_params()] to see them.
#' A name the packaged schema does not know is an error rather than a request:
#' these services ignore a parameter they do not recognize and answer with the
#' whole table, so an unchecked typo would return plausible, wrong data.
#'
#' # Pagination
#'
#' Each request returns one page of at most the module's page limit — 200 rows
#' for `especiais` and `parcerias`, 1000 for `fundoafundo` and `ted` — so a
#' larger `.limit` is met by fetching successive pages. `.limit` counts rows,
#' not pages; use `Inf` for every matching row. Several tables hold hundreds of
#' thousands of rows, so check the size with [tg_count()] first.
#'
#' Row order is the server's and cannot be set: these APIs publish no ordering
#' parameter. It was checked to be stable across page sizes, across repeated
#' calls and at depth, which is what makes multi-page collection safe. The
#' number of rows collected is checked against the total the API reports, and a
#' mismatch is reported as a warning.
#'
#' @param module A module name from [tg_modules()]: `"especiais"`,
#'   `"fundoafundo"`, `"parcerias"` or `"ted"`. Aliases such as
#'   `"fundo_a_fundo"` are accepted.
#' @param table A table name from [tg_tables()].
#' @param ... Filters, named after the parameters they set. See the Filters
#'   section.
#' @param .limit Maximum number of rows to return. Use `Inf` for every matching
#'   row.
#' @param .offset Number of matching rows to skip before the first one returned.
#' @param .page_size Rows per request. `NULL`, the default, asks for the largest
#'   page the module serves, which [tg_modules()] reports as `max_page_size`.
#' @param .progress Whether to show a progress bar while collecting pages.
#'   `NULL` shows one in interactive sessions when more than one page is needed.
#' @param .cache Whether to serve the request from the response cache. `NULL`
#'   follows the `transferegovr.cache` option. See [tg_cache_dir()].
#' @param .base_url The API base URL. Defaults to [tg_base_url()].
#'
#' @return A tibble. [tg_metadata()] reports the totals the API gave and how
#'   many pages were fetched. A column the API sends as an array of objects
#'   comes back as a list column; [tg_fields()] describes what is inside it.
#' @export
#' @family queries
#' @examples
#' if (interactive()) {
#'   tg_get("parcerias", "proposta", sg_uf_recebedor = "PE", .limit = 50)
#'
#'   tg_get("fundoafundo", "planos_acao", .limit = 10)
#' }
tg_get <- function(
  module,
  table,
  ...,
  .limit = 1000,
  .offset = 0,
  .page_size = NULL,
  .progress = NULL,
  .cache = NULL,
  .base_url = NULL
) {
  module <- .tg_match_module(module)
  table <- .tg_match_table(module, table)
  fields <- .tg_table_fields(module, table)

  filters <- .tg_eval_filters(
    rlang::enquos(...),
    .tg_table_params(module, table)
  )

  .tg_check_count(.limit, ".limit", allow_infinite = TRUE)
  .tg_check_count(.offset, ".offset", minimum = 0)
  max_page_size <- .tg_schema[[module]]$max_page_size
  .page_size <- .page_size %||% max_page_size
  .tg_check_count(.page_size, ".page_size", maximum = max_page_size)

  collected <- .tg_collect(
    module = module,
    table = table,
    query = filters,
    limit = .limit,
    offset = .offset,
    page_size = .page_size,
    progress = .progress,
    cache = .cache,
    base_url = .tg_module_base_url(module, .base_url)
  )

  result <- .tg_rows_to_tibble(collected$rows, fields)

  .tg_attach_metadata(result, module, table, collected, filters)
}

#' @rdname tg_get
#' @export
tg_obter <- tg_get

#' Count the rows a query matches
#'
#' Asks the API for the number of rows matching a set of filters without
#' retrieving them. Worth doing before a large [tg_get()]: the biggest table in
#' these APIs holds over a million rows, which at 200 rows a request is more
#' than five thousand requests.
#'
#' @inheritParams tg_get
#' @param ... Filters, named after the parameters they set. See [tg_get()].
#'
#' @return A single number.
#' @export
#' @family queries
#' @examples
#' if (interactive()) {
#'   tg_count("parcerias", "proposta")
#'   tg_count("parcerias", "proposta", situacao_proposta = "Aprovada")
#' }
tg_count <- function(
  module,
  table,
  ...,
  .cache = NULL,
  .base_url = NULL
) {
  module <- .tg_match_module(module)
  table <- .tg_match_table(module, table)

  filters <- .tg_eval_filters(
    rlang::enquos(...),
    .tg_table_params(module, table)
  )

  # One row is the smallest page the service accepts, and the envelope reports
  # the full total whatever the page size.
  page <- .tg_fetch(
    module, table,
    c(filters, list(pagina = "1", tamanho_da_pagina = "1")),
    cache = .cache,
    base_url = .tg_module_base_url(module, .base_url)
  )

  page$total
}

#' @rdname tg_count
#' @export
tg_contar <- tg_count

#' When a module's data was last refreshed
#'
#' Each module publishes the timestamp of its last load. It is the only
#' freshness signal these APIs give: they send no `ETag`, `Cache-Control` or
#' `Last-Modified` header.
#'
#' @inheritParams tg_get
#'
#' @return A `POSIXct` in UTC.
#' @export
#' @family discovery
#' @examples
#' if (interactive()) {
#'   tg_updated_at("parcerias")
#' }
tg_updated_at <- function(module, .cache = NULL, .base_url = NULL) {
  module <- .tg_match_module(module)

  path <- paste0(module, "/", .tg_schema[[module]]$timestamp_path)
  req <- .tg_request(path, list(), .tg_module_base_url(module, .base_url))
  body <- .tg_perform_json(req)

  value <- body$data_ultima_atualizacao

  if (!is.character(value) || length(value) != 1L) {
    cli::cli_abort(
      c(
        "The TransfereGov API reported no update timestamp.",
        "i" = "Its response carried no {.field data_ultima_atualizacao}."
      ),
      class = "transferegovr_response_error"
    )
  }

  .tg_as_datetime(value, "data_ultima_atualizacao")
}

#' @rdname tg_updated_at
#' @export
tg_atualizado_em <- tg_updated_at

# Module shortcuts ------------------------------------------------------------

#' Query a single module
#'
#' Thin wrappers over [tg_get()] with the module fixed, for code that stays
#' within one API.
#'
#' @param table A table name from [tg_tables()] for that module.
#' @param ... Passed to [tg_get()]: filters, and any of its `.`-prefixed
#'   arguments.
#'
#' @return A tibble, as [tg_get()] returns.
#' @name module_shortcuts
#' @family queries
#' @examples
#' if (interactive()) {
#'   tg_parcerias("proposta", .limit = 10)
#'   tg_fundo_a_fundo("programas", .limit = 10)
#'   tg_especiais("programas_especiais", .limit = 10)
#'   tg_ted("termos_execucao", .limit = 10)
#' }
NULL

#' @rdname module_shortcuts
#' @export
tg_parcerias <- function(table, ...) {
  tg_get("parcerias", table, ...)
}

#' @rdname module_shortcuts
#' @export
tg_fundo_a_fundo <- function(table, ...) {
  tg_get("fundoafundo", table, ...)
}

#' @rdname module_shortcuts
#' @export
tg_especiais <- function(table, ...) {
  tg_get("especiais", table, ...)
}

#' @rdname module_shortcuts
#' @export
tg_ted <- function(table, ...) {
  tg_get("ted", table, ...)
}

# Result metadata -------------------------------------------------------------

#' Inspect what a query retrieved
#'
#' @param x A tibble returned by [tg_get()].
#'
#' @return A list holding the module and table queried, the total number of
#'   matching rows the API reported, how many rows and pages were retrieved, the
#'   offset, page size and filters used, and when the query ran. `NULL` for any
#'   other object.
#' @export
#' @family queries
#' @examples
#' tg_metadata(tibble::tibble())
tg_metadata <- function(x) {
  attr(x, "transferegovr_metadata", exact = TRUE)
}

#' @rdname tg_metadata
#' @export
tg_metadados <- tg_metadata

.tg_attach_metadata <- function(result, module, table, collected, filters) {
  attr(result, "transferegovr_metadata") <- list(
    module = module,
    table = table,
    total_rows = collected$total,
    rows_returned = nrow(result),
    pages = collected$pages,
    offset = collected$offset,
    page_size = collected$page_size,
    filters = filters,
    cached = collected$cached,
    retrieved_at = Sys.time()
  )

  result
}

# Collection ------------------------------------------------------------------

# Pagination is by page number, so an offset that is not a whole number of
# pages is met by fetching the page it falls in and dropping the rows before
# it. That keeps `.offset` meaning "rows to skip" whatever `.page_size` is.
.tg_collect <- function(
  module, table, query, limit, offset, page_size, progress, cache, base_url
) {
  page_size <- as.integer(page_size)
  offset <- as.numeric(offset)

  first_page <- floor(offset / page_size) + 1
  drop <- offset %% page_size

  first <- .tg_fetch(
    module, table,
    c(query, .tg_page_query(first_page, page_size)),
    cache = cache, base_url = base_url
  )

  total <- first$total
  cached <- first$cached
  pages <- 1L

  rows <- if (drop > 0L) utils::tail(first$rows, -drop) else first$rows
  wanted <- .tg_rows_wanted(limit, total, offset)

  if (length(rows) >= wanted || length(first$rows) == 0L) {
    rows <- utils::head(rows, if (is.finite(wanted)) wanted else length(rows))
    return(.tg_collected(rows, total, pages, offset, page_size, cached, wanted))
  }

  remaining_pages <- ceiling((wanted - length(rows)) / page_size)
  bar <- .tg_progress_start(progress, remaining_pages, module, table)

  page_number <- first_page

  while (length(rows) < wanted) {
    page_number <- page_number + 1

    page <- .tg_fetch(
      module, table,
      c(query, .tg_page_query(page_number, page_size)),
      cache = cache, base_url = base_url
    )

    cached <- cached && page$cached
    pages <- pages + 1L

    # The server stops sending rows before the reported total is reached only
    # if the table shrank mid-collection. Breaking here keeps the loop finite.
    if (length(page$rows) == 0L) {
      break
    }

    rows <- c(rows, page$rows)
    .tg_progress_step(bar)
  }

  .tg_progress_done(bar)

  rows <- utils::head(rows, if (is.finite(wanted)) wanted else length(rows))

  .tg_collected(rows, total, pages, offset, page_size, cached, wanted)
}

.tg_page_query <- function(page, page_size) {
  list(
    pagina = format(page, scientific = FALSE),
    tamanho_da_pagina = format(page_size, scientific = FALSE)
  )
}

.tg_collected <- function(rows, total, pages, offset, page_size, cached,
                          wanted) {
  if (is.finite(wanted) && length(rows) != wanted) {
    cli::cli_warn(
      c(
        "Collected {length(rows)} row{?s} where the API reported {wanted}.",
        "i" = "The table may have changed while it was being read.",
        "i" = "Check {.fn tg_metadata} on the result."
      ),
      class = "transferegovr_incomplete_warning"
    )
  }

  list(
    rows = rows,
    total = total,
    pages = pages,
    offset = offset,
    page_size = page_size,
    cached = cached
  )
}

# How many rows the caller should end up with: the smaller of what was asked
# for and what is left after the offset.
.tg_rows_wanted <- function(limit, total, offset) {
  if (is.na(total)) {
    return(limit)
  }

  min(limit, max(0, total - offset))
}

.tg_module_base_url <- function(module, base_url) {
  base_url %||% getOption(
    "transferegovr.base_url",
    .tg_schema[[module]]$base_url
  )
}

.tg_fetch <- function(module, table, query, cache, base_url) {
  path <- paste0(module, "/", .tg_schema[[module]]$tables[[table]]$path)
  req <- .tg_request(path, query, base_url = base_url)

  use_cache <- cache %||% .tg_cache_enabled()
  if (!is.logical(use_cache) || length(use_cache) != 1L || is.na(use_cache)) {
    cli::cli_abort("{.arg .cache} must be {.code TRUE} or {.code FALSE}.")
  }

  key <- if (use_cache) .tg_cache_key(req$url) else NULL

  if (!is.null(key)) {
    hit <- .tg_cache_read(key)
    if (!is.null(hit)) {
      hit$cached <- TRUE
      return(hit)
    }
  }

  result <- .tg_perform(req)
  result$cached <- FALSE

  if (!is.null(key)) {
    .tg_cache_write(key, result)
  }

  result
}

# Validation ------------------------------------------------------------------

.tg_validate <- function() {
  validate <- getOption("transferegovr.validate", TRUE)

  if (!is.logical(validate) || length(validate) != 1L || is.na(validate)) {
    cli::cli_abort(
      "Option {.code transferegovr.validate} must be {.code TRUE} or
       {.code FALSE}."
    )
  }

  validate
}

.tg_check_count <- function(value, argument, minimum = 1, maximum = Inf,
                            allow_infinite = FALSE,
                            call = rlang::caller_env()) {
  # `is.finite()` has to come before `%%`: `Inf %% 1` is `NaN`, so the condition
  # itself would error instead of reporting the bad argument.
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    value >= minimum &&
    (
      (allow_infinite && is.infinite(value) && value > 0) ||
        (is.finite(value) && value %% 1 == 0 && value <= maximum)
    )

  if (!valid) {
    range <- if (is.finite(maximum)) {
      "between {minimum} and {maximum}"
    } else {
      "of {minimum} or more"
    }
    infinite <- if (allow_infinite) ", or {.code Inf}" else ""

    cli::cli_abort(
      paste0(
        "{.arg ", argument, "} must be a whole number ", range, infinite, "."
      ),
      call = call
    )
  }

  invisible(NULL)
}

# Progress --------------------------------------------------------------------

.tg_progress_start <- function(progress, pages, module, table) {
  show <- progress %||% (rlang::is_interactive() && pages > 1)

  if (!isTRUE(show) || pages < 1) {
    return(NULL)
  }

  cli::cli_progress_bar(
    name = paste0(module, "/", table),
    total = pages,
    .envir = rlang::caller_env(2)
  )
}

.tg_progress_step <- function(bar) {
  if (!is.null(bar)) {
    cli::cli_progress_update(id = bar)
  }
  invisible(NULL)
}

.tg_progress_done <- function(bar) {
  if (!is.null(bar)) {
    cli::cli_progress_done(id = bar)
  }
  invisible(NULL)
}
