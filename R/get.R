# Query verbs -----------------------------------------------------------------

#' Retrieve rows from a TransfereGov table
#'
#' Queries one of the forty-eight tables published by the three TransfereGov
#' open data APIs and returns them as a tibble, with columns typed from the
#' API's own schema.
#'
#' # Filters
#'
#' Name each filter after the column it applies to and give it a value or an
#' operator from [tg_operators()]. A bare value means "equals", a bare vector
#' means "is one of", and a list of operators applies several conditions to the
#' same column:
#'
#' ```r
#' tg_get("ted", "plano_acao", aa_ano_plano_acao = 2024)
#' tg_get("ted", "plano_acao", aa_ano_plano_acao = c(2024, 2025))
#' tg_get(
#'   "ted", "plano_acao",
#'   dt_inicio_vigencia = list(gte("2024-01-01"), lt("2025-01-01"))
#' )
#' ```
#'
#' Column names and categorical values are in Portuguese because they belong to
#' the API. Use [tg_fields()] to see them.
#'
#' # Pagination
#'
#' The service returns at most 1000 rows per request, whatever is asked of it,
#' so `.limit` above that is met by fetching successive pages. `.limit` counts
#' rows, not pages; use `Inf` for every matching row. Several tables hold
#' hundreds of thousands of rows, so check the size with [tg_count()] first.
#'
#' Pages are fetched with an explicit `.order` because offset pagination over an
#' unordered query has no defined row order and could repeat or skip rows. The
#' default order is the table's primary key when the API declares one, and its
#' identifier columns otherwise. The number of rows collected is checked against
#' the total the API reports, and a mismatch is reported as a warning.
#'
#' @param module A module name from [tg_modules()]: `"transferenciasespeciais"`,
#'   `"fundoafundo"` or `"ted"`. Aliases such as `"fundo_a_fundo"` are accepted.
#' @param table A table name from [tg_tables()].
#' @param ... Filters, named after the columns they apply to. See the Filters
#'   section.
#' @param .select Columns to return, as a character vector. `NULL`, the default,
#'   returns every column. Selecting fewer columns makes large queries markedly
#'   faster.
#' @param .order Sort order, as a character vector of column names, each
#'   optionally suffixed with `.asc` or `.desc`, and optionally with
#'   `.nullsfirst` or `.nullslast`. `NULL` uses the default order described
#'   under Pagination.
#' @param .limit Maximum number of rows to return. Use `Inf` for every matching
#'   row.
#' @param .offset Number of matching rows to skip before the first one returned.
#' @param .page_size Rows per request, between 1 and 1000.
#' @param .params Extra query parameters passed to the API verbatim, as a named
#'   list. This is the escape hatch for 'PostgREST' features the package does
#'   not model, such as `list(or = "(aa_ano_plano_acao.eq.2024,\
#'   aa_ano_plano_acao.eq.2025)")`.
#' @param .progress Whether to show a progress bar while collecting pages.
#'   `NULL` shows one in interactive sessions when more than one page is needed.
#' @param .cache Whether to serve the request from the response cache. `NULL`
#'   follows the `transferegovr.cache` option. See [tg_cache_dir()].
#' @param .base_url The API base URL. Defaults to [tg_base_url()].
#'
#' @return A tibble. [tg_metadata()] reports the totals the API gave and how
#'   many pages were fetched.
#' @export
#' @family queries
#' @examples
#' if (interactive()) {
#'   tg_get("ted", "plano_acao", aa_ano_plano_acao = gte(2024), .limit = 50)
#'
#'   tg_get(
#'     "fundoafundo", "plano_acao",
#'     .select = c("id_plano_acao", "vl_total_plano_acao"),
#'     .order = "vl_total_plano_acao.desc",
#'     .limit = 10
#'   )
#' }
tg_get <- function(
  module,
  table,
  ...,
  .select = NULL,
  .order = NULL,
  .limit = 1000,
  .offset = 0,
  .page_size = 1000,
  .params = list(),
  .progress = NULL,
  .cache = NULL,
  .base_url = tg_base_url()
) {
  module <- .tg_match_module(module)
  table <- .tg_match_table(module, table)
  fields <- .tg_table_fields(module, table)

  filters <- .tg_eval_filters(rlang::enquos(...))
  .tg_validate_columns(names(filters), fields, "filter")

  .tg_check_count(.limit, ".limit", allow_infinite = TRUE)
  .tg_check_count(.offset, ".offset", minimum = 0)
  .tg_check_count(.page_size, ".page_size", maximum = 1000L)
  .tg_check_params(.params)

  select <- .tg_prepare_select(.select, fields)
  order <- .tg_prepare_order(.order, fields, module, table)

  query <- c(
    filters,
    if (!is.null(select)) list(select = paste(select, collapse = ",")),
    list(order = paste(order, collapse = ",")),
    .params
  )

  collected <- .tg_collect(
    module = module,
    table = table,
    query = query,
    limit = .limit,
    offset = .offset,
    page_size = .page_size,
    progress = .progress,
    cache = .cache,
    base_url = .base_url
  )

  result <- .tg_rows_to_tibble(
    collected$rows,
    fields,
    columns = .tg_selected_names(.select)
  )

  .tg_attach_metadata(result, module, table, collected, order, select)
}

#' @rdname tg_get
#' @export
tg_obter <- tg_get

#' Count the rows a query matches
#'
#' Asks the API for the number of rows matching a set of filters without
#' retrieving them. Worth doing before a large [tg_get()]: the biggest table in
#' these APIs holds over a million rows, which is more than a thousand requests.
#'
#' @inheritParams tg_get
#' @param ... Filters, named after the columns they apply to. See [tg_get()].
#'
#' @return A single number.
#' @export
#' @family queries
#' @examples
#' if (interactive()) {
#'   tg_count("ted", "plano_acao")
#'   tg_count("ted", "plano_acao", aa_ano_plano_acao = 2024)
#' }
tg_count <- function(
  module,
  table,
  ...,
  .params = list(),
  .cache = NULL,
  .base_url = tg_base_url()
) {
  module <- .tg_match_module(module)
  table <- .tg_match_table(module, table)
  fields <- .tg_table_fields(module, table)

  filters <- .tg_eval_filters(rlang::enquos(...))
  .tg_validate_columns(names(filters), fields, "filter")
  .tg_check_params(.params)

  # `select=` with no columns asks PostgREST for rows with no fields, so the
  # count comes back without the body carrying any data.
  query <- c(filters, list(select = "", limit = "1"), .params)

  page <- .tg_fetch(module, table, query,
    count = TRUE, cache = .cache,
    base_url = .base_url
  )

  total <- page$range$total

  if (is.na(total)) {
    cli::cli_abort(
      c(
        "The API did not report a row count.",
        "i" = "Its {.field Content-Range} header carried no total."
      ),
      class = "transferegovr_response_error"
    )
  }

  total
}

#' @rdname tg_count
#' @export
tg_contar <- tg_count

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
#'   tg_ted("plano_acao", .limit = 10)
#'   tg_fundo_a_fundo("programa", .limit = 10)
#'   tg_transferencias_especiais("programa_especial")
#' }
NULL

#' @rdname module_shortcuts
#' @export
tg_ted <- function(table, ...) {
  tg_get("ted", table, ...)
}

#' @rdname module_shortcuts
#' @export
tg_fundo_a_fundo <- function(table, ...) {
  tg_get("fundoafundo", table, ...)
}

#' @rdname module_shortcuts
#' @export
tg_transferencias_especiais <- function(table, ...) {
  tg_get("transferenciasespeciais", table, ...)
}

# Result metadata -------------------------------------------------------------

#' Inspect what a query retrieved
#'
#' @param x A tibble returned by [tg_get()].
#'
#' @return A list holding the module and table queried, the total number of
#'   matching rows the API reported, how many rows and pages were retrieved, the
#'   offset, page size, order and selection used, and when the query ran.
#'   `NULL` for any other object.
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

.tg_attach_metadata <- function(result, module, table, collected, order,
                                select) {
  attr(result, "transferegovr_metadata") <- list(
    module = module,
    table = table,
    total_rows = collected$total,
    rows_returned = nrow(result),
    pages = collected$pages,
    offset = collected$offset,
    page_size = collected$page_size,
    order = order,
    select = select,
    cached = collected$cached,
    retrieved_at = Sys.time()
  )

  result
}

# Collection ------------------------------------------------------------------

.tg_collect <- function(
  module, table, query, limit, offset, page_size, progress, cache, base_url
) {
  page_size <- as.integer(page_size)
  offset <- as.numeric(offset)

  first_size <- if (is.finite(limit)) min(page_size, limit) else page_size

  first <- .tg_fetch(
    module, table,
    c(query, list(
      limit = format(first_size, scientific = FALSE),
      offset = format(offset, scientific = FALSE)
    )),
    count = TRUE, cache = cache, base_url = base_url
  )

  rows <- first$rows
  total <- first$range$total
  cached <- first$cached
  pages <- 1L

  wanted <- .tg_rows_wanted(limit, total, offset)

  if (length(rows) >= wanted) {
    return(.tg_collected(rows, total, pages, offset, page_size, cached, wanted))
  }

  # A first page that comes back empty while the API reports matching rows means
  # the offset is past the end, not that collection should continue.
  if (length(rows) == 0L) {
    return(.tg_collected(rows, total, pages, offset, page_size, cached, wanted))
  }

  remaining_pages <- ceiling((wanted - length(rows)) / page_size)
  bar <- .tg_progress_start(progress, remaining_pages, module, table)

  while (length(rows) < wanted) {
    size <- min(page_size, wanted - length(rows))

    page <- .tg_fetch(
      module, table,
      c(query, list(
        limit = format(size, scientific = FALSE),
        offset = format(offset + length(rows), scientific = FALSE)
      )),
      count = FALSE, cache = cache, base_url = base_url
    )

    cached <- cached && page$cached
    pages <- pages + 1L

    # The server stops sending rows before the reported total is reached only if
    # the table shrank mid-collection. Breaking here keeps the loop finite.
    if (length(page$rows) == 0L) {
      break
    }

    rows <- c(rows, page$rows)
    .tg_progress_step(bar)
  }

  .tg_progress_done(bar)

  .tg_collected(rows, total, pages, offset, page_size, cached, wanted)
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

.tg_fetch <- function(module, table, query, count, cache, base_url) {
  req <- .tg_request(module, table, query, count = count, base_url = base_url)

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

.tg_validate_columns <- function(columns, fields, what,
                                 call = rlang::caller_env()) {
  if (length(columns) == 0L || !.tg_validate()) {
    return(invisible(NULL))
  }

  unknown <- setdiff(unique(columns), fields$field)

  if (length(unknown) == 0L) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    c(
      "Unknown {what} column{?s}: {.val {unknown}}.",
      "i" = "See {.fn tg_fields} for the columns this table publishes.",
      "i" = "The packaged schema is from {.val {format(tg_schema_date())}}. If
             the API has gained a column since, set
             {.code options(transferegovr.validate = FALSE)}."
    ),
    class = "transferegovr_schema_error",
    call = call
  )
}

.tg_prepare_select <- function(select, fields, call = rlang::caller_env()) {
  if (is.null(select)) {
    return(NULL)
  }

  if (!is.character(select) || length(select) == 0L || anyNA(select)) {
    cli::cli_abort(
      "{.arg .select} must be a character vector of column names.",
      class = "transferegovr_schema_error",
      call = call
    )
  }

  .tg_validate_columns(.tg_selected_names(select), fields, "selected", call)

  select
}

# A selection entry may carry PostgREST syntax, `alias:column` for renaming or
# `column::type` for casting. Only the bare entries name a column that can be
# checked against the schema or used to shape an empty result.
.tg_selected_names <- function(select) {
  if (is.null(select)) {
    return(NULL)
  }

  if (any(grepl("[:()]", select))) {
    return(NULL)
  }

  select
}

.tg_prepare_order <- function(order, fields, module, table,
                              call = rlang::caller_env()) {
  if (is.null(order)) {
    return(.tg_default_order(module, table))
  }

  if (!is.character(order) || length(order) == 0L || anyNA(order)) {
    cli::cli_abort(
      "{.arg .order} must be a character vector of column names.",
      class = "transferegovr_schema_error",
      call = call
    )
  }

  columns <- sub("\\.(asc|desc)(\\.(nullsfirst|nullslast))?$", "", order)
  columns <- sub("\\.(nullsfirst|nullslast)$", "", columns)

  .tg_validate_columns(columns, fields, "ordering", call)

  order
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
    bound <- if (is.finite(maximum)) {
      " no greater than {maximum}"
    } else {
      ""
    }
    infinite <- if (allow_infinite) " or {.code Inf}" else ""

    cli::cli_abort(
      paste0(
        "{.arg ", argument, "} must be a whole number from {minimum}",
        bound, infinite, "."
      ),
      call = call
    )
  }

  invisible(NULL)
}

.tg_check_params <- function(params, call = rlang::caller_env()) {
  if (length(params) == 0L) {
    return(invisible(NULL))
  }

  names <- names(params)
  valid <- is.list(params) &&
    !is.null(names) &&
    all(nzchar(names)) &&
    all(vapply(
      params,
      function(p) length(p) == 1L && !is.list(p) && !is.na(p),
      logical(1)
    ))

  if (!valid) {
    cli::cli_abort(
      "{.arg .params} must be a named list of single values.",
      call = call
    )
  }

  reserved <- intersect(names, c("limit", "offset"))
  if (length(reserved) > 0L) {
    cli::cli_abort(
      c(
        "{.arg .params} must not set {.val {reserved}}.",
        "i" = "Use {.arg .limit} and {.arg .offset}, which pagination depends
               on."
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
