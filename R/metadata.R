# Schema access ---------------------------------------------------------------
#
# `.tg_schema`, `.tg_module_labels`, `.tg_module_aliases` and
# `.tg_schema_built_at` live in R/sysdata.rda and are rebuilt by
# data-raw/schema.R from the OpenAPI documents the APIs publish.

.tg_match_module <- function(module, call = rlang::caller_env()) {
  if (!is.character(module) || length(module) != 1L || is.na(module)) {
    cli::cli_abort(
      "{.arg module} must be a single string.",
      class = "transferegovr_schema_error",
      call = call
    )
  }

  key <- gsub("[^a-z0-9]+", "_", tolower(trimws(module)))
  # `[[` on a named character vector aborts on a missing name rather than
  # returning NULL, so `%||%` cannot be the fallback here.
  key <- if (key %in% names(.tg_module_aliases)) {
    unname(.tg_module_aliases[[key]])
  } else {
    gsub("_", "", key)
  }

  if (!key %in% names(.tg_schema)) {
    cli::cli_abort(
      c(
        "Unknown module: {.val {module}}.",
        "i" = "Choose one of {.val {names(.tg_schema)}}.",
        "i" = "See {.fn tg_modules} for what each one covers."
      ),
      class = "transferegovr_schema_error",
      call = call
    )
  }

  key
}

.tg_match_table <- function(module, table, call = rlang::caller_env()) {
  if (!is.character(table) || length(table) != 1L || is.na(table)) {
    cli::cli_abort(
      "{.arg table} must be a single string.",
      class = "transferegovr_schema_error",
      call = call
    )
  }

  tables <- names(.tg_schema[[module]]$tables)
  key <- trimws(table)

  if (!key %in% tables) {
    # The same table name exists in more than one module with different columns,
    # so a miss is often a module mix-up rather than a typo.
    elsewhere <- names(.tg_schema)[vapply(
      .tg_schema,
      function(m) key %in% names(m$tables),
      logical(1)
    )]
    elsewhere <- setdiff(elsewhere, module)

    message <- c(
      "Module {.val {module}} has no table {.val {table}}.",
      "i" = "See {.code tg_tables(\"{module}\")} for the {length(tables)}
             tables it publishes."
    )
    if (length(elsewhere) > 0L) {
      message <- c(
        message,
        "i" = "Module{?s} {.val {elsewhere}} do{?es/} publish a table with that
               name, with different columns."
      )
    }

    cli::cli_abort(
      message,
      class = "transferegovr_schema_error",
      call = call
    )
  }

  key
}

.tg_table_fields <- function(module, table) {
  .tg_schema[[module]]$tables[[table]]$fields
}

# The order used for multi-page collection. Offset pagination over an unordered
# query has no defined row order in Postgres, so pages could overlap or skip;
# an explicit order makes the sequence reproducible. A declared primary key is a
# total order. Failing that, identifier-like columns are the best available key,
# and `.tg_check_complete()` still verifies the collected count afterwards.
.tg_default_order <- function(module, table) {
  fields <- .tg_table_fields(module, table)

  keys <- fields$field[fields$primary_key]
  if (length(keys) > 0L) {
    return(paste0(keys, ".asc"))
  }

  identifiers <- fields$field[grepl("^(id|sq|co|nr|cd)_", fields$field)]
  if (length(identifiers) > 0L) {
    return(paste0(identifiers, ".asc"))
  }

  paste0(fields$field[[1L]], ".asc")
}

# Discovery -------------------------------------------------------------------

#' List the TransfereGov API modules
#'
#' @return A tibble with one row per module: its name, the label used in this
#'   documentation, the number of tables it publishes, and its API base URL.
#' @export
#' @family discovery
#' @examples
#' tg_modules()
tg_modules <- function() {
  modules <- names(.tg_schema)

  tibble::tibble(
    module = modules,
    label = unname(.tg_module_labels[modules]),
    tables = vapply(
      .tg_schema[modules], function(m) length(m$tables), integer(1),
      USE.NAMES = FALSE
    ),
    url = paste0(.tg_default_base_url(), "/", modules)
  )
}

#' @rdname tg_modules
#' @export
tg_modulos <- tg_modules

#' List the tables a module publishes
#'
#' @param module A module name from [tg_modules()]. Aliases such as
#'   `"fundo_a_fundo"` are accepted. `NULL` lists the tables of every module.
#' @param counts If `TRUE`, adds a `rows` column with the number of rows each
#'   table currently holds. This is the only part of this function that needs a
#'   network connection: it makes one request per table, so `tg_tables(counts =
#'   TRUE)` with no module makes forty-eight. Responses are cached.
#' @param modulo Portuguese alias for `module`, available in [tg_tabelas()] and
#'   [tg_campos()].
#' @param contagens Portuguese alias for `counts`, available only in
#'   [tg_tabelas()].
#'
#' @return A tibble with one row per table: its module, name, number of columns,
#'   the primary key when the API declares one, and the description published in
#'   the API schema. With `counts = TRUE`, also the current number of rows.
#' @export
#' @family discovery
#' @examples
#' tg_tables("ted")
#' tg_tables()
#'
#' if (interactive()) {
#'   # How big is everything, largest first?
#'   tg_tables(counts = TRUE)[order(-tg_tables(counts = TRUE)$rows), ]
#' }
tg_tables <- function(module = NULL, counts = FALSE) {
  modules <- if (is.null(module)) {
    names(.tg_schema)
  } else {
    .tg_match_module(module)
  }

  if (!is.logical(counts) || length(counts) != 1L || is.na(counts)) {
    cli::cli_abort("{.arg counts} must be {.code TRUE} or {.code FALSE}.")
  }

  rows <- lapply(modules, function(m) {
    tables <- .tg_schema[[m]]$tables
    tibble::tibble(
      module = m,
      table = names(tables),
      columns = vapply(tables, function(t) nrow(t$fields), integer(1),
        USE.NAMES = FALSE
      ),
      primary_key = vapply(
        tables,
        function(t) {
          keys <- t$fields$field[t$fields$primary_key]
          if (length(keys) == 0L) {
            NA_character_
          } else {
            paste(keys, collapse = ", ")
          }
        },
        character(1),
        USE.NAMES = FALSE
      ),
      description = vapply(tables, function(t) t$description %||% NA_character_,
        character(1),
        USE.NAMES = FALSE
      )
    )
  })

  result <- purrr::list_rbind(rows)

  if (!counts) {
    return(result)
  }

  result$rows <- .tg_row_counts(result$module, result$table)
  result
}

#' @rdname tg_tables
#' @export
tg_tabelas <- function(modulo = NULL, contagens = FALSE) {
  tg_tables(modulo, contagens)
}

# One request per table. A progress bar because forty-eight throttled requests
# take the better part of a minute the first time, and none after that while
# the cache is warm.
.tg_row_counts <- function(modules, tables) {
  bar <- cli::cli_progress_bar(
    name = "counting rows",
    total = length(tables),
    .envir = rlang::caller_env()
  )

  counts <- vapply(seq_along(tables), function(i) {
    cli::cli_progress_update(id = bar)
    tg_count(modules[[i]], tables[[i]])
  }, numeric(1))

  cli::cli_progress_done(id = bar)

  counts
}

#' List the columns of a table
#'
#' Every column name may be used as a filter in [tg_get()] and [tg_count()], in
#' `.select`, and in `.order`. Column names and categorical values stay in
#' Portuguese because they are the API's own contract.
#'
#' @inheritParams tg_tables
#' @param table A table name from [tg_tables()].
#' @param tabela Portuguese alias for `table`, available only in
#'   [tg_campos()].
#'
#' @return A tibble with one row per column: its name, the R type the package
#'   coerces it to, the Postgres type the API reports, whether it is part of the
#'   declared primary key, and its description.
#' @export
#' @family discovery
#' @examples
#' tg_fields("ted", "plano_acao")
tg_fields <- function(module, table) {
  module <- .tg_match_module(module)
  table <- .tg_match_table(module, table)
  .tg_table_fields(module, table)
}

#' @rdname tg_fields
#' @export
tg_campos <- function(modulo, tabela) {
  tg_fields(modulo, tabela)
}

#' Report the frozen schema's build date
#'
#' The package validates filters and types columns against a copy of the APIs'
#' OpenAPI documents taken on this date. A column added upstream since then is
#' still returned, but is typed by inspection rather than from the schema.
#'
#' @return A `Date`.
#' @export
#' @family discovery
#' @examples
#' tg_schema_date()
tg_schema_date <- function() {
  .tg_schema_built_at
}
