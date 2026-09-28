# Tests that talk to the real services. They are skipped unless
# TRANSFEREGOVR_LIVE_TESTS is set, because CRAN's checks must not depend on the
# government's servers.
#
#   TRANSFEREGOVR_LIVE_TESTS=1 devtools::test()

skip_unless_live <- function() {
  testthat::skip_if_not(
    identical(Sys.getenv("TRANSFEREGOVR_LIVE_TESTS"), "1"),
    "live tests are off"
  )
  testthat::skip_if_offline()
}

# A request built by the package, so it is throttled and retried like any
# other, but with its status left for the test to read.
raw_response <- function(module, table, query) {
  path <- paste0(module, "/", .tg_schema[[module]]$tables[[table]]$path)
  .tg_request(path, query, .tg_module_base_url(module, NULL)) |>
    httr2::req_perform()
}

test_that("every table in the frozen schema still answers", {
  skip_unless_live()

  tables <- tg_tables()

  failures <- character()

  # A warning counts as a failure too: a column whose type changed upstream
  # still parses, as character with a warning, so an errors-only check let
  # `proposta$in_formato_etapas` turning from an integer into an enum through.
  #
  # Except these: bank details declared as integers and sent masked as "***".
  # The warning is the package working as intended, not drift.
  masked <- c(
    "codigo_agencia_favorecido_gestao_financeira",
    "codigo_conta_favorecido_gestao_financeira",
    "codigo_agencia_beneficiario_subtransacao_gestao_financeira",
    "codigo_conta_beneficiario_subtransacao_gestao_financeira"
  )
  is_masked <- function(w) {
    any(vapply(masked, grepl, logical(1), x = conditionMessage(w),
               fixed = TRUE))
  }

  for (i in seq_len(nrow(tables))) {
    result <- tryCatch(
      withCallingHandlers(
        tg_get(tables$module[[i]], tables$table[[i]], .limit = 50,
               .progress = FALSE),
        warning = function(w) {
          if (is_masked(w)) invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e,
      warning = function(w) w
    )

    if (inherits(result, "condition")) {
      failures <- c(
        failures,
        paste0(tables$module[[i]], "/", tables$table[[i]], ": ",
               conditionMessage(result))
      )
    }
  }

  expect_equal(failures, character())
})

test_that("each module's frozen page limit is the one its service enforces", {
  skip_unless_live()

  # The limit is not uniform -- 200 for some modules, 1000 for others -- so it
  # is asserted per module, at the limit and one row past it.
  status_at <- function(module, size) {
    table <- tg_tables(module)$table[[1]]
    httr2::resp_status(
      raw_response(module, table, list(tamanho_da_pagina = size))
    )
  }

  modules <- tg_modules()
  for (i in seq_len(nrow(modules))) {
    limit <- modules$max_page_size[[i]]
    expect_equal(status_at(modules$module[[i]], limit), 200L,
                 label = paste(modules$module[[i]], "at its limit"))
    expect_equal(status_at(modules$module[[i]], limit + 1L), 422L,
                 label = paste(modules$module[[i]], "past its limit"))
  }
})

test_that("the frozen columns match what the services send", {
  skip_unless_live()

  tables <- tg_tables()
  drift <- character()

  for (i in seq_len(nrow(tables))) {
    module <- tables$module[[i]]
    table <- tables$table[[i]]

    # Only names are compared here; the masked columns' warning is covered
    # above.
    rows <- suppressWarnings(
      tg_get(module, table, .limit = 1, .progress = FALSE)
    )
    if (nrow(rows) == 0L) {
      next
    }

    expected <- tg_fields(module, table)$field
    unexpected <- setdiff(names(rows), expected)
    missing <- setdiff(expected, names(rows))

    if (length(unexpected) > 0L || length(missing) > 0L) {
      drift <- c(drift, paste0(
        module, "/", table,
        ": new ", paste(unexpected, collapse = ","),
        " gone ", paste(missing, collapse = ",")
      ))
    }
  }

  expect_equal(drift, character())
})

# Pagination ------------------------------------------------------------------
#
# A row count proves nothing about pagination. What proves pages neither
# overlap nor skip is fetching the same rows at two page sizes and comparing
# them, which is also what establishes that the server's order is stable.

test_that("the same rows come back whatever the page size", {
  skip_unless_live()

  strip <- function(x) {
    x <- as.data.frame(x)
    attr(x, "transferegovr_metadata") <- NULL
    rownames(x) <- NULL
    x
  }

  big <- tg_get("especiais", "meta_especiais", .limit = 450, .page_size = 200,
                .progress = FALSE)
  small <- tg_get("especiais", "meta_especiais", .limit = 450, .page_size = 50,
                  .progress = FALSE)

  expect_equal(nrow(big), 450L)
  expect_equal(strip(big), strip(small))
  expect_equal(tg_metadata(big)$pages, 3L)
  expect_equal(tg_metadata(small)$pages, 9L)
})

test_that("the order is stable deep into a large table", {
  skip_unless_live()

  first <- tg_get("especiais", "meta_especiais", .limit = 100,
                  .offset = 100000, .page_size = 100, .progress = FALSE)
  again <- tg_get("especiais", "meta_especiais", .limit = 100,
                  .offset = 100000, .page_size = 50, .progress = FALSE,
                  .cache = FALSE)

  expect_equal(first$id_meta, again$id_meta)
})

test_that("pages of 1000 hold the same rows as pages of 200", {
  skip_unless_live()

  # The larger limit is only safe if the order does not depend on page size,
  # which has to be shown at that size and at depth, not assumed from the
  # modules capped at 200.
  big <- tg_get("ted", "planos_acao_metas_etapas", .limit = 1000,
                .offset = 20000, .page_size = 1000, .progress = FALSE)
  small <- tg_get("ted", "planos_acao_metas_etapas", .limit = 1000,
                  .offset = 20000, .page_size = 200, .progress = FALSE)

  expect_equal(nrow(big), 1000L)
  expect_equal(big$id_etapa, small$id_etapa)
})

test_that("an offset lands on the row it names", {
  skip_unless_live()

  full <- tg_get("especiais", "meta_especiais", .limit = 300, .page_size = 200,
                 .progress = FALSE)
  offset <- tg_get("especiais", "meta_especiais", .limit = 100, .offset = 137,
                   .page_size = 60, .progress = FALSE)

  expect_equal(offset$id_meta, full$id_meta[138:237])
})

# Filters ---------------------------------------------------------------------

test_that("a filter narrows the result and the total agrees with it", {
  skip_unless_live()

  total <- tg_count("parcerias", "proposta")
  filtered <- tg_count("parcerias", "proposta", sg_uf_recebedor = "PE")

  expect_lt(filtered, total)
  expect_gt(filtered, 0)

  rows <- tg_get("parcerias", "proposta", sg_uf_recebedor = "PE", .limit = 25,
                 .progress = FALSE)
  expect_true(all(rows$sg_uf_recebedor == "PE"))
  expect_equal(tg_metadata(rows)$total_rows, filtered)
})

test_that("filters combine with AND", {
  skip_unless_live()

  uf <- tg_count("parcerias", "proposta", sg_uf_recebedor = "PE")
  both <- tg_count("parcerias", "proposta", sg_uf_recebedor = "PE",
                   situacao_proposta = "Aprovada")

  expect_lte(both, uf)
})

test_that("the enumerations the schema froze are the ones the service takes", {
  skip_unless_live()

  values <- tg_params("parcerias", "proposta")
  permitted <- values$values[[match("situacao_proposta", values$param)]]

  for (value in permitted) {
    expect_no_error(
      tg_count("parcerias", "proposta", situacao_proposta = value)
    )
  }
})

test_that("the parameters frozen as lists are the ones that take lists", {
  skip_unless_live()

  # The OpenAPI documents do not say which parameters take a list, so the
  # schema builder asked the service. Ask again: a list-taking parameter
  # rejects a non-integer with a message about comma-separated integers.
  wrong <- character()
  for (module in tg_modules()$module) {
    for (table in tg_tables(module)$table) {
      params <- tg_params(module, table)
      for (param in params$param[params$multiple]) {
        body <- httr2::resp_body_string(
          raw_response(module, table, stats::setNames(list("x"), param))
        )
        if (!grepl("separados por v", body, fixed = TRUE)) {
          wrong <- c(wrong, paste0(module, "/", table, " ", param))
        }
      }
    }
  }

  expect_equal(wrong, character())
})

test_that("a list means any of its values, up to the frozen limit", {
  skip_unless_live()

  one <- tg_count("ted", "planos_acao_metas", id_plano_acao = 3)
  other <- tg_count("ted", "planos_acao_metas", id_plano_acao = 4)
  both <- tg_count("ted", "planos_acao_metas", id_plano_acao = c(3, 4))
  expect_equal(both, one + other)

  # The client refuses a list over the limit before sending it, so the
  # server's own limit is checked with raw requests at the limit and past it.
  for (case in list(
    list("especiais", "devolucao_especiais", "id_devolucao"),
    list("ted", "planos_acao", "id_plano_acao")
  )) {
    limit <- with(
      tg_params(case[[1]], case[[2]]),
      max_values[param == case[[3]]]
    )
    status <- vapply(c(limit, limit + 1L), function(n) {
      query <- stats::setNames(
        list(paste(seq_len(n), collapse = ",")), case[[3]]
      )
      httr2::resp_status(raw_response(case[[1]], case[[2]], query))
    }, integer(1))
    expect_equal(status, c(200L, 400L), label = paste(case, collapse = "/"))
  }
})

# The property that motivates validating parameter names client-side ----------

test_that("the service really does ignore an unknown parameter", {
  skip_unless_live()

  # If this ever starts failing because the service began rejecting unknown
  # parameters, the client-side check in `.tg_validate_params()` could be
  # relaxed. Until then it is the only thing standing between a typo and a
  # silently unfiltered answer.
  withr::local_options(transferegovr.validate = FALSE)

  total <- tg_count("parcerias", "proposta")
  bogus <- tg_count("parcerias", "proposta", in_situacao_proposta = "Aprovada")

  expect_equal(bogus, total)

  # The newest module behaves the same way.
  expect_equal(
    tg_count("ted", "termos_execucao", tx_situacao = "x"),
    tg_count("ted", "termos_execucao")
  )
})

# Freshness -------------------------------------------------------------------

test_that("every module reports when it was last loaded", {
  skip_unless_live()

  for (module in tg_modules()$module) {
    stamp <- tg_updated_at(module)
    expect_s3_class(stamp, "POSIXct")
    expect_gt(stamp, as.POSIXct("2020-01-01", tz = "UTC"))
  }
})

# Nested columns --------------------------------------------------------------

test_that("a nested column arrives as a list column matching its sub-schema", {
  skip_unless_live()

  rows <- tg_get("parcerias", "programa", .limit = 20, .progress = FALSE)

  expect_type(rows$ufs_habilitadas, "list")

  populated <- rows$ufs_habilitadas[lengths(rows$ufs_habilitadas) > 0]
  skip_if(length(populated) == 0L, "no nested rows in this sample")

  expect_setequal(
    names(populated[[1]][[1]]),
    tg_fields("parcerias", "programa", nested = "ufs_habilitadas")$field
  )
})
