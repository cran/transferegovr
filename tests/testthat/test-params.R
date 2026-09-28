# Filters are the endpoints' own query parameters. There is no operator
# vocabulary to test; what matters is that a name or value the API would ignore
# or reject never reaches it.

test_that("a bare value becomes the parameter's value", {
  expect_equal(
    filter_string(in_situacao_parceria = "Aprovada"),
    c(in_situacao_parceria = "Aprovada")
  )
  expect_equal(
    filter_string(id_parceria = 37840),
    c(id_parceria = "37840")
  )
})

test_that("several filters are kept as separate parameters", {
  expect_equal(
    filter_string(
      module = "parcerias", table = "proposta",
      situacao_proposta = "Aprovada", sg_uf_recebedor = "PE"
    ),
    c(situacao_proposta = "Aprovada", sg_uf_recebedor = "PE")
  )
})

test_that("no filters produce no parameters", {
  expect_equal(
    .tg_eval_filters(rlang::quos(), .tg_table_params("parcerias", "parceria")),
    list()
  )
})

test_that("NULL drops the filter", {
  expect_equal(filter_string(id_parceria = NULL), NULL)
})

# Encoding --------------------------------------------------------------------

test_that("a large identifier is not rendered in scientific notation", {
  expect_equal(
    filter_string(cd_parceria = 202500037062),
    c(cd_parceria = "202500037062")
  )
})

test_that("dates and times are formatted the way the API declares them", {
  expect_equal(
    filter_string(dh_assinatura = as.Date("2025-03-04")),
    c(dh_assinatura = "2025-03-04")
  )
  expect_equal(
    filter_string(
      dh_assinatura = as.POSIXct("2025-03-04 05:06:07", tz = "UTC")
    ),
    c(dh_assinatura = "2025-03-04T05:06:07")
  )
})

test_that("logicals become true and false", {
  expect_equal(
    filter_string(
      module = "especiais", table = "planos_acao_especiais",
      id_plano_acao = TRUE
    ),
    c(id_plano_acao = "true")
  )
})

# Unknown parameters ----------------------------------------------------------
#
# The service answers 200 and ignores a parameter it does not recognise, so
# these are the tests that stop a typo returning the whole table.

test_that("an unknown parameter is an error, not a request", {
  expect_error(
    filter_string(module = "parcerias", table = "proposta",
                  in_situacao_proposta = "Aprovada"),
    class = "transferegovr_filter_error"
  )
})

test_that("the error explains why an ignored parameter is dangerous", {
  expect_snapshot(
    error = TRUE,
    tg_count("parcerias", "proposta", in_situacao_proposta = "Aprovada")
  )
})

test_that("a near miss is suggested", {
  expect_error(
    filter_string(module = "parcerias", table = "proposta",
                  situacao_propostas = "Aprovada"),
    "situacao_proposta",
    class = "transferegovr_filter_error"
  )
})

test_that("a name unlike anything known is reported without a suggestion", {
  expect_error(
    filter_string(module = "parcerias", table = "proposta", zzzzzzzz = 1),
    "Unknown filter",
    class = "transferegovr_filter_error"
  )
  expect_false(grepl(
    "Did you mean",
    tryCatch(
      filter_string(module = "parcerias", table = "proposta", zzzzzzzz = 1),
      error = conditionMessage
    )
  ))
})

test_that("validation can be switched off for a parameter added upstream", {
  withr::local_options(transferegovr.validate = FALSE)

  expect_equal(
    filter_string(module = "parcerias", table = "proposta", brand_new = 1),
    c(brand_new = "1")
  )
})

test_that("a column that is not filterable is still an unknown parameter", {
  # `meta_proposta` returns six columns and accepts twelve parameters, and the
  # two sets are not the same: filtering is not the same thing as selecting.
  fields <- tg_fields("parcerias", "meta_proposta")$field
  params <- tg_params("parcerias", "meta_proposta")$param

  unfilterable <- setdiff(fields, params)
  skip_if(length(unfilterable) == 0L, "every column is filterable here")

  args <- stats::setNames(list(1), unfilterable[[1]])
  expect_error(
    do.call(filter_string, c(args, module = "parcerias",
                             table = "meta_proposta")),
    class = "transferegovr_filter_error"
  )
})

# Enumerated values -----------------------------------------------------------

test_that("a permitted value passes", {
  expect_equal(
    filter_string(module = "parcerias", table = "proposta",
                  situacao_proposta = "Aprovada"),
    c(situacao_proposta = "Aprovada")
  )
})

test_that("a value outside the enumeration is rejected with the list", {
  expect_snapshot(
    error = TRUE,
    tg_count("parcerias", "proposta", situacao_proposta = "Aprovado")
  )
})

test_that("enumerations are checked case sensitively, as the API does", {
  expect_error(
    filter_string(module = "parcerias", table = "proposta",
                  sg_uf_recebedor = "pe"),
    class = "transferegovr_filter_error"
  )
})

# Shapes the API cannot express -----------------------------------------------

test_that("several values for one parameter are refused", {
  expect_snapshot(
    error = TRUE,
    tg_count("parcerias", "proposta", sg_uf_recebedor = c("PE", "PB"))
  )
})

# Parameters that take a list ------------------------------------------------

test_that("a list-taking parameter sends its values comma-separated", {
  # The service reads `id_parceria=3,4` as "is one of"; it would read a
  # repeated `id_parceria=3&id_parceria=4` as just 4.
  expect_equal(filter_string(id_parceria = c(3, 4)), c(id_parceria = "3,4"))
  expect_equal(filter_string(id_parceria = c("3", "4")), c(id_parceria = "3,4"))

  recorded <- local_recorded_requests(mock_page(2, total = 2))
  tg_get("parcerias", "parceria", id_parceria = c(3, 4), .progress = FALSE)
  expect_equal(request_query(recorded$requests[[1]])[["id_parceria"]], "3,4")
})

test_that("large identifiers in a list are not in scientific notation", {
  expect_equal(
    filter_string(id_parceria = c(202500037062, 1e5)),
    c(id_parceria = "202500037062,100000")
  )
})

test_that("repeated values in a list are sent once", {
  expect_equal(filter_string(id_parceria = c(3, 3, 4)), c(id_parceria = "3,4"))
})

test_that("a list is capped at what the parameter accepts", {
  # 200 here, 100 in especiais: the limit is frozen per parameter.
  expect_error(filter_string(id_parceria = 1:201), "at most 200",
               class = "transferegovr_filter_error")
  expect_length(filter_string(id_parceria = 1:200), 1L)

  expect_error(
    filter_string(id_devolucao = 1:101, module = "especiais",
                  table = "devolucao_especiais"),
    "at most 100",
    class = "transferegovr_filter_error"
  )

  recorded <- local_recorded_requests(list())
  expect_error(tg_get("parcerias", "parceria", id_parceria = 1:201))
  expect_length(recorded$requests, 0L)
})

test_that("a list takes only whole numbers, and no missing ones", {
  filter_error <- "transferegovr_filter_error"
  expect_error(filter_string(id_parceria = c(1.5, 2)), class = filter_error)
  expect_error(filter_string(id_parceria = c(-1, 2)), class = filter_error)
  expect_error(filter_string(id_parceria = c("3", "x")), class = filter_error)
  expect_error(filter_string(id_parceria = c(3, NA)), class = filter_error)
  expect_error(filter_string(id_parceria = c(TRUE, FALSE)),
               class = filter_error)
})

test_that("tg_params() marks which parameters take a list", {
  params <- tg_params("parcerias", "parceria")

  expect_true(params$multiple[[match("id_parceria", params$param)]])
  expect_equal(params$max_values[[match("id_parceria", params$param)]], 200L)
  expect_false(params$multiple[[match("in_situacao_parceria", params$param)]])
  expect_equal(
    params$max_values[[match("in_situacao_parceria", params$param)]], 1L
  )
})

test_that("a repeated parameter is refused rather than silently truncated", {
  # The service keeps the last occurrence and discards the rest without
  # reporting it, which would make the first condition vanish.
  expect_error(
    tg_count("parcerias", "proposta",
             sg_uf_recebedor = "PE", sg_uf_recebedor = "PB"),
    class = "transferegovr_filter_error"
  )
})

test_that("an unnamed filter is refused", {
  expect_error(
    .tg_eval_filters(
      rlang::quos("Aprovada"),
      .tg_table_params("parcerias", "proposta")
    ),
    class = "transferegovr_filter_error"
  )
})

test_that("a missing value is refused", {
  expect_error(
    filter_string(id_parceria = NA),
    class = "transferegovr_filter_error"
  )
})

test_that("an empty value is refused", {
  expect_error(
    filter_string(id_parceria = integer()),
    class = "transferegovr_filter_error"
  )
})

test_that("a list is refused", {
  expect_error(
    filter_string(id_parceria = list(1, 2)),
    class = "transferegovr_filter_error"
  )
})

# Discovery -------------------------------------------------------------------

test_that("tg_params() describes what the endpoint accepts", {
  params <- tg_params("parcerias", "proposta")

  expect_s3_class(params, "tbl_df")
  expect_named(
    params,
    c("param", "r_type", "api_type", "values", "pattern", "description",
      "multiple", "max_values")
  )
  expect_true("situacao_proposta" %in% params$param)
})

test_that("enumerated parameters carry their values and the rest are empty", {
  params <- tg_params("parcerias", "proposta")

  situacao <- params$values[[match("situacao_proposta", params$param)]]
  expect_true("Aprovada" %in% situacao)

  identifier <- params$values[[match("id_proposta", params$param)]]
  expect_length(identifier, 0L)
})

test_that("tg_parametros() is the same function", {
  expect_equal(
    tg_parametros("parcerias", "proposta"),
    tg_params("parcerias", "proposta")
  )
})
