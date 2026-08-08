## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----setup--------------------------------------------------------------------
# library(transferegovr)

## -----------------------------------------------------------------------------
# plans <- tg_get("ted", "plano_acao", .limit = 2500)
# 
# nrow(plans)
# #> [1] 2500
# tg_metadata(plans)$pages
# #> [1] 3

## -----------------------------------------------------------------------------
# tg_count("fundoafundo", "gestao_financeira_lancamentos")
# #> [1] 1115444

## -----------------------------------------------------------------------------
# tg_tables(counts = TRUE) |>
#   dplyr::arrange(desc(rows))
# #> # A tibble: 48 × 6
# #>   module                  table                           columns    rows …
# #>   <chr>                   <chr>                             <int>   <dbl>
# #> 1 fundoafundo             gestao_financeira_lancamentos        32 1115444
# #> 2 fundoafundo             gestao_financeira_subtransacoes      16  377666
# #> 3 transferenciasespeciais historico_pagamento_especial          5  281163
# #> 4 fundoafundo             plano_acao_historico                  5  183379
# #> 5 transferenciasespeciais meta_especial                        16  156016
# #> # ℹ 43 more rows

## -----------------------------------------------------------------------------
# tg_get(
#   "fundoafundo", "gestao_financeira_lancamentos",
#   .select = c(
#     "id_lancamento_gestao_financeira",
#     "id_plano_acao",
#     "data_lancamento_gestao_financeira",
#     "valor_lancamento_gestao_financeira"
#   ),
#   .limit = Inf
# )

## -----------------------------------------------------------------------------
# tg_count(
#   "fundoafundo", "gestao_financeira_lancamentos",
#   data_lancamento_gestao_financeira = list(gte("2025-01-01"), lt("2026-01-01"))
# )

## -----------------------------------------------------------------------------
# tg_metadata(plans)$order
# #> [1] "id_plano_acao.asc" "id_programa.asc"   "sq_instrumento.asc"

## -----------------------------------------------------------------------------
# tg_get("ted", "plano_acao", .order = "vl_total_plano_acao.desc", .limit = 2500)

## -----------------------------------------------------------------------------
# tg_get(
#   "ted", "plano_acao",
#   .order = c("vl_total_plano_acao.desc", "id_plano_acao.asc"),
#   .limit = 2500
# )

## -----------------------------------------------------------------------------
# first <- tg_get("ted", "plano_acao_etapa", .limit = 20000)
# rest <- tg_get("ted", "plano_acao_etapa", .limit = Inf, .offset = 20000)

## -----------------------------------------------------------------------------
# tg_cache_dir()
# #> [1] "/tmp/RtmpXXXX/transferegovr-cache"

## -----------------------------------------------------------------------------
# tg_cache_dir(tools::R_user_dir("transferegovr", "cache"))

## -----------------------------------------------------------------------------
# options(
#   transferegovr.requests_per_minute = 30,
#   transferegovr.max_tries = 6,
#   transferegovr.timeout = 120
# )

