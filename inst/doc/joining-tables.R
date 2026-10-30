## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## -----------------------------------------------------------------------------
# library(transferegovr)
# library(dplyr)

## -----------------------------------------------------------------------------
# pernambuco <- tg_get("discricionarias", "proponentes",
#                      uf_proponente = "PE", .limit = Inf)
# 
# selecao <- tg_get("discricionarias", "propostas_selecao_pac",
#                   id_proponente = head(pernambuco$id_proponente, 200),
#                   .limit = Inf)
# 
# formalizacao <- tg_get("discricionarias", "proposta_formalizacao_pac",
#                        id_proposta_selecao_pac =
#                          head(selecao$id_proposta_selecao_pac, 200),
#                        .limit = Inf)
# 
# propostas <- tg_get("discricionarias", "propostas",
#                     id_proposta = head(formalizacao$id_proposta, 200),
#                     .limit = Inf)

## -----------------------------------------------------------------------------
# beneficiarios <- tg_get("especiais", "beneficiarios_especiais", .limit = Inf)
# 
# pernambuco <- beneficiarios |>
#   filter(uf_beneficiario == "PE")
# 
# planos <- tg_get("especiais", "planos_acao_especiais", .limit = Inf) |>
#   semi_join(pernambuco, by = "id_beneficiario")

## -----------------------------------------------------------------------------
# planos <- tg_get(
#   "fundoafundo", "planos_acao",
#   uf_ente_recebedor_plano_acao = "PE",
#   .limit = Inf
# )

## -----------------------------------------------------------------------------
# propostas <- tg_get(
#   "parcerias", "proposta",
#   sg_uf_recebedor = "PE", situacao_proposta = "Aprovada",
#   .limit = Inf
# )
# 
# parcerias <- tg_get("parcerias", "parceria", .limit = Inf) |>
#   semi_join(propostas, by = "id_proposta")
# 
# contas <- tg_get("parcerias", "parceria_conta", .limit = Inf) |>
#   semi_join(parcerias, by = "id_parceria")

## -----------------------------------------------------------------------------
# library(purrr)
# 
# grupos <- split(contas$id_parceria_conta,
#                 ceiling(seq_along(contas$id_parceria_conta) / 200))
# 
# extratos <- list_rbind(map(grupos, function(ids) {
#   tg_get("parcerias", "extrato_bancario", id_parceria_conta = ids, .limit = Inf)
# }))

## -----------------------------------------------------------------------------
# planos <- tg_get("ted", "planos_acao", .limit = 200)
# 
# notas <- tg_get("ted", "notas_credito",
#                 id_plano_acao = planos$id_plano_acao, .limit = Inf)
# 
# eventos <- tg_get("ted", "eventos",
#                   id_nota = head(unique(notas$id_nota), 200), .limit = Inf)

## -----------------------------------------------------------------------------
# library(tidyr)
# 
# programas <- tg_get("parcerias", "programa", .limit = Inf)
# 
# programas |>
#   select(id_programa, ufs_habilitadas) |>
#   unnest_longer(ufs_habilitadas) |>
#   unnest_wider(ufs_habilitadas)
# #> # A tibble: … × 4
# #>   id_programa nm_uf        sg_uf cd_ibge
# #>         <dbl> <chr>        <chr>   <dbl>
# #> 1           7 MINAS GERAIS MG         31
# #> …

## -----------------------------------------------------------------------------
# tg_fields("parcerias", "programa", nested = "ufs_habilitadas")

## -----------------------------------------------------------------------------
# planos <- tg_get("especiais", "planos_acao_especiais", .limit = 500)
# beneficiarios <- tg_get("especiais", "beneficiarios_especiais", .limit = Inf)
# 
# sum(!planos$id_beneficiario %in% beneficiarios$id_beneficiario)

## -----------------------------------------------------------------------------
# one_page <- tg_get("fundoafundo", "programas", .limit = 200)
# 
# nrow(one_page)
# length(unique(one_page$id_programa))

