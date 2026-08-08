## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----setup--------------------------------------------------------------------
# library(transferegovr)
# library(dplyr)

## -----------------------------------------------------------------------------
# programas <- tg_get(
#   "ted", "programa",
#   .select = c(
#     "id_programa", "tx_nome_programa", "sigla_unidade_descentralizadora"
#   ),
#   .limit = Inf
# )
# 
# planos <- tg_get(
#   "ted", "plano_acao",
#   .select = c(
#     "id_plano_acao", "id_programa", "vl_total_plano_acao", "aa_ano_plano_acao"
#   ),
#   .limit = Inf
# )
# 
# planos |>
#   inner_join(programas, by = "id_programa") |>
#   group_by(sigla_unidade_descentralizadora) |>
#   summarise(planos = n(), total = sum(vl_total_plano_acao, na.rm = TRUE)) |>
#   arrange(desc(total))
# #> # A tibble: 5 × 3
# #>   sigla_unidade_descentralizadora planos         total
# #>   <chr>                            <int>         <dbl>
# #> 1 MDS                                229 422595208242.
# #> 2 MS                                 794  14472373138.
# #> 3 FNDCT                              154  13779027285.
# #> 4 MIDR                               603   5450415783.
# #> 5 MAPA                               284   2835531350.

## -----------------------------------------------------------------------------
# sum(!planos$id_programa %in% programas$id_programa)
# #> [1] 0

## -----------------------------------------------------------------------------
# planos_2024 <- tg_get(
#   "ted", "plano_acao",
#   aa_ano_plano_acao = 2024,
#   .select = c("id_plano_acao", "id_programa"),
#   .limit = Inf
# )
# 
# notas <- tg_get(
#   "ted", "nota_credito",
#   id_plano_acao = in_(planos_2024$id_plano_acao),
#   .limit = Inf
# )

## -----------------------------------------------------------------------------
# class(planos$id_plano_acao)
# #> [1] "numeric"

## -----------------------------------------------------------------------------
# programas_ff <- tg_get("fundoafundo", "programa", .limit = Inf)
# 
# nrow(programas_ff)
# #> [1] 129
# n_distinct(programas_ff$id_programa)
# #> [1] 125

## -----------------------------------------------------------------------------
# tg_tables() |> filter(!is.na(primary_key))

