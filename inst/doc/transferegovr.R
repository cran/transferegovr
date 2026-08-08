## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## ----setup--------------------------------------------------------------------
# library(transferegovr)

## -----------------------------------------------------------------------------
# tg_modules()
# #> # A tibble: 3 × 4
# #>   module                  label                      tables url
# #>   <chr>                   <chr>                       <int> <chr>
# #> 1 transferenciasespeciais Special transfers              14 https://api.tran…
# #> 2 fundoafundo             Fund-to-fund transfers         21 https://api.tran…
# #> 3 ted                     Decentralized credit (TED)     13 https://api.tran…

## -----------------------------------------------------------------------------
# tg_tables("ted")
# #> # A tibble: 13 × 5
# #>   module table                      columns primary_key description
# #>   <chr>  <chr>                        <int> <chr>       <chr>
# #> 1 ted    evento                          10 NA          …
# #> 2 ted    nota_credito                    11 NA          …
# #> 3 ted    plano_acao                      20 NA          …
# #> …

## -----------------------------------------------------------------------------
# tg_fields("ted", "plano_acao")
# #> # A tibble: 20 × 5
# #>   field                 r_type    pg_type           primary_key description
# #>   <chr>                 <chr>     <chr>             <lgl>       <chr>
# #> 1 id_plano_acao         double    bigint            FALSE       Identifica…
# #> 2 id_programa           double    bigint            FALSE       Identifica…
# #> 3 sigla_unidade_descen… character character varyi… FALSE       Sigla da U…
# #> …

## -----------------------------------------------------------------------------
# tg_get("ted", "plano_acao", .limit = 5)

## -----------------------------------------------------------------------------
# tg_get("ted", "plano_acao", aa_ano_plano_acao = 2024)

## -----------------------------------------------------------------------------
# tg_get("ted", "plano_acao", aa_ano_plano_acao = c(2024, 2025))

## -----------------------------------------------------------------------------
# tg_operators()
# #> # A tibble: 15 × 3
# #>   operator postgrest meaning
# #>   <chr>    <chr>     <chr>
# #> 1 eq       eq        equals
# #> 2 neq      neq       does not equal
# #> 3 gt       gt        greater than
# #> …

## -----------------------------------------------------------------------------
# tg_get(
#   "ted", "plano_acao",
#   aa_ano_plano_acao = gte(2024),
#   tx_objeto_plano_acao = ilike("*pesquisa*"),
#   tx_justificativa_plano_acao = not(is_null())
# )

## -----------------------------------------------------------------------------
# tg_get(
#   "ted", "plano_acao",
#   dt_inicio_vigencia = list(gte("2024-01-01"), lt("2025-01-01"))
# )

## -----------------------------------------------------------------------------
# tg_get(
#   "ted", "plano_acao",
#   dt_inicio_vigencia = list(
#     gte(as.Date("2024-01-01")), lt(as.Date("2025-01-01"))
#   )
# )

## -----------------------------------------------------------------------------
# tg_get(
#   "ted", "plano_acao",
#   .select = c("id_plano_acao", "vl_total_plano_acao", "dt_inicio_vigencia"),
#   .order = "vl_total_plano_acao.desc",
#   .limit = 10
# )

## -----------------------------------------------------------------------------
# tg_get(
#   "ted", "plano_acao",
#   .order = c("aa_ano_plano_acao.desc", "vl_total_plano_acao.desc.nullslast")
# )

## -----------------------------------------------------------------------------
# tg_count("ted", "plano_acao")
# #> [1] 6176
# 
# tg_count("fundoafundo", "gestao_financeira_lancamentos")
# #> [1] 1115444

## -----------------------------------------------------------------------------
# plans <- tg_get("ted", "plano_acao", .limit = 5)
# 
# class(plans$dt_inicio_vigencia)
# #> [1] "Date"
# class(plans$in_forma_execucao_direta)
# #> [1] "logical"
# class(plans$aa_ano_plano_acao)
# #> [1] "integer"

## -----------------------------------------------------------------------------
# plans <- tg_get("ted", "plano_acao", .limit = 2500)
# 
# tg_metadata(plans)
# #> $module
# #> [1] "ted"
# #> $table
# #> [1] "plano_acao"
# #> $total_rows
# #> [1] 6176
# #> $rows_returned
# #> [1] 2500
# #> $pages
# #> [1] 3
# #> …

