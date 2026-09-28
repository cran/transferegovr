## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## -----------------------------------------------------------------------------
# library(transferegovr)

## -----------------------------------------------------------------------------
# tg_modules()
# #> # A tibble: 4 × 5
# #>   module      label                  tables max_page_size url
# #>   <chr>       <chr>                   <int>         <int> <chr>
# #> 1 especiais   Special transfers          23           200 https://api-publ…
# #> 2 fundoafundo Fund-to-fund transfers     20          1000 https://api-publ…
# #> 3 parcerias   Partnerships               17           200 https://api-publ…
# #> 4 ted         Decentralized credit       14          1000 https://api-publ…

## -----------------------------------------------------------------------------
# tg_tables("parcerias")
# #> # A tibble: 17 × 6
# #>    module    table                 path                  columns params
# #>    <chr>     <chr>                 <chr>                   <int>  <int>
# #>  1 parcerias analise_proposta      analise-proposta            7      5
# #>  2 parcerias beneficiario_emenda_… beneficiario_emenda_…      16     14
# #>  3 parcerias cronograma_desembolso cronograma-desembolso       7      7
# #>  …

## -----------------------------------------------------------------------------
# tg_fields("parcerias", "proposta")
# #> # A tibble: 43 × 5
# #>    field               r_type    api_type nested description
# #>    <chr>               <chr>     <chr>    <chr>  <chr>
# #>  1 id_proposta         double    integer  NA     Identificador único da prop…
# #>  2 id_programa         double    integer  NA     Identificador do programa a…
# #>  …
# 
# tg_params("parcerias", "proposta")

## -----------------------------------------------------------------------------
# propostas <- tg_get(
#   "parcerias", "proposta",
#   sg_uf_recebedor = "PE",
#   situacao_proposta = "Aprovada",
#   .limit = 100
# )

## -----------------------------------------------------------------------------
# params <- tg_params("parcerias", "parceria")
# params[params$multiple, c("param", "max_values")]
# #> # A tibble: 2 × 2
# #>   param       max_values
# #>   <chr>            <int>
# #> 1 id_parceria        200
# #> 2 id_proposta        200
# 
# tg_get("parcerias", "parceria", id_proposta = c(1, 2))

## -----------------------------------------------------------------------------
# library(purrr)
# 
# nordeste <- c("PE", "PB", "AL", "RN", "CE", "SE", "BA", "PI", "MA")
# 
# propostas <- list_rbind(map(
#   nordeste,
#   \(uf) tg_get("parcerias", "proposta", sg_uf_recebedor = uf, .limit = Inf)
# ))

## -----------------------------------------------------------------------------
# params <- tg_params("parcerias", "proposta")
# params[lengths(params$values) > 0, c("param", "values")]
# #> # A tibble: 5 × 2
# #>   param                  values
# #>   <chr>                  <list>
# #> 1 sg_uf_recebedor        <chr [27]>
# #> 2 situacao_proposta      <chr [5]>
# #> 3 in_situacao_analise    <chr [4]>
# #> …
# 
# params$values[[match("situacao_proposta", params$param)]]
# #> [1] "Em Análise"    "Rejeitada"     "Aprovada"      "Em Elaboração"
# #> [5] "Inativada"

## -----------------------------------------------------------------------------
# tg_count("parcerias", "proposta", situacao_proposta = "Aprovado")
# #> Error in `tg_count()`:
# #> ! "Aprovado" is not a permitted value for `situacao_proposta`.
# #> ℹ Did you mean "Aprovada"?
# #> ℹ It accepts "Em Análise", "Rejeitada", "Aprovada", "Em Elaboração", and
# #>   "Inativada".

## -----------------------------------------------------------------------------
# tg_count("parcerias", "proposta", in_situacao_proposta = "Aprovada")
# #> Error in `tg_count()`:
# #> ! Unknown filter: "in_situacao_proposta".
# #> ✖ The API ignores a parameter it does not recognize and returns every row, so
# #>   this would look like a query that matched nothing in particular.
# #> ℹ Did you mean "situacao_proposta"?

## -----------------------------------------------------------------------------
# propostas <- tg_get("parcerias", "proposta", .limit = 5)
# 
# class(propostas$dt_proposta)
# #> [1] "Date"
# class(propostas$vl_total_planejamento_gastos)
# #> [1] "numeric"

## -----------------------------------------------------------------------------
# programas <- tg_get("parcerias", "programa", .limit = 20)
# 
# fields <- tg_fields("parcerias", "programa")
# fields$field[!is.na(fields$nested)]
# #> [1] "ufs_habilitadas"      "programa_atende_a"    "categorias_despesa"
# #> [4] "resultados_esperados" "indicadores_programa"
# 
# tg_fields("parcerias", "programa", nested = "ufs_habilitadas")
# #> # A tibble: 3 × 5
# #>   field   r_type    api_type nested description
# #>   <chr>   <chr>     <chr>    <chr>  <chr>
# #> 1 nm_uf   character string   NA     NA
# #> 2 sg_uf   character string   NA     NA
# #> 3 cd_ibge double    integer  NA     NA

## -----------------------------------------------------------------------------
# library(dplyr)
# library(tidyr)
# 
# programas |>
#   select(id_programa, ufs_habilitadas) |>
#   unnest_longer(ufs_habilitadas) |>
#   unnest_wider(ufs_habilitadas)

## -----------------------------------------------------------------------------
# tg_updated_at("parcerias")
# #> [1] "2026-08-03 UTC"

