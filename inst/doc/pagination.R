## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", eval = FALSE)

## -----------------------------------------------------------------------------
# library(transferegovr)

## -----------------------------------------------------------------------------
# sizes <- tg_tables(counts = TRUE)
# sizes[order(-sizes$rows), c("module", "table", "columns", "rows")]
# #> # A tibble: 74 × 4
# #>    module      table                                   columns    rows
# #>    <chr>       <chr>                                     <int>   <dbl>
# #>  1 parcerias   extrato_bancario                             13 1362980
# #>  2 fundoafundo gestao_financeira_lancamentos                28 1160094
# #>  3 especiais   gestao_financeira_lancamentos_especiais      34  735012
# #>  4 especiais   planos_trabalho_historico                     5  461164
# #>  5 parcerias   item_proposta                                14  428078
# #>  …

## -----------------------------------------------------------------------------
# tg_count("fundoafundo", "gestao_financeira_lancamentos")
# #> [1] 1160094

## -----------------------------------------------------------------------------
# tg_count("parcerias", "proposta")
# #> [1] 89415
# tg_count("parcerias", "proposta", sg_uf_recebedor = "PE")
# #> [1] 3258

## -----------------------------------------------------------------------------
# tg_get("parcerias", "proposta", .page_size = 201)
# #> Error in `tg_get()`:
# #> ! `.page_size` must be a whole number between 1 and 200.

## -----------------------------------------------------------------------------
# rows <- tg_count("parcerias", "extrato_bancario")
# ceiling(rows / 200)
# #> [1] 6815

## -----------------------------------------------------------------------------
# options(transferegovr.requests_per_minute = 120)

## -----------------------------------------------------------------------------
# tg_get("especiais", "meta_especiais", .limit = 450)

## -----------------------------------------------------------------------------
# tg_get("especiais", "meta_especiais", .limit = 100, .offset = 137,
#        .page_size = 60)

## -----------------------------------------------------------------------------
# programas <- tg_get("especiais", "programas_especiais", .limit = Inf)

## -----------------------------------------------------------------------------
# metas <- tg_get("especiais", "meta_especiais", .limit = 450)
# 
# tg_metadata(metas)
# #> $module
# #> [1] "especiais"
# #> $table
# #> [1] "meta_especiais"
# #> $total_rows
# #> [1] 156193
# #> $rows_returned
# #> [1] 450
# #> $pages
# #> [1] 3
# #> …

## -----------------------------------------------------------------------------
# strip <- function(x) {
#   x <- as.data.frame(x)
#   attr(x, "transferegovr_metadata") <- NULL
#   rownames(x) <- NULL
#   x
# }
# 
# big <- tg_get("especiais", "meta_especiais", .limit = 450, .page_size = 200)
# small <- tg_get("especiais", "meta_especiais", .limit = 450, .page_size = 50)
# 
# identical(strip(big), strip(small))
# #> [1] TRUE

## -----------------------------------------------------------------------------
# first <- tg_get("especiais", "meta_especiais", .limit = 450)
# again <- tg_get("especiais", "meta_especiais", .limit = 450)
# 
# tg_metadata(again)$cached
# #> [1] TRUE

## -----------------------------------------------------------------------------
# tg_cache_dir(tools::R_user_dir("transferegovr", "cache"))

## -----------------------------------------------------------------------------
# tg_updated_at("fundoafundo")
# #> [1] "2026-09-28 06:02:02 UTC"

## -----------------------------------------------------------------------------
# library(purrr)
# 
# total <- tg_count("fundoafundo", "gestao_financeira_lancamentos")
# slice_size <- 20000
# starts <- seq(0, total - 1, by = slice_size)
# 
# walk(starts, function(start) {
#   file <- sprintf("lancamentos-%08d.rds", start)
#   if (file.exists(file)) return(invisible(NULL))
# 
#   rows <- tg_get(
#     "fundoafundo", "gestao_financeira_lancamentos",
#     .limit = slice_size, .offset = start
#   )
#   saveRDS(rows, file)
# })

