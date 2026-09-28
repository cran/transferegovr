# the error explains why an ignored parameter is dangerous

    Code
      tg_count("parcerias", "proposta", in_situacao_proposta = "Aprovada")
    Condition
      Error in `tg_count()`:
      ! Unknown filter: "in_situacao_proposta".
      x The API ignores a parameter it does not recognize and returns every row, so this would look like a query that matched nothing in particular.
      i Did you mean "situacao_proposta"?
      i See `tg_params()` for the parameters this table accepts.
      i The packaged schema is from "2026-09-28". If the API has gained a parameter since, set `options(transferegovr.validate = FALSE)`.

# a value outside the enumeration is rejected with the list

    Code
      tg_count("parcerias", "proposta", situacao_proposta = "Aprovado")
    Condition
      Error in `tg_count()`:
      ! "Aprovado" is not a permitted value for `situacao_proposta`.
      i Did you mean "Aprovada"?
      i It accepts "Em Análise", "Rejeitada", "Aprovada", "Em Elaboração", and "Inativada".

# several values for one parameter are refused

    Code
      tg_count("parcerias", "proposta", sg_uf_recebedor = c("PE", "PB"))
    Condition
      Error in `tg_count()`:
      ! Filter `sg_uf_recebedor` has 2 values, and the API accepts one.
      i Query each value and bind the results, for example `purrr::list_rbind(lapply(values, function(v) tg_get(module, table, sg_uf_recebedor = v)))`.
      i Only the parameters `tg_params()` marks as multiple take several values in one request.

