# a 422 surfaces the parameter the service objected to

    Code
      tg_get("parcerias", "proposta", .progress = FALSE)
    Condition
      Error in `.tg_fetch()`:
      ! The TransfereGov API returned HTTP 422 (Unprocessable Entity).
      x situacao_proposta: Input should be 'Aprovada'
      i Check the parameter names and values with `tg_params()`.

