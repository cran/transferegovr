# transferegovr 0.2.0

The package now targets the public API host,
`api-publica.transferegov.gestao.gov.br`. That host serves a different kind of
service from the 'PostgREST' one the package was built against, so this release
rewrites the client rather than extending it. Code written against 0.1.0 will
need changing.

## What is covered

* **`parcerias` is new**: partnership management, 17 tables, including about
  89,000 partnerships and their proposals, budget commitments, payment orders
  and bank statements. It has no equivalent in the previous release.
* **`especiais` replaces `transferenciasespeciais`**, and grows from 14 tables
  to 23. The additions are the financial ones: transaction entries, sub-entries,
  account balances, beneficiaries, returned funds, management report analyses
  and settlement documents, and three history tables.
* **`fundoafundo` stays**, at 20 tables against the previous 21, with several
  child tables folded into their parents as nested columns.
* **`ted` stays**, moved to the public host with the others: 14 tables against
  the previous 13. Its table and column names follow the new service, which
  mostly means plurals — `evento` is now `eventos`, `plano_acao` is
  `planos_acao`, and `trf` is `programacoes_financeiras_trf` — so code written
  against 0.1.0 needs its table names checked. `tg_ted()` is kept.
* `"transferenciasespeciais"` still resolves, as an alias for `"especiais"`.
  `tg_transferencias_especiais()` is removed, and `tg_parcerias()` and
  `tg_especiais()` join `tg_fundo_a_fundo()` and `tg_ted()`.

74 tables and 1,045 columns in all, against 48 and 599.

## Filters

* Filters are now the endpoints' own typed query parameters rather than
  'PostgREST' operators. `tg_params()` lists what each table accepts, with the
  permitted values of the enumerated ones.
* **The comparison operators are removed** — `eq()`, `neq()`, `gt()`, `gte()`,
  `lt()`, `lte()`, `like()`, `ilike()`, `re_match()`, `re_imatch()`, `in_()`,
  `is_null()`, `is_true()`, `is_false()`, `not()` and `tg_operators()`. These
  services compare for equality, with "is one of" only on the identifiers
  described below.
* **An unknown parameter name is an error.** These services ignore a parameter
  they do not recognize and answer `200` with the whole table, so a typo would
  return a plausible, unfiltered result. Names are checked against the frozen
  schema before the request is made, and a near miss is suggested.
* Enumerated values are checked client-side too, so a bad value fails before the
  round trip rather than as a 422 after it.
* **Some identifier parameters take several values**, sent as one
  comma-separated value and matching any of them: 113 of them, in all four
  modules. `tg_params()` marks them as `multiple` and gives the most each
  accepts in `max_values` — 100 in `especiais`, 200 elsewhere. The OpenAPI
  documents do not say which parameters these are, so the schema builder asks
  the service.
* Any other filter with several values, and any parameter given twice, is
  refused. The service keeps the last occurrence of a repeated parameter and
  discards the rest without reporting it.

## Pagination

* Pagination is by page number, and the cap on rows per request is the one each
  module declares: 200 for `especiais` and `parcerias`, 1000 for `fundoafundo`
  and `ted`. `.page_size` defaults to that cap, and `tg_modules()` reports it.
  `.limit` still counts rows and `.offset` still counts rows, including when the
  offset falls inside a page.
* **`.order` and `.select` are removed.** These APIs publish no ordering or
  column-selection parameter.
* Row order is therefore the server's. It was verified rather than assumed:
  the same rows come back in the same sequence across page sizes, across
  repeated calls, 100,000 rows deep, on tables with no key, and on tables with
  nested columns.

## Other changes

* `tg_updated_at()` reports when a module's data was last loaded, from the
  `/data-atualizacao` endpoint each module publishes. It is the only freshness
  signal these APIs give.
* `tg_fields()` gains a `nested` argument, describing the columns of the objects
  inside a list column. 22 columns across `fundoafundo`, `parcerias` and `ted`
  arrive as list columns because the API folds a child table into its parent.
* `tg_fields()` reports `api_type` rather than `pg_type`, and no longer reports
  a primary key: these documents declare none.
* `tg_tables()` gains `path`, the endpoint a table maps to, and `params`, how
  many filters it accepts. A table may be named with either a hyphen or an
  underscore.
* Integer columns are returned as double. These documents declare no `format`,
  so int32 and int64 cannot be told apart, and identifiers here genuinely exceed
  `.Machine$integer.max` — `cd_parceria` reaches 202500037062.
* HTTP errors surface the validation detail the service reports, naming the
  parameter it objected to.

# transferegovr 0.1.0

First release.

* Covered the three 'PostgREST' TransfereGov open data APIs — special transfers
  (`transferenciasespeciais`), fund-to-fund transfers (`fundoafundo`) and
  decentralized credit (`ted`) — and the forty-eight tables they published.
* `tg_get()` and `tg_count()` queried any table; `tg_ted()`,
  `tg_fundo_a_fundo()` and `tg_transferencias_especiais()` fixed the module.
* Filters were named after the columns they applied to. A bare value meant
  "equals", a bare vector meant "is one of", and `tg_operators()` listed the
  fifteen comparison operators the services accepted.
* `tg_modules()`, `tg_tables()` and `tg_fields()` described the APIs offline,
  from a copy of their OpenAPI documents frozen into the package.
* Columns were typed from that schema rather than inferred.
* Pagination collected as many rows as `.limit` asked for, in pages of at most
  1000 — the service's own cap, which it applied silently. Every request carried
  an explicit order, so pages could not overlap or skip rows.
* Requests were throttled to sixty a minute and retried with exponential backoff
  on 429 and 5xx responses.
* Responses were cached for an hour, by default in the session's temporary
  directory.
* English was canonical throughout, with Portuguese aliases for the exported
  verbs.
