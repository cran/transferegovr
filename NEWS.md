# transferegovr 0.1.0

First release.

* Covers the three TransfereGov open data APIs — special transfers
  (`transferenciasespeciais`), fund-to-fund transfers (`fundoafundo`) and
  decentralized credit (`ted`) — and the forty-eight tables they publish.
* `tg_get()` and `tg_count()` query any table; `tg_ted()`,
  `tg_fundo_a_fundo()` and `tg_transferencias_especiais()` fix the module.
* Filters are named after the columns they apply to. A bare value means
  "equals", a bare vector means "is one of", and `tg_operators()` lists the
  fifteen comparison operators the services accept. Operators are evaluated in
  their own mask, so they work regardless of what the user has attached.
* Values carrying commas, parentheses or surrounding spaces are quoted, and
  numbers are never sent in scientific notation, so a filter matches what it
  reads as.
* `tg_modules()`, `tg_tables()` and `tg_fields()` describe the APIs offline,
  from a copy of their OpenAPI documents frozen into the package and rebuilt by
  `data-raw/schema.R`.
* Columns are typed from that schema rather than inferred, so a column that is
  entirely null on one page keeps its class on the next, and an empty result
  still carries the table's full set of columns. `bigint` columns are returned
  as double, because a value beyond `.Machine$integer.max` would become `NA` as
  an integer.
* Pagination collects as many rows as `.limit` asks for, in pages of at most
  1000 — the service's own cap, which it applies silently. Every request carries
  an explicit order, so pages cannot overlap or skip rows, and the number
  collected is checked against the total the API reports.
* `tg_metadata()` records the total the API reported, the rows and pages
  retrieved, the order and selection used, and whether the result was cached.
* Requests are throttled to sixty a minute and retried with exponential backoff
  on 429 and 5xx responses. A 400 is not retried, and its 'PostgREST' error body
  is surfaced with the offending column named.
* Responses are cached for an hour, by default in the session's temporary
  directory. `tg_cache_dir()` switches to a persistent location and
  `tg_cache_clear()` empties it.
* `tg_tables(counts = TRUE)` reports how many rows each table currently holds,
  so sizing every table is one call rather than a loop over `tg_count()`. It is
  the only part of that function that needs a network connection.
* A request whose URL grows past what the service accepts fails with a message
  naming the cause — a filter built with `in_()` over a long vector. curl
  otherwise reports "Error in the HTTP2 framing layer", which says nothing
  about the query.
* English is canonical throughout, with Portuguese aliases for the exported
  verbs. Table names, column names and categorical values stay in Portuguese
  because they are the APIs' own contract.
