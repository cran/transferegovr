# Building responses from literal JSON rather than from an R list is deliberate.
# `httr2::response_json()` round-trips through jsonlite, which turns R `NULL`
# into `{}` and so cannot express a JSON `null` -- exactly the value these APIs
# send for every empty column.

mock_json_response <- function(body,
                               status = 200L,
                               content_type = "application/json") {
  httr2::response(
    status_code = status,
    url = "https://api-publica.transferegov.gestao.gov.br/parcerias/parceria",
    headers = list(`Content-Type` = content_type),
    body = charToRaw(body)
  )
}

# One page of `n` rows of `parcerias/parceria`, with identifiers starting at
# `from`, wrapped in the envelope every table endpoint answers with.
mock_page <- function(n, from = 1L, total = n, page = 1L, page_size = 200L) {
  rows <- vapply(
    seq_len(n),
    function(i) {
      sprintf(
        paste0(
          '{"id_parceria":%d,"cd_parceria":%s,"id_proposta":%d,',
          '"in_situacao_parceria":"Aprovada",',
          '"dh_assinatura":"2025-01-0%dT12:00:00",',
          '"tx_justificativa":null,"publicacoes_parceria":[]}'
        ),
        from + i - 1L,
        # Beyond .Machine$integer.max on purpose: `cd_parceria` genuinely is,
        # and `%d` cannot render it.
        format(202500000000 + from + i - 1L, scientific = FALSE),
        from + i - 1L,
        (i %% 9L) + 1L
      )
    },
    character(1)
  )

  mock_envelope(
    paste0("[", paste(rows, collapse = ","), "]"),
    total = total, page = page, page_size = page_size
  )
}

# The paginated envelope, around whatever `data` is given as literal JSON.
mock_envelope <- function(data, total = 0L, page = 1L, page_size = 200L,
                          pages = NULL, status = 200L) {
  pages <- pages %||% ceiling(total / max(page_size, 1L))

  mock_json_response(
    sprintf(
      paste0(
        '{"data":%s,"total_pages":%d,"total_items":%d,',
        '"page_number":%d,"page_size":%d}'
      ),
      data, pages, total, page, page_size
    ),
    status = status
  )
}

# Serves the given responses in order and records the requests that asked for
# them. `env = parent.frame()` matters: `local_mocked_responses()` tears its
# mock down when the frame it was registered in exits, so registering it from
# inside this helper would unmock as soon as the helper returned.
local_recorded_requests <- function(responses, env = parent.frame()) {
  recorded <- new.env(parent = emptyenv())
  recorded$requests <- list()

  # A response is itself a list, so `is.list()` would leave a single one
  # unwrapped and serve its first element as if it were a response.
  if (inherits(responses, "httr2_response")) {
    responses <- list(responses)
  }

  index <- 0L

  httr2::local_mocked_responses(function(req) {
    index <<- index + 1L
    recorded$requests[[index]] <- req

    if (index > length(responses)) {
      testthat::fail(
        sprintf(
          "Request %d was made with only %d response(s) mocked.",
          index, length(responses)
        )
      )
    }

    responses[[index]]
  }, env = env)

  recorded
}

# The query string of a recorded request, as a named character vector.
request_query <- function(request) {
  query <- httr2::url_parse(request$url)$query

  if (is.null(query)) {
    return(character())
  }

  vapply(query, as.character, character(1))
}

# The path of a recorded request, without the leading slash.
request_path <- function(request) {
  sub("^/", "", httr2::url_parse(request$url)$path)
}

# The encoded filters a set of arguments produces, checked against the frozen
# parameters of `parcerias/parceria` unless another table is named.
filter_string <- function(..., module = "parcerias", table = "parceria") {
  unlist(
    .tg_eval_filters(rlang::quos(...), .tg_table_params(module, table)),
    use.names = TRUE
  )
}
