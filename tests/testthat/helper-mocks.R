# Building responses from literal JSON rather than from an R list is deliberate.
# `httr2::response_json()` round-trips through jsonlite, which turns R `NULL`
# into `{}` and so cannot express a JSON `null` -- exactly the value these APIs
# send for every empty column.

mock_json_response <- function(body,
                               status = 200L,
                               content_range = NULL,
                               content_type = "application/json") {
  headers <- list(`Content-Type` = content_type)

  if (!is.null(content_range)) {
    headers[["Content-Range"]] <- content_range
  }

  httr2::response(
    status_code = status,
    url = "https://api.transferegov.gestao.gov.br/ted/plano_acao",
    headers = headers,
    body = charToRaw(body)
  )
}

# One page of `n` rows of `ted/plano_acao`, with identifiers starting at `from`.
mock_page <- function(n, from = 1L, total = NULL, first = from - 1L) {
  rows <- vapply(
    seq_len(n),
    function(i) {
      sprintf(
        paste0(
          '{"id_plano_acao":%d,"aa_ano_plano_acao":2024,',
          '"vl_total_plano_acao":%d.5,"dt_inicio_vigencia":"2024-01-0%d",',
          '"in_forma_execucao_direta":true,"tx_objeto_plano_acao":"row %d"}'
        ),
        from + i - 1L, (from + i - 1L) * 10L, (i %% 9L) + 1L, from + i - 1L
      )
    },
    character(1)
  )

  range <- if (is.null(total)) {
    sprintf("%d-%d/*", first, first + n - 1L)
  } else {
    sprintf("%d-%d/%d", first, first + n - 1L, total)
  }

  mock_json_response(
    paste0("[", paste(rows, collapse = ","), "]"),
    content_range = range
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

# The query string of a recorded request, as a named character vector. Repeated
# names are kept, because two conditions on one column are two parameters.
request_query <- function(request) {
  query <- httr2::url_parse(request$url)$query

  if (is.null(query)) {
    return(character())
  }

  vapply(query, as.character, character(1))
}

filter_string <- function(...) {
  unlist(.tg_eval_filters(rlang::quos(...)), use.names = TRUE)
}
