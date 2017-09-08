
#' Dowload CJSG search results
#' @param query Character vector wtih search query
#' @param file Path to file where to save search restults
#' @param classes Character vector with lawsuit class IDs
#' @param subjects Character vector with lawsuit subject IDs
#' @param courts Character vector with lawsuit court IDs
#' @param trial_start Date when the trial started
#' @param trial_end Date when the trial ended
#' @param registration_start Date when registration started
#' @param registration_end Date then registration ended
#' @param tj TJ form which to get data (only works with TJSP for now)
#' @seealso [cjsg_table()]
#' @export
download_cjsg <- function(query, file, classes = "", subjects = "",
                          courts = "", trial_start = "", trial_end = "",
                          registration_start = "", registration_end = "",
                          tj = "tjsp") {

  # Stop if TJ isn't TJSP
  stopifnot(tj == "tjsp")

  # Convert parameters to expected format
  strings <- list(classes, subjects, courts) %>%
    purrr::modify(stringr::str_c, collapse = ",")
  dates <- list(
      trial_start, trial_end,
      registration_start, registration_end) %>%
    purrr::modify(date_pt)

  # Query for POST request
  query_post <- list(
    "conversationId" = "",
    "dados.buscaInteiroTeor" = query,
    "dados.pesquisarComSinonimos" = "S",
    "contadoragente" = 0,
    "contadorMaioragente" = 0,
    "contadorjuizProlator" = 0,
    "contadorMaiorjuizProlator" = 0,
    "classesTreeSelection.values" = strings[[1]],
    "assuntosTreeSelection.values" = strings[[2]],
    "contadorcomarca" = 0,
    "contadorMaiorcomarca" = 0,
    "secoesTreeSelection.values" = strings[[3]],
    "dados.dtJulgamentoInicio" = dates[[1]],
    "dados.dtJulgamentoFim" = dates[[2]],
    "dados.dtRegistroInicio" = dates[[3]],
    "dados.dtRegistroFim" = dates[[4]],
    "dados.origensSelecionadas" = "T",
    "tipoDecisaoSelecionados" = "A",
    "dados.ordenarPor" = "dtPublicacao")

  # Execute post request
  httr::POST(
    "https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do",
    body = query_post, httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(file, TRUE))
}

#' Check certain characteristics regarding a CJSG download
#' @param ... Arguments passed on to [download_cjsg()] (except `file`)
#' @seealso [download_cjsg()], [cjsg_table()]
#' @export
peek_cjsg <- function(...) {

  # Calculate how many pages are there
  pages <- download_cjsg(..., file = tempfile()) %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//*[@id='paginacaoSuperior-A']") %>%
    rvest::html_text() %>%
    str_replace_all("[\\t\\n]", "") %>%
    stringr::str_extract_all(" [0-9]+") %>%
    purrr::pluck(1) %>%
    stringr::str_trim() %>%
    as.numeric() %>%
    magrittr::divide_by(.[1]) %>%
    purrr::pluck(2) %>%
    ceiling()

  # Print message
  message("There are ", pages, " pages to download")
  return(pages)
}

#' N??mero de p??ginas
#'
#' Calcula o n??mero de p??ginas retornadas por uma consulta de julgados de segundo grau.
#'
#' @param session sess??o da CJSG.
#' @param parms se for \code{NULL}, admite que j?? est?? na p??gina de resultados da CJSG.
#'
#' @return objeto de classe \code{session}.
#'
#' @examples
#' \dontrun{
#' library(tjsp)
#' s <- cjsg_session()
#' parms <- cjsg_parms(s, livre = 'acordam')
#' cjsg_npags(s, parms)
#' cjsg(s, parms, max_pag = 1, path = '.') # chamada internamente dentro de cjsg.
#' }
#'
#' @export
cjsg_npags <- function(session, parms = NULL) {
  if (!is.null(parms)) session <- session %>% rvest::submit_form(parms)
  num <- session$response %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node('#nomeAba-A') %>%
    rvest::html_text() %>%
    readr::parse_number()
  (num %/% 20) + 1
}
