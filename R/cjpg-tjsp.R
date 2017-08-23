#' Sess??o da CJPG
#'
#' Abre uma sess??o da Consulta de Julgados de Primeiro Grau.
#'
#' @return objeto de classe \code{session}.
#'
#' @export
cjpg_session <- function() {
  u <- 'https://esaj.tjsp.jus.br/cjpg/'
  httr::handle_reset(u)
  rvest::html_session(u, httr::config(ssl_verifypeer = FALSE))
}

#' N??mero de p??ginas
#'
#' Calcula o n??mero de p??ginas retornadas por uma consulta de julgados de primeiro grau.
#'
#' @param session sess??o da CJPG.
#' @param parms se for \code{NULL}, admite que j?? est?? na p??gina de resultados da CJPG.
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
cjpg_npags <- function(session, parms = NULL) {
  if (!is.null(parms)) session <- rvest::submit_form(session, parms)
  cpath <- '#resultados > table:nth-child(1) > tr:nth-child(1) > td:nth-child(1)'
  num <- session$response %>%
    httr::content('text') %>%
    xml2::read_html(encoding = 'latin1') %>%
    rvest::html_node(cpath) %>%
    rvest::html_text() %>%
    stringr::str_extract('de [0-9]+') %>%
    tidyr::extract_numeric()
  (num %/% 10) + 1
}

#' Par??metros de pesquisa
#'
#' Inclui par??metros de pesquisa da CJPG.
#'
#' O intervalo de datas, caso seja inclu??do, precisa ser de no m??ximo um ano.
#'
#' @param session sess??o da CJPG.
#' @param livre string com pesquisa livre por palavras-chave.
#' @param data_inicial data inicial da decis??o em formado \%Y-\%m-\%d ou objeto Date.
#' @param data_final data final da decis??o em formado \%Y-\%m-\%d ou objeto Date.
#' @param secoes sess??es
#' @param classes vetor com as classes que se deseja pesquisar. Obter o vetor de \code{cjpg_classes}.
#' @param assuntos vetor com os assuntos que se deseja pesquisar. Obter o vetor de \code{cjpg_assuntos}.
#' @param magistrados vetor com os magistrados que se deseja pesquisar. NOT IMPLEMENTED YET.
#' @param varas vetor com as varas que se deseja pesquisar. Obter o vetor de \code{cjpg_varas}.
#'
#' @return objeto de classe \code{form}.
#'
#' @examples
#' \dontrun{
#' library(tjsp)
#' s <- cjpg_session()
#' parms <- cjsg_parms(
#'   s, livre = 'sentenca', data_inicial = '2015-01-01',
#'   data_final = '2015-05-01')
#' parms
#' cjpg(s, parms, max_pag = 1, path = '.')
#' }
#' @export
cjpg_parms <- function(session, livre = '', data_inicial = NULL, data_final = NULL, secoes = '',
                       classes = '', assuntos = '', magistrados = '', varas = '') {
  if (magistrados != '') stop('magistrados NOT IMPLEMENTED YET.')

  classes <- paste(classes, collapse = ',')
  assuntos <- paste(assuntos, collapse = ',')
  varas <- paste(varas, collapse = ',')
  dt_inicial <- ''
  if (!is.null(data_inicial)) {
    data_inicial <- lubridate::ymd(data_inicial)
    dt_inicial <- sprintf('%02d/%02d/%d', lubridate::day(data_inicial),
                          lubridate::month(data_inicial),
                          lubridate::year(data_inicial))
  }
  dt_final <- ''
  if (!is.null(data_final)) {
    data_final <- lubridate::ymd(data_final)
    dt_final <- sprintf('%02d/%02d/%d', lubridate::day(data_final),
                        lubridate::month(data_final),
                        lubridate::year(data_final))
  }
  suppressWarnings({
    session %>%
      rvest::html_form() %>%
      dplyr::first() %>%
      rvest::set_values('dadosConsulta.pesquisaLivre' = livre,
                        'classeTreeSelection.values' = classes,
                        'assuntoTreeSelection.values' = assuntos,
                        'varasTreeSelection.values' = varas,
                        'dadosConsulta.dtInicio' = dt_inicial,
                        'dadosConsulta.dtFim' = dt_final)
  })
}

#' Baixa uma p??gina.
#'
#' Baixa uma p??gina a partir de um resultado da CJPG. N??o deve ser usado diretamente.
#'
#' @param pag n??mero da p??gina a ser baixada.
#' @param path pasta em que o arquivo ser?? salvo.
#' @param ow logical, sobrescrever arquivo?
#' @param s sessao da CJSG.
#'
cjpg_pag <- function(pag, path, ow, s) {
  # Sys.sleep(1) # precisa esperar pois o servidor bloqueia IP.
  u <- 'https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=%d&conversationId='
  u_pag <- sprintf(u, pag)
  arq <- sprintf('%s/%05d.html', path, pag)
  if (!file.exists(arq) || ow) {
    httr::GET(u_pag, httr::write_disk(arq, overwrite = ow),
              handle = s$handle,
              httr::config(ssl_verifypeer = FALSE))
    tibble::data_frame(result = 'OK')
  } else {
    tibble::data_frame(result = 'j\u00E1 existe')
  }
}

#' Consulta de Julgados de Primeiro Grau
#'
#' Baixa arquivos HTML correspondentes a uma busca na CJPG.
#'
#' @param session sess??o da CJPG, retornado pela fun????o \code{\link{cjpg_session}}.
#' @param parms form com par??metros de busca, retornado pela fun????o \code{\link{cjpg_parms}}.
#' @param path caminho onde os arquivos HTML ser??o baixados. Tentar?? criar a pasta automaticamente.
#' @param max_pag n??mero m??ximo de p??ginas a serem baixadas. Se \code{NA} ou \code{Inf}, baixar?? todas as p??ginas resultantes da pesquisa.
#' @param overwrite logical: sobrescrever arquivos salvos?
#' @param verbose logical: imprimir mensagens de acompanhamento do download?
#' @param p probabilidade de imprimir uma mensagem de acompanhamento se \code{verbose} for \code{TRUE}. Default 5\%.
#'
#' @export
cjpg <- function(session, parms, path = 'data-raw/cjpg',
                 max_pag = 10, overwrite = FALSE,
                 verbose = TRUE, p = .05) {
  suppressWarnings(dir.create(path, recursive = TRUE))
  if (!file.exists(path)) stop
  suppressWarnings({
    r0 <- rvest::submit_form(session, parms, submit = 'pbSubmit',
                             httr::config(ssl_verifypeer = FALSE))
  })
  n_pags <- cjpg_npags(r0)
  if(is.na(n_pags)){return(tibble::data_frame(result = 'Busca sem resultados'))}
  n_pags <- if (is.na(max_pag) || is.infinite(max_pag) || max_pag > n_pags) n_pags else max_pag
  abjutils::dvec(cjpg_pag, 1:n_pags, path = path, ow = overwrite, s = session)
}
