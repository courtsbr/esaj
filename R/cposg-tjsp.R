dados_cposg <- function(p) {
  list('conversationId' = '',
       'paginaConsulta' = '1',
       'localPesquisa.cdLocal' = '-1',
       'cbPesquisa' = 'NUMPROC',
       'tipoNuProcesso' = 'UNIFICADO',
       'numeroDigitoAnoUnificado' = stringr::str_sub(p, 1, 11),
       'foroNumeroUnificado' = stringr::str_sub(p, -4, -1),
       'dePesquisaNuUnificado' = p,
       'dePesquisaNuAntigo' = '')
}

#' Baixa um processo.
#'
#' Baixa um processo na consulta de processos de segundo grau do TJSP. Deve ser usado internamente.
#'
#' @param p n??mero do processo (string apenas com os n??meros).
#' @param path caminho da pasta onde ser?? salvo o arquivo HTML.
#' @param ow sobrescrever o arquivo HTML?
#'
cposg_um <- function(p, path, ow) {
  # Sys.sleep(1)
  arq <- sprintf('%s/%s.html', path, p)
  if (!file.exists(arq) || ow) {
    httr::GET('https://esaj.tjsp.jus.br/cposg/search.do',
              query = dados_cposg(p),
              config = httr::config(ssl_verifypeer = FALSE),
              httr::write_disk(arq, overwrite = ow))
    tibble::tibble(result = 'OK')
  } else {
    tibble::tibble(result = 'j\u00E1 existe')
  }
}

#' Baixa processos
#'
#' Baixa processos na consulta de processos de segundo grau do TJSP.
#'
#' @param processos n??mero do processo (string apenas com os n??meros).
#' @param path caminho da pasta onde os arquivos HTML ser??o salvos.
#' @param overwrite sobrescrever os arquivos HTML?
#'
#' @export
cposg <- function(processos, path = 'data-raw/cposg', overwrite = FALSE) {
  suppressWarnings(dir.create(path, recursive = TRUE))
  processos <- gsub('[^0-9]', '', processos)
  abjutils::dvec(cposg_um, processos, path = path, ow = overwrite)
}
