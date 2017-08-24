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

cposg_um <- function(p, path, ow) {
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

#' @title Download 2nd degree lawsuits
#'
#' @description  Download second degree lawsuits from TJSP.
#'
#' @param processos Lawsuit ID (only numbers)
#' @param tj TJ of the lawsuits (only works with TJSP for now)
#' @param path Path to the directory where the lawsuit should be downloaded
#' @param overwrite Whether to write over already existing HTMLs
#'
#' @export
 download_2deg_lawsuit <- function(processos, tj = "tjsp", path = 'data-raw/cposg', overwrite = FALSE) {
  stopifnot(tj == "tjsp")
  suppressWarnings(dir.create(path, recursive = TRUE))
  processos <- gsub('[^0-9]', '', processos)
  abjutils::dvec(cposg_um, processos, path = path, ow = overwrite)
}
