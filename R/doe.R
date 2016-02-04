#' Baixa diários oficiais
#'
#' Acessa os Diários de Justiça Eletrônicos dos Tribunais de Justiça e baixa
#' os arquivos em PDF.
#'
#' @param tj character vector indicando o Tribunal. Atualmente funciona com
#' TJSP, TJAC, TJAL, TJAM, TJMS, TJRN, TJSC, TJCE, TJBA. Default \code{'TJSP'}.
#' @param dates Date vector ou character vector em YYYY-MM-DD com as datas
#' que se deseja baixar. Default \code{Sys.Date()}.
#' @param path pasta onde os arquivos serão gravados. Para cada data, uma pasta
#' será criada e os arquivos PDF serão salvos nessa pasta. Default \code{'data-raw/dje_pdf'}
#' @param verbose imprimir mensagens? Default \code{FALSE}.
#'
#' @return \code{tbl_df} com diagnóstico dos resultados.
#'
#' @examples
#'
#' dir.create('data-raw/dje_pdf', recursive = TRUE, showWarnings = FALSE)
#' tjsp_dje <- dje(dates = Sys.Date() - 0:3)
#' table(tjsp_dje$result)
#'
#' # --------------------------------------------------------------------------
#' tjal_dje <- dje(tj = 'TJAL', dates = Sys.Date() - 0:3)
#' tjam_dje <- dje(tj = 'TJAM', dates = Sys.Date() - 0:3)
#' tjce_dje <- dje(tj = 'TJCE', dates = Sys.Date() - 0:3)
#'
#' @export
dje <- function(tj = 'TJSP', dates = Sys.Date(), path = 'data-raw/dje_pdf',
                verbose = FALSE) {
  f <- sprintf('dje_%s', tolower(tj))
  eval(call(f, dates, path, verbose))
}

#' @rdname dje
#'
#' @param from,to Date vector ou character vector em formato YYYY-MM-DD.
#' @inheritParams dje
#'
#' @export
dje_range <- function(from, to, tj = 'TJSP', path = 'data-raw/dje_pdf',
                      verbose = FALSE) {
  dates <- seq(as.Date(from), as.Date(to), by = 1)
  dje(tj, dates, path, verbose)
}

dje_tjsp <- function(dates, path, verbose = FALSE) {
  u <- 'http://www.dje.tjsp.jus.br/cdje/downloadCaderno.do?'
  pastas <- sprintf('%s/tjsp_dje_%s', path, dates)
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  d <- expand.grid(date = dates, caderno = as.character(c(11:15, 18)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sprintf('%sdtDiario=%s&cdCaderno=%s', u, date_link, caderno),
                  arq = sprintf('%s/tjsp_dje_%s_%s.pdf', pastas, caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(download_arq(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

dje_tjac <- function(dates, path, verbose) {

}

dje_tjal <- function(dates, path, verbose) {
  u <- 'http://www2.tjal.jus.br/cdje/downloadCaderno.do?'
  pastas <- sprintf('%s/tjal_dje_%s', path, dates)
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  d <- expand.grid(date = dates, caderno = as.character(c(1:2)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sprintf('%sdtDiario=%s&cdCaderno=%s', u, date_link, caderno),
                  arq = sprintf('%s/tjal_dje_%s_%s.pdf', pastas, caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(download_arq(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)

}

dje_tjam <- function(dates, path, verbose) {
  u <- 'http://esaj.tjam.jus.br/cdje/downloadCaderno.do?'
  pastas <- sprintf('%s/tjam_dje_%s', path, dates)
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  d <- expand.grid(date = dates, caderno = as.character(c(1:3)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sprintf('%sdtDiario=%s&cdCaderno=%s', u, date_link, caderno),
                  arq = sprintf('%s/tjam_dje_%s_%s.pdf', pastas, caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(download_arq(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

dje_tjms <- function(dates, path, verbose) {

}

dje_tjrn <- function(dates, path, verbose) {

}

dje_tjsc <- function(dates, path, verbose) {

}

dje_tjce <- function(dates, path, verbose) {
  u <- 'http://esaj.tjce.jus.br/cdje/downloadCaderno.do?'
  pastas <- sprintf('%s/tjce_dje_%s', path, dates)
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  d <- expand.grid(date = dates, caderno = as.character(c(1:2)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sprintf('%sdtDiario=%s&cdCaderno=%s', u, date_link, caderno),
                  arq = sprintf('%s/tjce_dje_%s_%s.pdf', pastas, caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(download_arq(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

dje_tjba <- function(dates, path, verbose) {

}

download_arq <- function(u, a, verbose = FALSE) {
  if (file.exists(a)) {
    if (verbose) cat('\narquivo ',  a, ' ja existe!\n')
    return(dplyr::data_frame(result = 'exists'))
  }
  if (verbose) cat('\nbaixando ', a, '...', sep = '')
  res <- tryCatch({
    r <- suppressWarnings({
      httr::GET(u, httr::write_disk(a, overwrite = TRUE))
    })
    ct <- httr::headers(r)[['content-type']]
    ct <- ifelse(is.null(ct), 'application', ct)
  }, error = function(e) as.character(e))
  if (stringr::str_detect(res, 'Timeout')) {
    if (verbose) cat('ERRO!\n')
    return(dplyr::data_frame(result = 'timeout'))
  }
  if (httr::status_code(r) == 200 && stringr::str_detect(ct, 'application')) {
    if (verbose) cat('OK!\n')
    return(dplyr::data_frame(result = 'ok'))
  }
  if (verbose) cat('ERRO!\n')
  return(dplyr::data_frame(result = 'nao tem dje'))
}


