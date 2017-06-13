# cpo_sg <- function(processos, path = NULL) {
#   d <- dplyr::data_frame(n_processo = unique(processos))
#   d <- dplyr::do(dplyr::group_by(d, n_processo),
#                  result = cpo_sg_um(.$n_processo, path = path))
#   d <- tidyr::unnest(d, result)
#   d <- dplyr::ungroup(d)
#   d
# }

#' Funcao que faz o download das informacoes de um processo de segundo
#' grau (SG) no TJSP.
#'
#' @export
# cpo_sg_um <- function(p, path) {
#   p <- gsub('[^0-9]', '', p)
#   if(!is.null(path) & file.exists(sprintf('%s/%s.html', path, p))) {
#     return(data.frame())
#   }
#   Sys.sleep(1)
#   u <- build_url_cpo_sg(p)
#   r <- httr::GET(u)
#   k <- TRUE
#   while (r$status_code != 200) {
#     if (k) {
#       cat("\nesperando...")
#     }
#     else {
#       cat("...")
#     }
#     Sys.sleep(2)
#     r <- httr::GET(u)
#     k <- FALSE
#   }
#   if (!k) cat("\n")
#   if(!is.null(path)) {
#     arq <- sprintf('%s/%s.html', path, p)
#     if(!file.exists(arq)) {
#       cat(httr::content(r, 'text'), file = arq)
#     }
#   }
#   return('OK')
# }

build_url_cpo_sg <- function(p, tj) {
  if (tj == 'TJSP') {
    p <- gsub("[^0-9]", "", as.character(p))
    dados_url <- list("paginaConsulta" = "1",
                      "localPesquisa.cdLocal" = "-1",
                      "cbPesquisa" = "NUMPROC",
                      "tipoNuProcesso" = "UNIFICADO",
                      "numeroDigitoAnoUnificado" = "",
                      'foroNumeroUnificado' = "",
                      'dePesquisaNuUnificado' = "")
    dados_url[["numeriDigitoAnoUnificado"]] <- stringr::str_sub(p, end = 15)
    dados_url[["foroNumeroUnificado"]] <- stringr::str_sub(p, start = 22)
    dados_url[["dePesquisaNuUnificado"]] <- p
    url1 <- "http://esaj.tjsp.jus.br/cpo/sg/search.do"
    parametros <- paste(names(dados_url), unlist(dados_url), sep = "=")
    url2 <- paste(url1, paste0(parametros, collapse = "&"), sep = "?")
  } else if (tj == 'TJAL') {
    p <- gsub("[^0-9]", "", as.character(p))
    dados_url <- list('conversationId' = '',
                      "paginaConsulta" = "1",
                      "localPesquisa.cdLocal" = "-1",
                      "cbPesquisa" = "NUMPROC",
                      "tipoNuProcesso" = "UNIFICADO",
                      "numeroDigitoAnoUnificado" = "",
                      'foroNumeroUnificado' = "",
                      'dePesquisaNuUnificado' = "",
                      'dePesquisa' = '')
    dados_url[["numeroDigitoAnoUnificado"]] <- stringr::str_sub(p, end = 15)
    dados_url[["foroNumeroUnificado"]] <- stringr::str_sub(p, start = 22)
    dados_url[["dePesquisaNuUnificado"]] <- p
    url1 <- "http://www2.tjal.jus.br/cposg5/search.do"
    parametros <- paste(names(dados_url), unlist(dados_url), sep = "=")
    url2 <- paste(url1, paste0(parametros, collapse = "&"), sep = "?")
  }
  url2
}

#' @export
decisao_cpo_sg_um <- function(h) {
  try({
    html <- rvest::html(h, encoding = 'UTF-8')
    xpath <- '(//table[@width="98%" and @align="center"])[last()]'
    r <- rvest::html_node(html, xpath = xpath)
    tab <- rvest::html_table(r)
    names(tab) <-c('data', 'situacao', 'decisao')
    return(tab)
  })
  erro <- dplyr::data_frame(data = 'erro', situacao = 'erro', decisao = 'erro')
  return(erro)
}

#' @export
decisao_cpo_sg <- function(arqs) {
  d <- dplyr::data_frame(arq = arqs)
  d <- dplyr::do(dplyr::group_by(d, arq), decisao_cpo_sg_um(.$arq))
  d <- dplyr::ungroup(d)
  d
}


