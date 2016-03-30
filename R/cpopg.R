#' Consulta processos primeiro grau TJSP
#'
#' @export
cpo_pg <- function(processos, path = "data-raw/cpo-pg", tj = 'TJSP') {
  if(tj == 'TJSC') {
    d <- pesquisar_processos(processos, path)
    return(d)
  }

  f <- dplyr::failwith(dplyr::data_frame(result = "erro"), cpo_pg_um)
  d <- dplyr::data_frame(n_processo = unique(processos))
  d <- dplyr::mutate(d, id = 1:n(), path = path, tj = tj)
  clust <- multidplyr::create_cluster(parallel::detectCores())
  d <- multidplyr::partition(d, id, n_processo, cluster = clust)
  d <- dplyr::do(d, {
    cpo_pg_um <- function(p, path, tj) {
      p <- gsub("[^0-9]", "", p)
      arq <- sprintf("%s/%s.html", path, p)
      if (!is.null(path) & file.exists(arq)) {
        return(dplyr::data_frame(result = "arquivo existe"))
      }
      # Sys.sleep(1)
      u <- esaj:::build_url_cpo_pg(p, tj)
      if (!file.exists(arq)) {
        r <- httr::GET(u, httr::config(ssl_verifypeer = FALSE),
                       httr::write_disk(arq))
      }
      k <- TRUE
      while (r$status_code != 200) {
        if (k)
          cat("\nesperando...")
        else cat("...")
        if (!file.exists(arq)) {
          r <- httr::GET(u,
                         httr::config(ssl_verifypeer = FALSE),
                         httr::write_disk(arq))
        }
        k <- FALSE
      }
      if (!k) cat("\n")
      return(dplyr::data_frame(result = "OK"))
    }
    cat(.$n_processo, '\n', file = 'data-raw/log.txt', append = TRUE)
    f <- dplyr::failwith(dplyr::data_frame(result = "erro"), cpo_pg_um)
    f(.$n_processo, path = .$path, tj = .$tj)
  })
  d <- dplyr::collect(d)
  d <- dplyr::ungroup(d)
  d
}

#' @export
build_url_cpo_pg <- function(p, tj) {
  if (tj == 'TJSP') {
    p <- gsub("[^0-9]", "", as.character(p))
    dados_url <- list(conversationId = "",
                      dadosConsulta.localPesquisa.cdLocal = "-1",
                      cbPesquisa = "NUMPROC",
                      dadosConsulta.tipoNuProcesso = "UNIFICADO",
                      numeroDigitoAnoUnificado = "",
                      foroNumeroUnificado = "",
                      dadosConsulta.valorConsultaNuUnificado = "",
                      dadosConsulta.valorConsulta = "")
    dados_url[["numeriDigitoAnoUnificado"]] <- stringr::str_sub(p, end = 15)
    dados_url[["foroNumeroUnificado"]] <- stringr::str_sub(p, start = 22)
    dados_url[["dadosConsulta.valorConsultaNuUnificado"]] <- p
    url1 <- "https://esaj.tjsp.jus.br/cpopg/search.do"
    parametros <- paste(names(dados_url), unlist(dados_url), sep = "=")
    url2 <- paste(url1, paste0(parametros, collapse = "&"), sep = "?")

  } else if (tj == 'TJAL') {
    dados_url <- list('conversationId' = '',
                      'dadosConsulta.localPesquisa.cdLocal' = '-1',
                      'cbPesquisa' = 'NUMPROC',
                      'dadosConsulta.tipoNuProcesso' = 'UNIFICADO',
                      'numeroDigitoAnoUnificado' = '',
                      'foroNumeroUnificado' = '',
                      'dadosConsulta.valorConsultaNuUnificado' = '',
                      'dadosConsulta.valorConsulta' = '')
    url1 <- 'http://www2.tjal.jus.br/cpopg/search.do'
    dados_url[["numeroDigitoAnoUnificado"]] <- stringr::str_sub(p, end = 15)
    dados_url[["foroNumeroUnificado"]] <- stringr::str_sub(p, start = 22)
    dados_url[["dadosConsulta.valorConsultaNuUnificado"]] <- p
    parametros <- paste(names(dados_url), unlist(dados_url), sep = "=")
    url2 <- paste(url1, paste0(parametros, collapse = "&"), sep = "?")
  }
  url2
}








