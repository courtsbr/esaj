#' @export
parse_cpo_sg <- function(path, sample = NULL, keyval = FALSE) {
  l <- list.files(path, full.names = TRUE)
  if(!is.null(sample)) {
    l <- sample(l, sample, replace = FALSE)
  }
  d <- dplyr::data_frame(l = l)
  d <- dplyr::group_by(d, l)

  if(keyval) {
    d <- dplyr::do(d, parse_cpo_sg_um_keyval(.$l))
  } else {
    d <- dplyr::do(d, parse_cpo_sg_um(.$l))
  }
  d <- dplyr::ungroup(d)
  d <- dplyr::select(d, -l)
  d
}

#' @export
parse_cpo_sg_um_keyval <- function(r) {
  try({
    h <- rvest::html(r, encoding = "UTF-8")
    xpath <- "//table[@id != 'secaoFormConsulta' and (@class='secaoFormBody' "
    xpath <- paste0(xpath, "or @id='tableTodasPartes' or (@id='tablePartes")
    xpath <- paste0(xpath, "Principais' and @id!='tableTodasPartes'))]//tr//td")
    keyval <- sapply(XML::getNodeSet(h, xpath), XML::xmlValue)
    keyval <- iconv(keyval, to = 'UTF-8')
    keyval <- gsub('às [0-9]+\\:[0-9]+', '', keyval)
    keyval <- stringr::str_trim(gsub('\\&nbsp', ' ', keyval))
    keyval <- stringr::str_trim(gsub(" +", " ", gsub("[ \t\r\n\v\f]+", " ", keyval)))
    keyval <- keyval[!duplicated(keyval, incomparables = '') | stringr::str_detect(keyval, ':[^[:alpha:]]*$')]
    keyval <- paste(keyval, collapse = ' ')

    re <- '(([[:alpha:]]+:)|(Valor da ação:)|(Outros assuntos:)|(Local Físico:))'
    key <- stringr::str_match_all(keyval, re)[[1]][, 2]
    key <- stringr::str_trim(gsub(':', '', key))
    key <- rm_accent(gsub(' +', '_', tolower(key)))
    #key[key %in% c('reqte', 'reclamante')] <- 'reqte'
    #key[key %in% c('reqda', 'reclamada', 'reclamado')] <- 'reqdo'
    #key[key == 'advogada'] <- 'advogado'

    val <- stringr::str_split(keyval, re)[[1]][-1]
    val <- stringr::str_trim(gsub('^[^A-Za-z0-9]+|[^A-Za-z0-9]+$', '', val))

    #     if(any(stringr::str_detect(key, 'reqte')) &
    #        any(stringr::str_detect(key, 'reqdo')) &
    #        any(stringr::str_detect(key, 'adv'))) {
    #       ind <- 1:length(key) %in% (which(key == 'reqte')[1] + 1):(which(key == 'reqdo')[1] - 1)
    #       ind <- ind & (key == 'advogado')
    #       key[ind] <- 'reqte_adv'
    #       val[ind] <- paste(val[ind], collapse = '\n')
    #       val[key == 'advogado'] <- paste(val[key == 'advogado'], collapse = '\n')
    #       key[key == 'advogado'] <- 'reqdo_adv'
    #     }

    d <- data.frame(key, val, stringsAsFactors = FALSE)
    if(is.character(r)) {
      d$arq <- r
    }
    return(d)
  })
  d <- data.frame(arq = r, key = 'erro', val = 'erro', stringsAsFactors = FALSE)
  return(d)
}

#' Funcao que faz o download das informacoes de um processo de segundo
#' grau (SG) no TJSP.
#'
#' Retorna um data.frame com os metadados basicos e andamentos do processo
#'
#' @export
cpo_sg <- function(processos, path = "data-raw/cpo-sg", tj = 'TJSP') {
  if(tj == 'TJSC') {
    d <- pesquisar_processos(processos, path)
    return(d)
  }
  # f <- dplyr::fail\with(dplyr::data_frame(result = "erro"), cpo_pg_um)
  d <- dplyr::data_frame(n_processo = unique(processos))
  d <- dplyr::mutate(d, id = 1:n(), path = path, tj = tj)
  clust <- multidplyr::create_cluster(parallel::detectCores())
  d <- multidplyr::partition(d, id, n_processo, cluster = clust)
  d <- dplyr::do(d, {
    cpo_sg_um <- function(p, path, tj) {
      p <- gsub("[^0-9]", "", p)
      arq <- sprintf("%s/%s.html", path, p)
      if (!is.null(path) & file.exists(arq)) {
        return(dplyr::data_frame(result = "arquivo existe"))
      }
      # Sys.sleep(1)
      u <- esaj:::build_url_cpo_sg(p, tj)
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
    # cat(.$id, '\n', file = 'data-raw/log.txt', append = TRUE)
    f <- dplyr::failwith(dplyr::data_frame(result = "erro"), cpo_sg_um)
    cpo_sg_um(.$n_processo, path = .$path, tj = .$tj)
  })
  d <- dplyr::collect(d)
  d <- dplyr::ungroup(d)
  d
}


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


