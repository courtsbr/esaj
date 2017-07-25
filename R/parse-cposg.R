# @export
parse_cpo_sg <- function(path, s = NULL, keyval = FALSE) {
  l <- list.files(path, full.names = TRUE)
  if(!is.null(s)) {
    l <- sample(l, s, replace = FALSE)
  }
  d <- dplyr::data_frame(l = l)
  d <- dplyr::group_by(d, l)

  if(keyval) {
    d <- dplyr::do(d, parse_cpo_sg_um_keyval(.$l))
  } else {
    d <- dplyr::do(d, parse_cpo_sg(.$l))
  }
  d <- dplyr::ungroup(d)
  d <- dplyr::select(d, -l)
  d
}

# @export
parse_cpo_sg_um_keyval <- function(r) {
  try({
    xpath <- "//table[@id != 'secaoFormConsulta' and (@class='secaoFormBody' "
    xpath <- paste0(xpath, "or @id='tableTodasPartes' or (@id='tablePartes")
    xpath <- paste0(xpath, "Principais' and @id!='tableTodasPartes'))]//tr//td")
    #keyval <- sapply(XML::getNodeSet(h, xpath), XML::xmlValue)

    keyval <- xml2::read_html(r, encoding = "UTF-8") %>%
      rvest::html_nodes(xpath = xpath) %>%
      rvest::html_text()
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

# Funcao que faz o download das informacoes de um processo de segundo
# grau (SG) no TJSP.
#
# Retorna um data.frame com os metadados basicos e andamentos do processo
#
# @export
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
      u <- build_url_cpo_sg(p, tj)
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
