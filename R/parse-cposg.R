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






# formbody <- "//table[@id != 'secaoFormConsulta' and @class='secaoFormBody']//tr//td"
# todaspartes <- "//table[@id != 'secaoFormConsulta' and @id='tableTodasPartes']//tr//td"
# partesprincipais <- "//table[@id != 'secaoFormConsulta' and @id='tablePartesPrincipais' and @id!='tableTodasPartes']//tr//td"
#
# parse_formbody <- function(parser) {
#   purrr::list_merge(parser, name = "Form Body", xpath = formbody)
# }
#
# parse_todaspartes <- function(parser) {
#   purrr::list_merge(parser, name = "Todas Partes", xpath = todaspartes)
# }
#
# parse_partesprincipais <- function(parser) {
#   purrr::list_merge(parser, name = "Partes Principais", xpath = partesprincipais)
# }

make_parser <- function() {
  list(name = NULL, getter = NULL) %>% rlang::set_attrs("class" = "parser")
}

parse_parts <- function(parser) {

  # Check class
  stopifnot(class(parser) == "parser")

  # Function for getting parts
  get_parts <- function(html) {
    html %>%
      rvest::html_nodes(xpath = "//*[@id='tablePartesPrincipais']") %>%
      rvest::html_table(fill = TRUE) %>%
      purrr::pluck(1) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        X2 = stringr::str_split(X2, "&nbsp"),
        id = 1:nrow(.)) %>%
      tidyr::unnest(X2) %>%
      dplyr::mutate(
        part = str_replace_all(X1, "[^a-zA-Z]", ""),
        role = stringr::str_extract(dplyr::lag(X2), "\\t [a-zA-Z]+:"),
        role = str_replace_all(role, "[^a-zA-Z]", ""),
        role = ifelse(is.na(role), part, role),
        name = str_replace_all(X2, " ?\\n.+", "")) %>%
      dplyr::select(id, name, part, role)
  }

  # Add get_parts to getters
  purrr::list_merge(parser, name = "Parts", getter = get_parts)
}

parse_data <- function(parser) {

  # Check class
  stopifnot(class(parser) == "parser")

  # Function for getting data
  get_data <- function(html) {
    html %>%
      rvest::html_nodes(xpath = "//*[@class='secaoFormBody']") %>%
      rvest::html_table(fill = TRUE) %>%
      purrr::pluck(2) %>%
      dplyr::as_tibble() %>%
      dplyr::filter(!(is.na(X2) & is.na(X3))) %>%
      dplyr::select(-X3) %>%
      dplyr::add_row(
        X1 = "Situação",
        X2 = stringr::str_extract(.[1, 2], "[A-Za-z]+$")) %>%
      dplyr::mutate(
        X1 = str_replace_all(X1, ":", ""),
        X2 = str_replace_all(X2, " ?[\\n\\t].+", ""),
        X2 = str_replace_all(X2, "\\n", "")) %>%
      purrr::set_names("data", "value")
  }

  # Add get_data to getters
  purrr::list_merge(parser, name = "Data", getter = get_data)
}

parse_movements <- function(parser) {

  # Check class
  stopifnot(class(parser) == "parser")

  # Function for getting movements
  get_movs <- function(html) {
    html %>%
      rvest::html_nodes(xpath = "//*[@id='tabelaTodasMovimentacoes']") %>%
      rvest::html_table(fill = TRUE) %>%
      purrr::pluck(1) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        X1 = lubridate::dmy(X1),
        X3 = str_replace_all(X3, "[\\t\\n]", ""),
        X3 = str_replace_all(X3, "\\r", " "),
        X3 = str_replace_all(X3, " +", " ")) %>%
      dplyr::select(-X2) %>%
      purrr::set_names("movement", "description")
  }

  # Add get_movs to getters
  purrr::list_merge(parser, name = "Movements", getter = get_movs)
}

run_parser <- function(files, parser) {

  # Check if parser is a parser
  stopifnot(class(parser) == "parser")

  # Map parse_keyval over files and xpaths
  out <- purrr::map(files, function(file) {
    html <- xml2::read_html(file)
    tbls <- purrr::invoke_map(parser$getter, list(list(html = html)))
    tbls <- purrr::set_names(tbls, parser$name)
  })
  out <- purrr::set_names(out, files)

  return(out)
}

print.parser <- function(x, ...) {
  if (length(x$name) == 0) {
    cat("An empty parser\n")
  }
  else {
    cat("A parser for the following objects:\n")
    purrr::walk(x$name, ~cat("- ", .x, "\n", sep = ""))
  }
}






parser <- make_parser() %>%
  parse_data() %>%
  parse_movements() %>%
  parse_parts()


run_parser(files, parser) %>% str()















# @export
parse_cposg_um_keyval <- function(file) {
  try({

    xpath <- "//table[@id != 'secaoFormConsulta' and (@class='secaoFormBody' "
    xpath <- paste0(xpath, "or @id='tableTodasPartes' or (@id='tablePartes")
    xpath <- paste0(xpath, "Principais' and @id!='tableTodasPartes'))]//tr//td")
    #keyval <- sapply(XML::getNodeSet(h, xpath), XML::xmlValue)

    keyval <- xml2::read_html(r, encoding = "UTF-8") %>%
      rvest::html_nodes(xpath = xpath) %>%
      rvest::html_text()
    keyval <- iconv(keyval, to = 'UTF-8')
    keyval <- gsub('\u00e0s [0-9]+\\:[0-9]+', '', keyval)
    keyval <- stringr::str_trim(gsub('\\&nbsp', ' ', keyval))
    keyval <- stringr::str_trim(gsub(" +", " ", gsub("[ \t\r\n\v\f]+", " ", keyval)))
    keyval <- keyval[!duplicated(keyval, incomparables = '') | stringr::str_detect(keyval, ':[^[:alpha:]]*$')]
    keyval <- paste(keyval, collapse = ' ')

    re <- "(([[:alpha:]]+:)|(Valor da a\u00e7\u00e3o:)|(Outros assuntos:)|(Local F\u00edsico:))"
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

    d <- dplyr::tibble(key, val)
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
    d <- pesquisar_processos_2inst(processos, path)
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
