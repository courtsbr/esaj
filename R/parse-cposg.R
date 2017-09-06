
#' Makes a parser
#' @export
make_parser <- function() {
  list(name = NULL, getter = NULL) %>% rlang::set_attrs("class" = "parser")
}

#' Parses parts
#' @param parser A parser returned by [make_parser()]
#' @export
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
  purrr::list_merge(parser, name = "parts", getter = get_parts)
}

#' Parses data
#' @param parser A parser returned by [make_parser()]
#' @export
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
        X1 = "Situa\u00E7\u00E3o",
        X2 = stringr::str_extract(.[1, 2], "[A-Za-z]+$")) %>%
      dplyr::mutate(
        X1 = str_replace_all(X1, ":", ""),
        X2 = str_replace_all(X2, " ?[\\n\\t].+", ""),
        X2 = str_replace_all(X2, "\\n", "")) %>%
      purrr::set_names("data", "value")
  }

  # Add get_data to getters
  purrr::list_merge(parser, name = "data", getter = get_data)
}

#' Parses movements
#' @param parser A parser returned by [make_parser()]
#' @export
parse_movs <- function(parser) {

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
  purrr::list_merge(parser, name = "movs", getter = get_movs)
}

#' Runs a parser
#' @param files A character vector with the paths to one ore more files
#' @param parser A parser returned by [make_parser()]
#' @param cores The number of cores to be used when parsing
#' @export
run_parser <- function(files, parser, cores = 1) {

  # Check if parser is a parser
  stopifnot(class(parser) == "parser")

  # Given a parser and a file, apply getters
  apply_getters <- function(file, parser) {
    html <- xml2::read_html(file)
    parser$getter %>%
      purrr::invoke_map(list(list(html = html))) %>%
      purrr::set_names(parser$name) %>%
      purrr::modify(list) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(file = file) %>%
      dplyr::select(file, dplyr::everything())
  }

  # Apply getters to all files
  parallel::mcmapply(
    apply_getters, files, list(parser = parser),
    SIMPLIFY = FALSE, mc.cores = cores) %>%
    dplyr::bind_rows()
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
