
#' @title Download results of a query on second degree lawsuits filed in
#'   Brazilian Justice Courts
#'
#' @description Downloads an HTML with the results obtained from querying a
#'   dataset of all second degree lawsuits and then one HTML for each page of
#'   results (at most `max_page` pages). `query` should be the string to look
#'   for in the lawsuits and `clases`, `courts`, etc. should be the filtering
#'   parameters (make sure to use [cjsg_table()] to get lists of all valid codes
#'   for these arguments).
#'
#' @param query Character vector with search query
#' @param path Path to directory where to save HTMLs
#' @param classes Character vector with class IDs (see [cjsg_table()])
#' @param subjects Character vector with subject IDs (see [cjsg_table()])
#' @param courts Character vector with court IDs (see [cjsg_table()])
#' @param trial_start Lower bound for trial date
#' @param trial_end Upper bound for trial date
#' @param registration_start Lower bound for registration date
#' @param registration_end Upper bound for registration date
#' @param min_page First page of results to download
#' @param max_page Last page of results to download. If is \code{NA} or
#'   \code{Inf}, we use \code{\link{peek_cjsg}}.
#' @param cores The number of cores to be used when downloading. If you use more
#'   than one core and is dowloading more than 15 pages, you will probably have
#'   your IP blocked.
#' @param wait Seconds to wait between downloads. Does not work properly if
#'   \code{cores} is greater than one, so you will probably have your IP blocked
#'   anyway.
#' @param tj TJ from which to get data (only works with TJSP for now)
#' @return A character vector with the paths to the downloaded files
#'
#' @seealso [cjsg_table()], [browse_table()]
#' @export
download_cjsg <- function(query, path = ".", classes = "", subjects = "",
                          courts = "", trial_start = "", trial_end = "",
                          registration_start = "", registration_end = "",
                          min_page = 1, max_page = 1, cores = 1,
                          wait = .5, tj = "tjsp") {

  # Stop
  stopifnot(tj == "tjsp")

  # Convert parameters to expected format
  strings <- list(classes, subjects, courts) %>%
    purrr::modify(stringr::str_c, collapse = ",")
  dates <- list(
      trial_start, trial_end,
      registration_start, registration_end) %>%
    purrr::modify(date_pt)

  # Query for POST request
  query_post <- list(
    "conversationId" = "",
    "dados.buscaInteiroTeor" = query,
    "dados.pesquisarComSinonimos" = "S",
    "contadoragente" = 0,
    "contadorMaioragente" = 0,
    "contadorjuizProlator" = 0,
    "contadorMaiorjuizProlator" = 0,
    "classesTreeSelection.values" = strings[[1]],
    "assuntosTreeSelection.values" = strings[[2]],
    "contadorcomarca" = 0,
    "contadorMaiorcomarca" = 0,
    "secoesTreeSelection.values" = strings[[3]],
    "dados.dtJulgamentoInicio" = dates[[1]],
    "dados.dtJulgamentoFim" = dates[[2]],
    "dados.dtRegistroInicio" = dates[[3]],
    "dados.dtRegistroFim" = dates[[4]],
    "dados.origensSelecionadas" = "T",
    "tipoDecisaoSelecionados" = "A",
    "dados.ordenarPor" = "dtPublicacao")

  # Create directory if necessary
  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path)
  file <- stringr::str_c(path, "/search.html")

  # Execute post request
  httr::POST(
    "https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do",
    body = query_post, httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(file, TRUE))

  if (is.na(max_page) || is.infinite(max_page)) {
    max_page <- cjsg_npags(dirname(file))
    cjsg_print_npags(max_page, min_page)
  }

  stopifnot(min_page <= max_page)

  # Function do download a page into a directory
  download_pages <- function(page, path, wait) {

    Sys.sleep(wait)
    # Query for GET request
    query_get <- list(
      "tipoDeDecisao" = "A",
      "pagina" = page,
      "conversationId" = "")

    # Download page
    file <- stringr::str_c(path, "/page", page, ".html")
    if (!file.exists(file)) {
      httr::GET(
        "https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do",
        query = query_get, httr::config(ssl_verifypeer = FALSE),
        httr::write_disk(file, TRUE))
    }
    return(normalizePath(file))
  }

  # Download all pages
  files <- parallel::mcmapply(
    download_pages, min_page:max_page,
    path = path, wait = wait,
    SIMPLIFY = FALSE, mc.cores = cores)
  return(c(file, purrr::flatten_chr(files)))
}

cjsg_npags <- function(path) {
  # Get number of pages
  path %>%
    list.files("search", full.names = TRUE) %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//*[@id='paginacaoSuperior-A']") %>%
    rvest::html_text() %>%
    stringr::str_extract_all(" [0-9]+") %>%
    purrr::pluck(1) %>%
    stringr::str_trim() %>%
    as.numeric() %>%
    magrittr::divide_by(.[1]) %>%
    purrr::pluck(2) %>%
    ceiling()
}

cjsg_print_npags <- function(pages, min_pag) {
  # Print message
  min_p <- ifelse(min_pag == -1, 1, min_pag)
  message(
    "There are ", (pages - min_pag + 1), " pages to download\n",
    "This should take around ",
    how_long((pages - min_p + 1) * 0.5105))
  invisible(pages)
}

#' Check how long a call to [download_cjsg()] will probably take
#' @param ... Arguments passed on to [download_cjsg()] (
#' `path` will be ignored)
#' @seealso [download_cjpg()], [cjpg_table()]
#' @export
peek_cjsg <- function(...) {

  # Special treatment to some arguments
  dots <- rlang::dots_list(...)
  path <- tempdir()
  dots$path <- path
  min_p <- dots$min_page %||% -1
  max_p <- dots$max_page %||% -1
  dots$min_page <- 1
  dots$max_page <- 1
  dots$wait <- 0

  # Call download_cjsg
  do.call(download_cjsg, dots)
  pages <- cjsg_npags(path)
  cjsg_print_npags(pages, min_p)
}
