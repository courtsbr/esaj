
#' @title Download results of a query on first degree lawsuits filed
#' in Brazilian Justice Courts
#'
#' @description Downloads an HTML with the results obtained from
#' querying a dataset of all first degree lawsuits and then one
#' HTML for each page of results (at most `max_page` pages). `query`
#' should be the string to look for in the lawsuits and `clases`,
#' `courts`, etc. should be the filtering parameters (make sure
#' to use [cjpg_table()] to get lists of all valid codes for these
#' arguments).
#'
#' @param query Character vector with search query
#' @param path Path to directory where to save HTMLs
#' @param classes Character vector with class IDs (see [cjpg_table()])
#' @param subjects Character vector with subject IDs (see [cjpg_table()])
#' @param courts Character vector with court IDs (see [cjpg_table()])
#' @param date_start Lower bound for date
#' @param date_end Upper bound for date
#' @param min_page First page of results to download
#' @param max_page Last page of results to download
#' @param cores The number of cores to be used when downloading
#' @param tj TJ from which to get data (only works with TJSP for now)
#' @return A character vector with the paths to the downloaded files
#'
#' @seealso [cjpg_table()], [browse_table()]
#' @export
download_cjpg <- function(query, path = ".", classes = "", subjects = "",
                          courts = "", date_start = "", date_end = "",
                          min_page = 1, max_page = 1, cores = 1, tj = "tjsp") {

  # Stop
  stopifnot(tj == "tjsp")
  stopifnot(min_page <= max_page)

  # Convert parameters to expected format
  strings <- list(classes, subjects, courts) %>%
    purrr::modify(stringr::str_c, collapse = ",")
  dates <- list(date_start, date_end) %>%
    purrr::modify(date_pt)

  # Those lines are no longer necessary, although the original requisition uses the '+'
  # if (stringr::str_detect(query, "\"")) {
  #   query <- stringr::str_replace_all(query, " ", "+")
  # }

  # Query for POST request
  query_post <- list(
    "conversationId" = "",
    "dadosConsulta.pesquisaLivre" = query,
    "tipoNumero" = "UNIFICADO",
    "classeTreeSelection.values" = strings[[1]],
    "assuntoTreeSelection.values" = strings[[2]],
    "contadoragente" = 0,
    "contadorMaioragente" = 0,
    "dadosConsulta.dtInicio" = dates[[1]],
    "dadosConsulta.dtFim" = dates[[2]],
    "varasTreeSelection.values" = strings[[3]],
    "dadosConsulta.ordenacao" = "DESC")

  # Create directory if necessary
  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path)
  file <- stringr::str_c(path, "/search.html")

  # Execute post request
  httr::POST(
    "https://esaj.tjsp.jus.br/cjpg/pesquisar.do",
    body = query_post, httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(file, TRUE))

  # Function do download a page into a directory
  download_pages <- function(page, path) {

    # Query for GET request
    query_get <- list(
      "pagina" = page,
      "conversationId" = "")

    # Protect GET in case there are no pages
    GET <- purrr::possibly(httr::GET, "")

    # Download page
    file <- stringr::str_c(path, "/page_", stringr::str_pad(page, 4, "left", "0"), ".html")
    out <- GET(
      "https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do",
      query = query_get, httr::config(ssl_verifypeer = FALSE),
      httr::write_disk(file, TRUE))

    # Normalize path if necessary
    if (is.character(out)) { file <- out }
    else { file <- normalizePath(file) }

    return(file)
  }

  # Download all pages
  files <- parallel::mcmapply(
    download_pages, min_page:max_page, list(path = path),
    SIMPLIFY = FALSE, mc.cores = cores)
  return(c(file, purrr::flatten_chr(files)))
}

  #' Check how long a call to [download_cjpg()] will probably take
#' @param ... Arguments passed on to [download_cjpg()] (
#' `path` will be ignored)
#' @seealso [download_cjpg()], [cjpg_table()]
#' @export
peek_cjpg <- function(...) {

  # Special treatment to some arguments
  dots <- rlang::dots_list(...)
  path <- tempdir()
  dots$path <- path
  min_p <- dots$min_page
  max_p <- dots$max_page
  dots$min_page <- 1
  dots$max_page <- 1

  # Call download_cjpg
  do.call(download_cjpg, dots)

  # Fix pages
  dots$min_page <- min_p %||% 1
  dots$max_page <- max_p %||% 1

  # Get number of pages
  pages <- path %>%
    list.files("search", full.names = TRUE) %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//*[@id='resultados']/table[1]") %>%
    rvest::html_text() %>%
    stringr::str_extract_all(" [0-9]+") %>%
    purrr::pluck(1) %>%
    stringr::str_trim() %>%
    as.numeric()
  n_pages <- pages %>%
    magrittr::divide_by(.[1]) %>%
    purrr::pluck(2) %>%
    `%||%`(0) %>%
    ceiling()

  # Print message
  if (n_pages == 0) {
    message("There are no pages to download")
    invisible(pages)
  }
  else {
    dots$max_page <- min(dots$max_page, n_pages)
    n_pages <- dots$max_page - dots$min_page + 1

    if (n_pages > 1000) {
      message(
        "There are ",
        pages[1]*n_pages, " lawsuits to download ",
        "(for a total of ", n_pages, " pages)\n",
        "This should take around ",
        how_long(n_pages*1.3988),
        "\nNote that this estimate is only ok for less than 1000 pages")
    }
    else {
      message(
        "There are ",
        pages[2], " lawsuits to download ",
        "(for a total of ", n_pages, " pages)\n",
        "This should take around ",
        how_long(n_pages*1.3988))
    }

    invisible(pages)
  }
}
