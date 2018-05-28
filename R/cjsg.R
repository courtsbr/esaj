
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
#' @param ... Param `rapporteurs` for [download_cjsg_tjmg()]
#' @return A character vector with the paths to the downloaded files
#'
#' @seealso [cjsg_table()], [browse_table()]
#' @export
download_cjsg <- function(query, path = ".", classes = "", subjects = "",
                          courts = "", trial_start = "", trial_end = "",
                          registration_start = "", registration_end = "",
                          min_page = 1, max_page = 1, cores = 1,
                          wait = .5, tj = "tjsp", ...) {

  if (tj == "tjmg") { return(download_cjsg_tjmg(query, path, classes, subjects,
                                                courts, trial_start, trial_end,
                                                registration_start, registration_end,
                                                min_page, max_page, ...)) }



  # Convert parameters to expected format
  strings <- list(classes, subjects, courts) %>%
    purrr::modify(stringr::str_c, collapse = ",")
  dates <- list(
      trial_start, trial_end,
      registration_start, registration_end) %>%
    purrr::modify(date_pt)
  if (stringr::str_detect(query, "\"")) {
    query <- stringr::str_replace_all(query, " ", "+")
  }

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

    # Protect GET in case there are no pages
    GET <- purrr::possibly(httr::GET, "")

    # Download page
    out <- NULL; file <- stringr::str_c(path, "/page_", stringr::str_pad(page, 4, "left", "0"), ".html")
    if (!file.exists(file)) {
      out <- GET(
        "https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do",
        query = query_get, httr::config(ssl_verifypeer = FALSE),
        httr::write_disk(file, TRUE))
    }

    # Normalize path if necessary
    if (is.character(out)) { file <- out }
    else { file <- normalizePath(file) }

    return(file)
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
    `%||%`(0) %>%
    ceiling()
}

cjsg_print_npags <- function(pages, min_pag) {

  # Print message
  if (pages == 0) {
    message("There are no pages to download")
    invisible(pages)
  }
  else {
    min_p <- ifelse(min_pag == -1, 1, min_pag)
    message(
      "There are ", (pages - min_pag + 1), " pages to download\n",
      "This should take around ",
      how_long((pages - min_p + 1) * 0.5105))
    invisible(pages)
  }
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

#' Temporary function for downloading TJMG's CJSG queries
#'
#' @param query Character vector with search query
#' @param path Path to directory where to save HTMLs
#' @param classes Character vector with class IDs (e.g. `c(175, 43, 259, 263)`)
#' @param subjects Character vector with subject IDs (e.g. `c(10207, 10008, 10199)`)
#' @param courts Character vector with court IDs (e.g. `c("1-7", "1-9", "2-3", "1-1")`)
#' @param rapporteurs Character vector with rapporteur IDs (e.g. `c("2-1528561", "2-2345361")`)
#' @param trial_start Lower bound for trial date
#' @param trial_end Upper bound for trial date
#' @param registration_start Lower bound for registration date
#' @param registration_end Upper bound for registration date
#' @param min_page First page of results to download
#' @param max_page Last page of results to download. If is \code{NA} or
#'   \code{Inf}, we use \code{\link{peek_cjsg}}.
#' @return A character vector with the paths to the downloaded files
#'
download_cjsg_tjmg <- function(query, path = ".", classes = "", subjects = "",
                               courts = "", trial_start = "", trial_end = "",
                               registration_start = "", registration_end = "",
                               min_page = 1, max_page = 1, rapporteurs = "") {

  # Create directory if necessary
  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path)

  # Replicate name of item over vector
  replicate_over <- function(vec, name) {
    vec %>% as.character() %>% as.list() %>% purrr::set_names(rep(name, length(.))) }
  names <- c("listaClasse", "listaAssunto", "listaOrgaoJulgador", "listaRelator")

  # Convert dates to expected format
  dates <- list(
    trial_start, trial_end,
    registration_start, registration_end) %>%
    purrr::modify(date_pt)

  # Create part of query with lists of filters
  lists <- list(classes, subjects, courts, rapporteurs) %>%
    purrr::map2(names, replicate_over) %>%
    purrr::flatten() %>%
    purrr::discard(~.x == "")

  # Query for GET request
  query_get <- c(list(
    dataPublicacaoInicial = dates[[3]],
    dataPublicacaoFinal = dates[[4]],
    dataJulgamentoInicial = dates[[1]],
    dataJulgamentoFinal = dates[[2]],
    numeroRegistro = "1",
    totalLinhas = "1",
    palavras = query,
    pesquisarPor = "ementa",
    pesquisaTesauro = "true",
    orderByData = "1",
    linhasPorPagina = "10",
    pesquisaPalavras = "Pesquisar",
    classe = "",
    codigoAssunto = "",
    codigoOrgaoJulgador = "",
    codigoCompostoRelator = ""
  ), lists)

  # Base URL
  base <- "http://www5.tjmg.jus.br/jurisprudencia/"

  # Run search query on website's home
  u_search <- stringr::str_c(base, "pesquisaPalavrasEspelhoAcordao.do")
  r_search <- httr::GET(u_search, query = query_get)

  # Collect captcha
  v8 <- V8::v8(); captcha <- tempfile(fileext = ".jpeg")
  u_captcha <- stringr::str_c(base, "captcha.svl?", v8$eval("Math.random()*5"))
  r_captcha <- httr::GET(u_captcha, httr::write_disk(captcha, overwrite = TRUE))

  # Query for POST request
  query_post <- list(
    "callCount" = "1",
    "page" = "link_busca",
    "httpSessionId" = r_search$cookies$value[1],
    "scriptSessionId" = "",
    "c0-scriptName" = "ValidacaoCaptchaAction",
    "c0-methodName" = "isCaptchaValid",
    "c0-id" = "0",
    "c0-param0" = stringr::str_c("string:", decryptr::decrypt(captcha, "tjmg")),
    "batchId" = "0")
  file.remove(captcha)

  # Validate captcha's answer
  u_validate <- stringr::str_c(base, "dwr/call/plaincall/ValidacaoCaptchaAction.isCaptchaValid.dwr")
  r_validate <- httr::POST(u_validate, body = query_post, encode = "form")

  # Iterate over pages of results
  files <- c()
  for (i in min_page:max_page) {

    # Update page number
    query_get["paginaNumero"] = i

    # Rerun search, now with captcha broken
    file <- stringr::str_c(path, "/page_", stringr::str_pad(i, 4, "left", "0"), ".html")
    httr::GET(u_search, query = query_get, httr::write_disk(file, overwrite = TRUE))

    # Check whether should keep file
    paginator <- file %>%
      xml2::read_html() %>%
      stringr::str_detect("p?gina [0-9]* de [0-9]*")
    if (!paginator) { file.remove(file); break() }

    files <- append(files, file)
  }

  return(files)
}
