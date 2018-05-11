
#' Parse one lawsuit from a CJSG page
#'
#' @param node A `.fundocinza1` node extracted from the page
#' @return One row with the data concerning the lawsuit
parse_cjpg_lawsuit <- function(node) {

  # Get complicated variables
  cd <- node %>%
    xml2::xml_find_first(".//a[@title='Visualizar Inteiro Teor']") %>%
    rvest::html_attr("name") %>%
    stringr::str_trim()
  id <- node %>%
    xml2::xml_find_first(".//a[@title='Visualizar Inteiro Teor']") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[^0-9]", "")
  tx <- node %>%
    rvest::html_node(xpath = ".//table//div[@style='display: none;']") %>%
    rvest::html_text() %>%
    stringr::str_trim()

  # Create table prototype
  keys <- node %>%
    rvest::html_nodes("table > tr > td > strong") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    abjutils::rm_accent() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[^0-9a-z]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
  vals <- node %>%
    xml2::xml_find_all(".//table/tr/td/strong/following-sibling::text()[1]") %>%
    rvest::html_text() %>%
    stringr::str_trim()
  infos <- tibble::tibble(key = keys, val = vals) %>%
    dplyr::mutate(id = 1) %>%
    tidyr::spread(key, val) %>%
    dplyr::select(-id)

  # Build final table
  tibble::tibble(id_lawsuit = id, code_lawsuit = cd) %>%
    dplyr::bind_cols(infos) %>%
    dplyr::mutate(summary = tx) %>%
    dplyr::rename(
      subject = assunto, class = classe, district = comarca,
      date_available = data_de_disponibilizacao, jurisdiction = foro,
      judge = magistrado, court = vara)
}

#' Parse a page of CJPG results
#'
#' @param file The path to the file to be parsed
#' @param pb Progress bar created by [parse_cjpg()]
#' @return A tibble with the parsed information
parse_cjpg_ <- function(file, pb = NULL) {

  # Safely parse everything
  parse <- purrr::possibly(parse_cjpg_lawsuit, tibble::tibble(), quiet = FALSE)

  # Iterate over xml nodes to parse every lawsuit
  table <- file %>%
    xml2::read_html("UTF-8") %>%
    rvest::html_nodes(".fundocinza1") %>%
    purrr::map_dfr(parse)

  if (!is.null(pb)) { pb$tick() }
  return(table)
}

#' Parse lawsuits extracted from CJPG query
#'
#' @param file Character vector with the paths to one or more files
#' @param cores Number of cores to use when parsing
#'
#' @return A tibble with the columns
#' \itemize{
#'   \item `file` Name of the file
#'   \item `id_lawsuit` Number of the lawsuit (doesn't have to be unique)
#'   \item `code_lawsuit` Unique code of the lawsuit
#'   \item `subject` Subject of the lawsuit
#'   \item `class` Class of the subject
#'   \item `district` Name of the district
#'   \item `date_available` Date when lawsuit was made available (\%d/\%m/\%Y)
#'   \item `jurisdiction` Name of the jurisdiction
#'   \item `judge` Name of the judge
#'   \item `court` Body responsible for the lawsuit
#'   \item `summary` Summary of the ruling
#' }
#' @export
parse_cjpg <- function(file, cores = 1) {

  # Set names for .id
  names(file) <- file
  file <- file[file.size(file) > 0]

  # Run either with progress bar or on parallel
  if (cores == 1) {
    pb <- progress::progress_bar$new(total = length(file))
    purrr::map_dfr(file, parse_cjpg_, pb, .id = "file")
  } else {
    file %>%
      parallel::mclapply(parse_cjpg_, mc.cores = cores) %>%
      dplyr::bind_rows(.id = "file")
  }
}
