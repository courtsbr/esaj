tidy_nm <- function (x) {
  x %>% tolower() %>%
    abjutils::rm_accent() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[^0-9a-z]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

parse_cjpg_um <- function(i, nodes) {
  node <- nodes[[i]]
  trim <- stringr::str_trim
  aux <- node %>%
    rvest::html_node(xpath = './/a[@title="Visualizar Inteiro Teor"]')
  id_processo <- aux %>% rvest::html_attr('name') %>% trim()
  n_processo <- aux %>% rvest::html_text() %>% trim() %>%
    stringr::str_replace_all('[^0-9]', '')
  keys <- node %>%
    rvest::html_nodes('table > tr > td > strong') %>%
    rvest::html_text() %>%
    trim() %>%
    tidy_nm()
  xp <- './/table/tr/td/strong/following-sibling::text()[1]'
  vals <- node %>%
    rvest::html_nodes(xpath = xp) %>%
    rvest::html_text() %>%
    trim()
  d_infos <- tibble::tibble(key = keys, val = vals) %>%
    dplyr::mutate(id = 1) %>%
    tidyr::spread(key, val) %>%
    dplyr::select(-id)
  txt <- node %>%
    rvest::html_node(xpath = './/table//div[@style="display: none;"]') %>%
    rvest::html_text() %>%
    trim()
  tab_infos <- tibble::tibble(n_processo = n_processo,
                              id_processo = id_processo) %>%
    dplyr::bind_cols(d_infos) %>%
    dplyr::mutate(txt = txt)
  tab_infos
}

parse_cjpg_arq <- function(arq) {
  itens <- xml2::read_html(arq, encoding = 'UTF-8') %>%
    rvest::html_nodes('.fundocinza1')
  abjutils::dvec(parse_cjpg_um, 1:length(itens),
                 nodes = itens, verbose = FALSE) %>%
    dplyr::select(-item)
}

#' @title CJPG parser
#'
#' @description Parser for files downloaded by [cjpg()].
#'
#' @param arqs Character vector with the paths of the files to be parsed
#'
#' @return A tibble with the columns
#' \itemize{
#'   \item `arq` Name of the file
#'   \item `id` ID found in the page read
#'   \item `cd_acordao` Unique ID of the ruling
#'   \item `n_processo` Number of the lawsuit (doesn't have to be unique)
#'   \item `comarca` Name of the district
#'   \item `data_julgamento` Date of the judgement (\%d/\%m/\%Y)
#'   \item `data_registro` Date of registration in the system (\%d/\%m/\%Y)
#'   \item `ementa` Summary of the ruling
#'   \item `orgao_julgador` Body responsible for the appeal
#'   \item `outros_numeros` Old/additional IDs
#'   \item `relatora` Name of the rapporteur
#'   \item `classe_assunto` Class/subject, separated by slashes
#'   \item `txt_ementa` Text of the summary with no formatting
#' }
#' @export
parse_cjpg <- function(arqs) {
  abjutils::dvec(parse_cjpg_arq, arqs) %>%
    dplyr::rename(arq = item) %>%
    dplyr::distinct(id_processo, .keep_all = TRUE)
}
