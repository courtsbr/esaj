parse_cjsg_um <- function(i, nodes) {
  node <- nodes[[i]]
  trim <- stringr::str_trim
  id <- node %>%
    rvest::html_node('.ementaClass') %>%
    rvest::html_text() %>%
    trim() %>%
    stringr::str_replace_all('[^0-9]', '')
  infos <- node %>%
    rvest::html_node('.downloadEmenta') %>% {
      tibble::tibble(n_processo = trim(rvest::html_text(.)),
                     cd_acordao = rvest::html_attr(., 'cdacordao'))
    }
  ca <- node %>%
    rvest::html_node('.assuntoClasse') %>%
    rvest::html_text() %>%
    trim()
  tsf <- node %>%
    rvest::html_node('textarea') %>%
    rvest::html_text()
  tab_infos <- node %>%
    rvest::html_nodes('.ementaClass2') %>%
    rvest::html_text() %>%
    stringr::str_split_fixed(':', 2) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    magrittr::set_names(c('key', 'val')) %>%
    dplyr::mutate_all(dplyr::funs(trim(.))) %>%
    dplyr::mutate(key = tolower(abjutils::rm_accent(key)),
                  key = stringr::str_replace_all(key, ' +', '_'),
                  key = stringr::str_replace_all(key, '[^a-z_]', ''),
                  key = stringr::str_replace_all(key, '_d[eo]_', '_')) %>%
    tidyr::spread(key, val) %>%
    dplyr::bind_cols(infos) %>%
    dplyr::mutate(id = id, classe_assunto = ca, txt_ementa = tsf) %>%
    dplyr::select(id, cd_acordao, n_processo, dplyr::everything(), txt_ementa)
  tab_infos
}

parse_cjsg_arq <- function(arq) {
  itens <- xml2::read_html(arq, encoding = 'UTF-8') %>%
    rvest::html_nodes('.fundocinza1')
  abjutils::dvec(parse_cjsg_um, 1:length(itens), nodes = itens, verbose = FALSE) %>%
    dplyr::select(-item)
}

#' @title CJSG parser
#'
#' @description Parser for files downloaded by [cjsg()].
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
parse_cjsg <- function(arqs) {
  abjutils::dvec(parse_cjsg_arq, arqs) %>%
    dplyr::rename(arq = item)
}
