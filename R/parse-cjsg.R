parse_cjsg_one <- function(node) {
  trim <- stringr::str_trim
  id <- node %>%
    rvest::html_node('.ementaClass') %>%
    rvest::html_text() %>%
    trim() %>%
    stringr::str_replace_all('[^0-9]', '')
  infos <- node %>%
    rvest::html_node('.downloadEmenta') %>% {
      tibble::tibble(id_lawsuit = trim(rvest::html_text(.)),
                     id_decision = rvest::html_attr(., 'cdacordao'))
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
    dplyr::mutate(id_page = id,
                  class_subject = ca,
                  txt_summary = tsf) %>%
    dplyr::select(id_page,
                  id_decision,
                  id_lawsuit,
                  class_subject,
                  district = comarca,
                  court = orgao_julgador,
                  dt_decision = data_julgamento,
                  dt_registration = data_registro,
                  rapporteur = relatora,
                  summary = ementa,
                  txt_summary) %>%
    tibble::as_tibble()
  tab_infos
}

parse_cjsg_file <- function(file) {
  safe_parse_cjsg_one <- purrr::possibly(parse_cjsg_one,
                                         tibble::tibble(result = "error"))
  items <- xml2::read_html(file, encoding = 'UTF-8') %>%
    rvest::html_nodes('.fundocinza1')
  purrr::map_dfr(items, parse_cjsg_one)
}

#' @title CJSG parser
#'
#' @description Parser for files downloaded by [download_cjsg()].
#'
#' @param files Character vector with the paths of the files to be parsed
#' @param cores Number of cores to be used. Defaults to 1 to show progress bar.
#'
#' @return A tibble with the columns
#' \itemize{
#'   \item `file` Name of the file
#'   \item `id_page` ID found in the page
#'   \item `id_decision` Unique ID of the ruling
#'   \item `id_lawsuit` Number of the lawsuit (doesn't have to be unique)
#'   \item `district` Name of the district
#'   \item `dt_decision` Date of the judgement (\%d/\%m/\%Y)
#'   \item `dt_registration` Date of registration in the system (\%d/\%m/\%Y)
#'   \item `summary` Summary of the ruling
#'   \item `court` Body responsible for the appeal
#'   \item `rapporteur` Name of the rapporteur
#'   \item `class_subject` Class/subject, separated by slashes
#'   \item `txt_summary` Text of the summary with no formatting
#' }
#' @export
parse_cjsg <- function(files, cores = 1) {
  names(files) <- files
  safe_parse_cjsg_file <- purrr::possibly(parse_cjsg_file,
                                          tibble::tibble(result = "error"))
  if (cores == 1) {
    p <- progress::progress_bar$new(total = length(files))
    purrr::map_dfr(files, ~{
      p$tick()
      safe_parse_cjsg_file(.x)
    }, .id = "file")
  } else {
    parallel::mclapply(files, safe_parse_cjsg_file, mc.cores = cores) %>%
      dplyr::bind_rows(.id = "file")
  }
}
