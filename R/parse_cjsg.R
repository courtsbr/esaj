#' Parse lawsuits extracted from CJSG query
#'
#' @param file Character vector with the paths to one or more files
#' @param cores Number of cores to use when parsing
#'
#' @return A tibble with the columns
#' \itemize{
#'   \item `file` Name of the file
#'   \item `id_page` ID found in the page
#'   \item `id_decision` Unique ID of the ruling
#'   \item `id_lawsuit` Number of the lawsuit (doesn't have to be unique)
#'   \item `class_subject` Class/subject, separated by slashes
#'   \item `district` Name of the district
#'   \item `court` Body responsible for the appeal
#'   \item `date_decision` Date of the judgement (\%d/\%m/\%Y)
#'   \item `date_publication` Date of the publication (\%d/\%m/\%Y)
#'   \item `date_registration` Date of registration in the system (\%d/\%m/\%Y)
#'   \item `rapporteur` Name of the rapporteur
#'   \item `summary` Summary of the ruling
#'   \item `txt_summary` Text of the summary with no formatting
#' }
#' @export
parse_cjsg <- function(file, cores = 1) {

  # Set names for .id
  names(file) <- file
  file <- file[file.size(file) > 0]

  # Run either with progress bar or on parallel
  if (cores == 1) {
    pb <- progress::progress_bar$new(total = length(file))
    purrr::map_dfr(file, parse_cjsg_, pb, .id = "file")
  } else {
    file %>%
      parallel::mclapply(parse_cjsg_, mc.cores = cores) %>%
      dplyr::bind_rows(.id = "file")
  }
}

#' Parse a page of CJSG results
#'
#' @param file The path to the file to be parsed
#' @param pb Progress bar created by [parse_cjsg()]
#' @return A tibble with the parsed information
parse_cjsg_ <- function(file, pb = NULL) {

  # Safely parse everything
  parse <- purrr::possibly(parse_cjsg_lawsuit, tibble::tibble(), quiet = FALSE)

  # Iterate over xml nodes to parse every lawsuit
  table <- file %>%
    xml2::read_html("UTF-8") %>%
    rvest::html_nodes(".fundocinza1") %>%
    purrr::map_dfr(parse)

  if (!is.null(pb)) { pb$tick() }
  return(table)
}

#' Parse one lawsuit from a CJSG page
#'
#' @param node A `.fundocinza1` node extracted from the page
#' @return One row with the data concerning the lawsuit
parse_cjsg_lawsuit <- function(node) {

  # Auxiliary function to fill in missing columns in table
  fill_in_columns <- function(data) {

    # Fill in ementa and publicacao
    if (!tibble::has_name(data, "ementa"))
      data <- dplyr::mutate(data, ementa = NA_character_)
    if (!tibble::has_name(data, "data_publicacao"))
      data <- dplyr::mutate(data, data_publicacao = NA_character_)

    return(data)
  }

  # Auxiliary function to create a column that doesn't exist
  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]

    if(length(add)!=0) data[add] <- NA_character_
    data
  }

  # Get information from lawsuit
  tmp <- rvest::html_node(node, ".downloadEmenta")
  infos <- tibble::tibble(
    id_lawsuit = stringr::str_trim(rvest::html_text(tmp)),
    id_decision = rvest::html_attr(tmp, "cdacordao"))

  # Get complicated variables
  id <- node %>%
    rvest::html_node(".ementaClass") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[^0-9]", "")
  cs <- node %>%
    rvest::html_node(".assuntoClasse") %>%
    rvest::html_text() %>%
    stringr::str_trim()
  ts <- node %>%
    rvest::html_node("textarea") %>%
    rvest::html_text()

  # Create final table
  node %>%
    rvest::html_nodes(".ementaClass2") %>%
    rvest::html_text() %>%
    stringr::str_split_fixed(":", 2) %>%
    tibble::as_tibble() %>%
    purrr::set_names(c("key", "val")) %>%
    dplyr::mutate_all(stringr::str_trim) %>%
    dplyr::mutate(
      key = key %>%
        abjutils::rm_accent() %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all(" +", "_") %>%
        stringr::str_replace_all("[^a-z_]", "") %>%
        stringr::str_replace_all("_d[eo]_", "_")) %>%
    tidyr::spread(key, val) %>%
    dplyr::bind_cols(infos) %>%
    fill_in_columns() %>%
    dplyr::mutate(id = id, cs = cs, ts = ts) %>%
    fncols("data_julgamento") %>%
    dplyr::select(
      id_page = id, id_decision, id_lawsuit, class_subject = cs,
      district = comarca, court = orgao_julgador, date_decision = data_julgamento,
      date_publication = data_publicacao, date_registration = data_registro,
      rapporteur = relatora, summary = ementa, txt_summary = ts)
}
