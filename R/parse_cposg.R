
#' Parses parts
#' @param parser A parser returned by [make_parser()]
#' @export
parse_parts.cposg <- function(parser) {

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting parts
  get_parts <- function(html) {
    html %>%
      xml2::xml_find_all("//*[@id='tablePartesPrincipais']") %>%
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
parse_data.cposg <- function(parser) {

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting data
  get_data <- function(html) {
    html %>%
      xml2::xml_find_all("//*[@class='secaoFormBody']") %>%
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
parse_movs.cposg <- function(parser) {

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting movements
  get_movs <- function(html) {
    html %>%
      xml2::xml_find_all("//*[@id='tabelaTodasMovimentacoes']") %>%
      rvest::html_table(fill = TRUE) %>%
      purrr::pluck(1) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        X1 = lubridate::dmy(X1, quiet = TRUE),
        X3 = str_replace_all(X3, "[\\t\\n]", ""),
        X3 = str_replace_all(X3, "\\r", " "),
        X3 = str_replace_all(X3, " +", " ")) %>%
      dplyr::select(-X2) %>%
      purrr::set_names("movement", "description")
  }

  # Add get_movs to getters
  purrr::list_merge(parser, name = "movs", getter = get_movs)
}

#' Parses decisions
#' @param parser A parser returned by [make_parser()]
#' @export
parse_decisions.cposg <- function(parser){

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting decisions
  get_decisions <- function(html) {

    #Gets all eligible tables
    tables <- html %>%
      xml2::xml_find_all("//table[@style='margin-left:15px; margin-top:1px;']")

    #Beginning of the table
    first_table <- tables %>%
      rvest::html_text() %>%
      stringr::str_which("Situa\u00e7\u00e3o do julgamento") %>%
      max()

    #Check if first_table is Inf
    if(is.infinite(first_table)){return(dplyr::data_frame(date = NA, decision = NA))}

    #End of the table
    last_table <- length(tables)

    tables[first_table:last_table] %>%
      rvest::html_table(fill = TRUE) %>%
      dplyr::bind_rows() %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        X1 = lubridate::dmy(X1, quiet = TRUE),
        X2 = stringr::str_replace_all(X2, "[:space:]+"," "),
        X3 = stringr::str_replace_all(X3, "[:space:]+", " ")) %>%
      dplyr::select(-X2) %>%
      dplyr::filter(!is.na(X1)) %>%
      purrr::set_names("date", "decision")
  }

  # Add get_decisions to getters
  purrr::list_merge(parser, name = "decisions", getter = get_decisions)
}
