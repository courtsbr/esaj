
#' @title Download information about some of CJSG's structures
#'
#' @description Downloads a table with information about lawsuit's
#' classes, subjects or courts to help with [download_cjsg()]. You
#' can also browse some of these tables with [browse_table()].
#'
#' @param type Either `"classes"` or `"subjects"` or `"courts"`
#' @param tj TJ from which to get data (only works with TJSP for now)
#' @return A tibble with either 12 columns (if `type` is `"classes"`
#' or `"subjects"`) or 3 columns (if `type` is `"courts"`)
#'
#' @seealso [download_cjpg()], [browse_table()]
#' @export
cjsg_table <- function(type, tj = "tjsp") {

  # Stop if TJ isn't TJSP
  stopifnot(tj == "tjsp")

  # If type is courts, redirect
  if (type == "courts") { return(cjsg_courts()) }

  # Translate type
  type <- switch (type,
    classes = "classes",
    subjects = "assuntos")

  # Fetch table with information
  stringr::str_c(
    "https://esaj.tjsp.jus.br/cjsg/", type,
    "TreeSelect.do?campoId=", type) %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE)) %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//div[@class='treeView']") %>%
    purrr::modify(xml2::as_list) %>%
    dplyr::first() %>% dplyr::nth(2) %>%
    purrr::keep(~is.list(.x)) %>%
    tree_to_tibble() %>%
    dplyr::mutate(
      name0 = ifelse(is.na(name0), name5, name0),
      id0 = ifelse(is.na(id0), id5, id0)) %>%
    dplyr::select(
      dplyr::ends_with('0'), dplyr::ends_with('1'),
      dplyr::ends_with('2'), dplyr::ends_with('3'),
      dplyr::ends_with('4'), dplyr::ends_with('5'))
}

# Download table with court information for [cjsg_table()]
cjsg_courts <- function() {

  # Function for creating the rows of the table
  create_row <- function(x) {

    # XPaths
    xp_parent <- ".//span[contains(@id, 'secoes_tree')]"
    xp_child <- ".//li[@class='leafItem']//span[contains(@id, 'secoes_tree')]"

    # Create row
    branch <- x %>%
      rvest::html_node(xpath = xp_parent) %>%
      rvest::html_text()
    x %>%
      rvest::html_nodes(xpath = xp_child) %>%
      purrr::map(~tibble::tibble(
        id = rvest::html_attr(.x, 'value'),
        court = rvest::html_text(.x))) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(branch = branch)
  }

  # Create courts table
  stringr::str_c(
    "https://esaj.tjsp.jus.br/cjsg/",
    "secaoTreeSelect.do?campoId=secoes") %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE)) %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_nodes('li.open') %>%
    rlang::as_list() %>%
    purrr::modify(create_row) %>%
    dplyr::bind_rows() %>%
    dplyr::select(branch, court, id)
}
