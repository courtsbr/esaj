
#' @title Downloads information about some CJSG structures
#'
#' @description Downloads a table with information about lawsuit
#' classes, subjects or courts to help with [download_cjsg()]
#'
#' @param type Either `"classes"` or `"subjects"` or `"courts"`
#' @param tj TJ to fetch from (only works with TJSP for now)
#'
#' @export
cjsg_table <- function(type, tj = "tjsp") {

  # Stop if TJ isn't TJSP
  stopifnot(tj == "tjsp")

  # If type is courts, redirect
  if (type == "courts") { return(courts_table()) }

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
    XML::htmlParse(encoding = 'UTF-8') %>%
    XML::getNodeSet('//div[@class="treeView"]') %>%
    purrr::modify(XML::xmlToList) %>%
    dplyr::first() %>% dplyr::nth(2) %>%
    purrr::keep(~is.list(.x)) %>%
    tree_to_tibble() %>%
    dplyr::select(
      dplyr::ends_with('leaf'),
      dplyr::ends_with('0'),
      dplyr::everything()) %>%
    dplyr::mutate(
      titulo0 = ifelse(is.na(titulo0), titulo_leaf, titulo0),
      cod0 = ifelse(is.na(cod0), titulo_leaf, cod0))
}

#' Download table with court information for [cjsg_table()]
courts_table <- function() {

  # Function for creating the rows of the table
  create_row <- function(x) {

    # XPaths
    xp_parent <- ".//span[contains(@id, 'secoes_tree')]"
    xp_child <- ".//li[@class='leafItem']//span[contains(@id, 'secoes_tree')]"

    # Create row
    pai <- x %>%
      rvest::html_node(xpath = xp_parent) %>%
      rvest::html_text()
    x %>%
      rvest::html_nodes(xpath = xp_child) %>%
      purrr::map(~tibble::tibble(
        cod = rvest::html_attr(.x, 'value'),
        secao = rvest::html_text(.x))) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(pai = pai)
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
    dplyr::select(pai, secao, cod)
}
