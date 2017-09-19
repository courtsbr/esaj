
#' @title Downloads information about some CJPG structures
#' @description Downloads a table with information about lawsuit
#' classes, subjects or courts to help with [download_cjpg()]
#' @param type Either `"classes"` or `"subjects"` or `"courts"`
#' @param tj TJ form which to get data (only works with TJSP for now)
#' @export
cjpg_table <- function(type, tj = "tjsp") {

  # Stop if TJ isn't TJSP
  stopifnot(tj == "tjsp")

  # If type is courts, redirect
  if (type == "courts") { return(cjpg_courts()) }

  # Translate type
  type <- switch (type,
    classes = "classe",
    subjects = "assunto")

  # Fetch table with information
  stringr::str_c(
    "https://esaj.tjsp.jus.br/cjpg/", type,
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

# Download table with court information for [cjpg_table()]
cjpg_courts <- function() {

  # Function for creating the rows of the table
  create_row <- function(x) {

    # XPaths
    xp_parent <- ".//span[contains(@id, 'varas_tree')]"
    xp_child <- ".//li[@class='leafItem']//span[contains(@id, 'varas_tree')]"

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
    "https://esaj.tjsp.jus.br/cjpg/",
    "varasTreeSelect.do?campoId=varas") %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE)) %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_nodes('li.open') %>%
    rlang::as_list() %>%
    purrr::modify(create_row) %>%
    dplyr::bind_rows() %>%
    dplyr::select(branch, court, id)
}

#' Browse table returned by [cjpg_table()] or [cjsg_table()]
#' @param table Table returned by [cjpg_table()] or [cjsg_table()]
#' (only valid for `"classes"` or `"subjects"` types)
#' @param patterns A list containing (at most) 6 character vectors
#' of one or more regular expressions (applied from left to right
#' on root to leaves); e.g.
#' `list(c("ADM", "CRIMINAL"), "", "", "", "", "")`
#' @details Regex of the same level will be ORed and of different
#' levels will be ANDed, e.g.,
#' `list(c("ADM", "CRIMINAL"), "", "", "", "", "")` becomes
#' `list("(?:ADM|CRIMINAL)", "", "", "", "", "")` and each
#' element will be applied with [dplyr::filter()] to a level of
#' the tree
#' @seealso [cjpg_table()], [cjsg_table()]
#' @export
browse_table <- function(table, patterns) {

  patterns <- purrr::modify(patterns, function(pat) {
    pat %>%
      stringr::str_c(collapse = "|") %>%
      stringr::str_c("(?:", ., ")") %>%
      stringr::str_replace("\\(\\?\\:\\)", "")
  })

  # Transform NAs into matches
  str_detect <- function(string, pattern) {
    stringr::str_detect(string, pattern) %>%
      magrittr::inset(is.na(.) && pattern == "", TRUE)
  }

  # Apply filters
  table %>%
    dplyr::filter(
      str_detect(name0, patterns[[1]]),
      str_detect(name1, patterns[[2]]),
      str_detect(name2, patterns[[3]]),
      str_detect(name3, patterns[[4]]),
      str_detect(name4, patterns[[5]]),
      str_detect(name5, patterns[[6]]))
}
