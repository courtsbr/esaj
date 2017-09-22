
#' @title Download information about some of CJPG's structures
#'
#' @description Downloads a table with information about lawsuit's
#' classes, subjects or courts to help with [download_cjpg()]. You
#' can also browse some of these tables with [browse_table()].
#'
#' @param type Either `"classes"` or `"subjects"` or `"courts"`
#' @param tj TJ from which to get data (only works with TJSP for now)
#' @return A tibble with either 12 columns (if `type` is `"classes"`
#' or `"subjects"`) or 3 columns (if `type` is `"courts"`)
#'
#' @seealso [download_cjpg()], [browse_table()]
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

#' @title Browse table returned by [cjpg_table()] or [cjsg_table()]
#'
#' @description This function uses a list of regex to filter CJPG
#' and CJSG tables (only if they are of type `"classes"` or
#' `"subjects"`) more easily than with `dplyr::select()`. For
#' details on how the matching occurs, see **Matching**.
#'
#' @section Matching: For the matching to work properly, `patterns`
#' should be a list of at most 6 character vectors, each one
#' containing either one or a vector of regular expressions to
#' be applied from left to right on columns `name0` to `name5`
#' (note that vectors are ORed and different elements are ANDed).
#' Example: If `patterns` looks something like
#' `list(c("ADM", "CRIMINAL"), "", "", "", "", "Recurso")`,
#' then we'll get back the rows where `name0` contains "ADM"
#' **or** "CRIMINAL" **and** where `name5` contains "Recurso".
#'
#' @param table Table returned by [cjpg_table()] or [cjsg_table()]
#' (only valid for `"classes"` or `"subjects"` types)
#' @param patterns A list containing (at most) 6 character vectors
#' of one or more regular expressions (applied from left to right
#' on `name0` to `name5`), e.g.,
#' `list(c("ADM", "CRIMINAL"), "", "", "", "", "Recurso")`
#' @return The original table filtered according to `patterns`
#'
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
