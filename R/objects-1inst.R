get_obj <- function(tipo) {
  u_base <- 'https://esaj.tjsp.jus.br/cjpg/%sTreeSelect.do?campoId=%s'
  r <- httr::GET(sprintf(u_base, tipo[1], tipo[1]), httr::config(ssl_verifypeer = FALSE))
}

# Transform an XML tree into a tibble
tree_to_tibble <- function(tree, n = 0) {

  # Extract category names
  names <- tree %>%
    purrr::map(purrr::pluck, 2, 1) %>%
    purrr::compact() %>%
    magrittr::extract(. != "") %>%
    purrr::flatten_chr()

  # Extract category codes
  ids <- tree %>%
    purrr::map(purrr::pluck, 2) %>%
    purrr::map(attr, "value") %>%
    purrr::compact() %>%
    magrittr::extract(. != "") %>%
    purrr::flatten_chr()

  # Iterate over every branch of tree
  purrr::imap_dfr(lengths(tree, FALSE), function(len, i) {

    # If element is a leaf node, return it's contents
    # Otherwise recur on it's elements
    if (len == 3) {
      dplyr::tibble(name5 = names[i], id5 = ids[i])
    }
    else {
      tree %>%
      purrr::pluck(i, 4) %>%
      magrittr::extract(names(.) == 'li') %>%
      tree_to_tibble(n + 1) %>%
      dplyr::mutate(
        !!stringr::str_c("name", n) := names[i],
        !!stringr::str_c("id", n) := ids[i])
    }
  })
}

#' @title Downloads table of CJPG items
#'
#' @description Downloads a table with the elements of class, subject and circuit to help with [cjpg()]
#'
#' @param tipo class, subject or circuit
#' @param tj TJ of the table (only works with TJSP for now)
#'
#' @return `data.frame` with titles and ID of the leaves and nodes of the table tree
#'
#' @export
cjpg_table <- function(tipo = c('classe', 'assunto', 'varas'), tj = "tjsp") {
  stopifnot(tj == "tjsp")
  r <- get_obj(tipo)
  tree <- r %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    xml2::xml_find_all("//div[@class='treeView']") %>%
    purrr::modify(xml2::as_list) %>%
    dplyr::first() %>% dplyr::nth(2) %>%
    purrr::keep(~is.list(.x)) %>%
    tree_to_tibble() %>%
    dplyr::select(
      dplyr::ends_with('0'), dplyr::ends_with('1'),
      dplyr::ends_with('2'), dplyr::ends_with('3'),
      dplyr::ends_with('4'), dplyr::ends_with('5'))
}
