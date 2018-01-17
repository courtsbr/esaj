#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
NULL

#' Double-pipe operator
#'
#' See \code{\link[magrittr]{\%<>\%}} for more details.
#'
#' @name %<>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %<>%
NULL

#' Quasiquotation of an expression
#'
#' See \code{\link[rlang]{!!}} for more details.
#'
#' @rdname quasiquotation
#' @keywords internal
#' @importFrom rlang !!
NULL

#' Definition operator
#'
#' See \code{\link[rlang]{:=}} for more details.
#'
#' @name :=
#' @rdname op-definition
#' @keywords internal
#' @importFrom rlang :=
NULL

"%||%" <- function(x, y) {
  if (rlang::is_null(x)) y else x
}

# Shortcuts for stringr functions
str_replace_all <- stringr::str_replace_all
str_detect <- stringr::str_detect

# Function for extracting elements from vector given e1 | e2
extract_or <- function(x, e1, e2) {
  magrittr::or(e1, e2) %>% magrittr::extract(x, .)
}

# Convert Portuguese months to number
conv_month <- function(date) {

  # Convert date to more sensible format
  date <- stringr::str_replace_all(date, " de ", "-")
  month <- stringr::str_extract(date, "[:alpha:]+")

  # Get month number
  month <- switch (month,
    "janeiro" = "1",
    "fevereiro" = "2",
    "mar\u00e7o" = "3",
    "abril" = "4",
    "maio" = "5",
    "junho" = "6",
    "julho" = "7",
    "agosto" = "8",
    "setembro" = "9",
    "outubro" = "10",
    "novembro" = "11",
    "dezembro" = "12")

  # Replace name with number
  stringr::str_replace(date, "[:alpha:]+", month)
}

# Convert a date to pt_BR format
date_pt <- function(date) {

  # Check if string is empty
  if (stringr::str_length(date) == 0) { return(date) }

  # Apply conversion
  date <- lubridate::as_date(date)
  stringr::str_c(
    stringr::str_pad(lubridate::day(date), 2, "left", "0"),
    stringr::str_pad(lubridate::month(date), 2, "left", "0"),
    lubridate::year(date),
    sep = "/")
}

# Return time in a human readable way
how_long <- function(x) {

  if (x < 60) {
    round(x, 1) %>% stringr::str_c(" seconds")
  } else if (x < 3600) {
    round(x/60, 1) %>% stringr::str_c(" minutes")
  } else if (x < 86400) {
    round(x/3600 , 1) %>% stringr::str_c(" hours")
  } else if (x < 604800) {
    round(x/86400, 1) %>% stringr::str_c(" days")
  } else {
    round(x/604800, 1) %>% stringr::str_c(" weeks")
  }
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

# Remove diacritics
rm_diacritics <- function(string) {
  string %>%
    abjutils::rm_accent() %>%
    str_replace_all("[\\~\\^\\'\\`]", "")
}

globalVariables(c(
  ".", "Documento", "X1", "X2", "X3", "adv", "arq", "b", "booklet",
  "color", "date_link", "desc", "forma", "g", "head", "id",
  "info", "key", "link", "n", "n_processo", "nome", "r", "result",
  "rm_accent", "setNames", "value", "y", "cd_acordao", "id0", "id5",
  "id_processo", "item", "branch", "court", "txt_ementa", "val", "role",
  "name", "part", "name0", "name1", "name2", "name3", "name4", "name5",
  "class_subject", "comarca", "data_julgamento", "data_registro",
  "ementa", "id_decision", "id_lawsuit", "id_page", "orgao_julgador",
  "relatora", "txt_summary", "hidden", "data_publicacao", "Data",
  "Movimento", "movement", "description"))
