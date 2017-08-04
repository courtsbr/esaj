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

"%||%" <- function(x, y) {
  if (rlang::is_null(x)) y else x
}

# Convert Portuguese months to number
conv_month <- function(date) {

  # Convert date to more sensible format
  date <- stringr::str_replace_all(date, " de ", "-")
  month <- stringr::str_extract(date, "[a-z]+")

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
  stringr::str_replace(date, "[a-z]+", month)
}

globalVariables(c(
  ".", "Documento", "X1", "X2", "X3", "adv", "arq", "b", "booklet",
  "color", "date_link", "desc", "forma", "g", "head", "id",
  "info", "key", "link", "n", "n_processo", "nome", "r", "result",
  "rm_accent", "setNames", "value", "y"))
