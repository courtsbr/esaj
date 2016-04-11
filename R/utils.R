desacentuar <- function(x) {
  gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
}

arrumar_key <- function(x) {
  desacentuar(stringr::str_replace_all(tolower(x), " +", "_"))
}

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
