desacentuar <- function(x) {
  gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
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
