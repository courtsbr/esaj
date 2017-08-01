
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

globalVariables(c(
  ".", "Documento", "X1", "X2", "X3", "adv", "arq", "b", "caderno",
  "cor", "date_link", "desc", "edicao", "forma", "g", "head", "id",
  "info", "key", "link", "n", "n_processo", "nome", "r", "result",
  "rm_accent", "setNames", "value", "y"))
