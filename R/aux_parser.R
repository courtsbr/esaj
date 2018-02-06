
#' Parse parts
#' @param parser A parser returned by [make_parser()]
#' @export
parse_parts <- function(parser) {
  UseMethod("parse_parts", parser)
}

#' Parse data
#' @param parser A parser returned by [make_parser()]
#' @export
parse_data <- function(parser) {
  UseMethod("parse_data", parser)
}

#' Parse decisions
#' @param parser A parser returned by [make_parser()]
#' @export
parse_decisions <- function(parser) {
  UseMethod("parse_decisions", parser)
}

#' Parse movements
#' @param parser A parser returned by [make_parser()]
#' @export
parse_movs <- function(parser) {
  UseMethod("parse_movs", parser)
}

#' Parse history
#' @param parser A parser returned by [make_parser()]
#' @export
parse_hist <- function(parser) {
  UseMethod("parse_hist", parser)
}

#' Parse hearings
#' @param parser A parser returned by [make_parser()]
#' @export
parse_hearings <- function(parser) {
  UseMethod("parse_hearings", parser)
}

#' Parse police department
#' @param parser A parser returned by [make_parser()]
#' @export
parse_pd <- function(parser) {
  UseMethod("parse_pd", parser)
}

# Print parser
print.parser <- function(x, ...) {
  if (length(x$name) == 0) {
    cat("An empty parser\n")
  }
  else {
    cat("A parser for the following objects:\n")
    purrr::walk(x$name, ~cat("- ", .x, "\n", sep = ""))
  }
}
