
parse_parts <- function(parser) {
  UseMethod("parse_parts", parser)
}

parse_data <- function(parser) {
  UseMethod("parse_data", parser)
}

parse_decisions <- function(parser) {
  UseMethod("parse_decisions", parser)
}

parse_movs <- function(parser) {
  UseMethod("parse_movs", parser)
}

parse_hist <- function(parser) {
  UseMethod("parse_hist", parser)
}

parse_hearings <- function(parser) {
  UseMethod("parse_hearings", parser)
}

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
