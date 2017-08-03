# Get URLs for download depending on its TJ
get_lwst_data <- function(id) {

  # Switch base for URLs depending on TJ's number
  urls <- switch(get_n(id),
                 "04" = list(u_captcha = "consultasaj.tjam", u_search = "consultasaj.tjam"),
                 "05" = list(u_captcha = "esaj.tjba", u_search = "esaj.tjba"),
                 "24" = list(u_captcha = "esaj.tjsc", u_search = "esaj.tjsc"))

  # Fill rest of URLs
  urls$u_captcha <- stringr::str_c(
    "http://", urls$u_captcha, ".jus.br/cpopg/imagemCaptcha.do")
  urls$u_search <- stringr::str_c(
    "http://", urls$u_search, ".jus.br/cpopg/search.do")

  return(urls)
}

# Get TJ's number
get_n <- function(id) {
  if (stringr::str_length(id) == 20) { stringr::str_sub(id, 15, 16) }
  else if (stringr::str_length(id) == 25) { stringr::str_sub(id, 19, 20) }
  else { stop("Ivalid ID") }
}
