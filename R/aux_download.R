# Get URLs for download depending on its TJ
get_urls <- function(id) {

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
  stringr::str_split(id, "\\.")[[1]][4]
}
