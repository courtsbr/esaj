
# Baixar processo do TJBA
# id <- "0552486-62.2015.8.05.0001"
baixar_tjba <- function(id, path = '.') {

  # Try at most 10 times
  for (i in 1:10) {

    # Aux function for breaking captcha
    break_captcha <- purrr::possibly(captchasaj::decodificar, 'xxxxx')

    # Relevant URLs
    u_captcha <- "http://esaj.tjba.jus.br/cpopg/imagemCaptcha.do"
    u_search <- "http://esaj.tjba.jus.br/cpopg/search.do"

    # Download captcha
    f_captcha <- tempfile()
    writeBin(httr::content(httr::GET(u_captcha), "raw"), f_captcha)

    # Create GET query
    query <- query_processo(id)
    query$vlCaptcha <- break_captcha(f_captcha, captchasaj::modelo$modelo)

    # Download lawsuit
    f_lwst <- sprintf("%s/%s.html", path, gsub("[^0-9]", "", id))
    f_search <- httr::GET(u_search, query = query, httr::write_disk(f_lwst, TRUE))

    # Free temporary file
    file.remove(f_captcha)

    # Breaking condition
    if (!tem_captcha(f_search)) { break }
    else { file.remove(f_lwst) }
  }
}
