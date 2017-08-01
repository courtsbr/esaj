
# Baixar processo do TJBA
# id <- "0552486-62.2015.8.05.0001"
baixar_tjba <- function(id, path = '.') {

  # Tentar 10 vezes no máximo
  for (i in 1:10) {

    # Query
    query <- query_processo(id)
    id_clean <- gsub('[^0-9]', '', id)

    # Acesso inicial
    u_open <- 'http://esaj.tjba.jus.br/cpopg/open.do'
    r_open <- httr::GET(u_open, httr::set_cookies(NULL))

    # Captcha
    u_captcha <- 'http://esaj.tjba.jus.br/cpopg/imagemCaptcha.do'
    r_captcha <- httr::GET(u_captcha)
    tmp <- tempfile()
    writeBin(httr::content(r_captcha, "raw"), tmp)

    # Decodificar captcha
    decodificar <- purrr::possibly(captchasaj::decodificar, 'xxxxx')
    valor <- decodificar(tmp, captchasaj::modelo$modelo)

    # Preencher query
    query$vlCaptcha <- valor

    # Baixar processo
    u_search <- "http://esaj.tjba.jus.br/cpopg/search.do"
    file <- sprintf('%s/%s.html', path, id_clean)
    r <- httr::GET(u_search, query = query,
                   httr::write_disk(file, overwrite = TRUE))

    # Break
    if (!tem_captcha(r)) { break }
    else { file.remove(file) }
  }
}
