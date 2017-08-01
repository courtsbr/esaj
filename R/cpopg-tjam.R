
# Baixar processo do TJAM
# id <- "0257518-22.2013.8.04.0001"
baixar_tjam <- function(id, path = '.') {

  # Tentar 10 vezes no máximo
  for (i in 1:10) {

    # Query
    query <- query_processo(id)
    id_clean <- gsub('[^0-9]', '', id)

    # Acesso inicial
    u_open <- 'http://consultasaj.tjam.jus.br/cpopg/open.do'
    r_open <- httr::GET(u_open)

    # Captcha
    time_stamp <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
    captcha <- baixar_captcha_cor(path, time_stamp)

    # Preencher query
    query$uuidCaptcha <- uuid_captcha(captcha)
    query$vlCaptcha <- quebrar_captcha_cor(captcha)

    # Baixar processo
    u_search <- 'http://consultasaj.tjam.jus.br/cpopg/search.do'
    file <- sprintf('%s/%s.html', path, id_clean)
    r <- httr::GET(u_search, query = query,
                   httr::write_disk(file, overwrite = TRUE))

    # Break
    if (!tem_captcha(r)) { break }
    else { file.remove(file) }
  }
}
