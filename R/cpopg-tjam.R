
# Download TJAM lawsuit
# id <- "0257518-22.2013.8.04.0001"
baixar_tjam <- function(id, path = '.') {

  # Try at most 10 times
  for (i in 1:10) {

    # Relevant URLs
    u_captcha <- "http://consultasaj.tjam.jus.br/cpopg/imagemCaptcha.do"
    u_search <- "http://consultasaj.tjam.jus.br/cpopg/search.do"

    # Download captcha
    time_stamp <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
    f_captcha <- baixar_captcha_cor(u_captcha, path, time_stamp)

    # Create GET query
    query <- query_processo(id)
    query$uuidCaptcha <- uuid_captcha(f_captcha)
    query$vlCaptcha <- quebrar_captcha_cor(f_captcha)

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
