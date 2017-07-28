
# Baixar processo do TJAM
# cod_processo <- "0257518-22.2013.8.04.0001"
baixar_tjam <- function(cod_processo, dir_processo = '.') {

  # Tentar 10 vezes no máximo
  for (i in 1:10) {

    # Query
    query <- query_processo(cod_processo)
    cod_processo_clean <- gsub('[^0-9]', '', cod_processo)

    # Acesso inicial
    u_open <- 'http://consultasaj.tjam.jus.br/cpopg/open.do'
    r_open <- httr::GET(u_open)

    # Captcha
    time_stamp <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
    arq_captcha <- baixar_captcha_cor(dir_processo, time_stamp)

    # Preencher query
    query$uuidCaptcha <- uuid_captcha(arq_captcha)
    query$vlCaptcha <- quebrar_captcha_cor(arq_captcha)

    # Baixar processo
    u_search <- 'http://consultasaj.tjam.jus.br/cpopg/search.do'
    arq_processo <- sprintf('%s/%s.html', dir_processo, cod_processo_clean)
    r <- httr::GET(u_search, query = query,
                   httr::write_disk(arq_processo, overwrite = TRUE))

    # Break
    if (!tem_captcha(r)) { break }
    else { file.remove(arq_processo) }
  }
}
