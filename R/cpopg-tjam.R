
# Quebrar um captcha do TJAM
quebrar_captcha_tjam <- function(arq) {

  # Trazer o json do arquivo
  json <- jsonlite::fromJSON(arq)

  # Coletar a imagem do arquivo
  image <- json %>%
    with(imagem) %>%
    stringr::str_split_fixed(',', 2) %>%
    magrittr::extract(TRUE, 2) %>%
    base64enc::base64decode()

  # Coletar cores
  cor_json <- json %>%
    with(labelValorCaptcha) %>%
    stringr::str_match('<b>([A-Za-z]+)') %>%
    magrittr::extract(TRUE, 2)

  # Criar img_df
  img_png <- png::readPNG(image)
  img_dim <- dim(img_png)
  img_df <- tibble::tibble(
      x = rep(1:img_dim[2], each = img_dim[1]),
      y = rep(img_dim[1]:1, img_dim[2]),
      r = as.vector(img_png[,,1]),
      g = as.vector(img_png[,,2]),
      b = as.vector(img_png[,,3])
    ) %>%
    dplyr::mutate(cor = rgb(r, g, b), id = 1:n()) %>%
    tirar_fundo_e_riscos()

  # Completar df
  tmp <- tempfile(fileext = '.png')
  complete_df <- purrr::cross_df(
    list(x = min(img_df$x):max(img_df$x),
         y = min(img_df$y):max(img_df$y))
  )

  # Mostrar imagem
  img_df %>%
    filtrar_cor(cor_json) %>%
    dplyr::mutate(preto = 0) %>%
    dplyr::arrange(x, y) %>%
    dplyr::right_join(complete_df, c('x', 'y')) %>%
    tidyr::replace_na(list(preto = 1)) %>%
    dplyr::select(x, y, preto) %>%
    tidyr::spread(x, preto, fill = 1) %>%
    dplyr::select(-y) %>%
    as.matrix() %>%
    magrittr::extract(nrow(.):1, TRUE) %>%
    png::writePNG(tmp)

  # Resposta para o captcha
  res <- tmp %>%
    magick::image_read() %>%
    magick::image_trim() %>%
    magick::image_scale('x50') %>%
    tesseract::ocr() %>%
    stringr::str_trim() %>%
    tolower() %>%
    stringr::str_replace_all('[^a-z]', '')

  return(res)
}
quebrar_captcha_tjam <- purrr::possibly(quebrar_captcha_tjam, "xxxx")

# Baixar um captcha do TJAM
baixar_captcha_tjam <- function(dir = '.', ts = '') {
  tmp <- tempfile()
  r <- httr::POST(
    'http://consultasaj.tjam.jus.br/cpopg/imagemCaptcha.do',
    body = list(timestamp = ts, uuidCaptcha = '', conversationId = ''),
    httr::write_disk(tmp, overwrite = TRUE))

  invisible(tmp)
}

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
    arq_captcha <- baixar_captcha_tjam(dir_processo, time_stamp)

    # Preencher query
    query$uuidCaptcha <- uuid_captcha(arq_captcha)
    query$vlCaptcha <- quebrar_captcha_tjam(arq_captcha)

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
