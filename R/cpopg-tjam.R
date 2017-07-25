tirar_fundo_e_riscos <- function(d) {
  d %>%
    dplyr::filter(y > 15) %>%
    dplyr::group_by(cor) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n < max(n)) %>%
    dplyr::filter(n > sort(unique(n), decreasing = TRUE)[3])
}
filtrar_cor <- function(d, cor_nm) {
  cor_correta <- eval(call(tolower(cor_nm), d))
  dplyr::filter(d, cor == cor_correta)
}
laranja <- function(d) {
  d %>%
    dplyr::filter(b < .5) %>%
    dplyr::filter(r + g == max(r + g)) %>%
    head(1) %>%
    with(cor)
}
azul <- function(d) {
  d %>%
    dplyr::filter(r < .5, g < .8) %>%
    dplyr::filter(b == max(b)) %>%
    head(1) %>%
    with(cor)
}
verde <- function(d) {
  d %>%
    dplyr::filter(r < .5, b < .5) %>%
    dplyr::filter(g == max(g)) %>%
    head(1) %>%
    with(cor)
}
vermelho <- function(d) {
  d %>%
    dplyr::filter(b < .5, g < .5) %>%
    dplyr::filter(r == max(r)) %>%
    head(1) %>%
    with(cor)
}
rosa <- function(d) {
  d %>%
    dplyr::filter(g < .8) %>%
    dplyr::filter(r + b == max(r + b)) %>%
    head(1) %>%
    with(cor)
}
roxo <- function(d) {
  d %>%
    dplyr::filter(g < .5) %>%
    dplyr::filter(r + b == max(r + b)) %>%
    head(1) %>%
    with(cor)
}
preto <- function(d) {
  d %>%
    dplyr::filter(r + b + g == min(r + b + g)) %>%
    head(1) %>%
    with(cor)
}

decrypt_captcha_tjam <- function(arq) {
  json <- jsonlite::fromJSON(arq)
  image <- json %>%
    with(imagem) %>%
    stringr::str_split_fixed(',', 2) %>%
    magrittr::extract(TRUE, 2) %>%
    base64enc::base64decode()
  cor_json <- json %>%
    with(labelValorCaptcha) %>%
    stringr::str_match('<b>([A-Za-z]+)') %>%
    magrittr::extract(TRUE, 2)
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
  tmp <- tempfile(fileext = '.png')
  complete_df <- purrr::cross_df(
    list(x = min(img_df$x):max(img_df$x),
         y = min(img_df$y):max(img_df$y))
  )
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
  res <- tmp %>%
    magick::image_read() %>%
    magick::image_trim() %>%
    magick::image_scale('x50') %>%
    tesseract::ocr() %>%
    stringr::str_trim() %>%
    tolower() %>%
    stringr::str_replace_all('[^a-z]', '')
  res
}
decrypt_captcha_tjam <- purrr::possibly(decrypt_captcha_tjam, "xxxx", quiet = T)

download_captcha_tjam <- function(dir = './', time_stamp = '') {
  link_captcha <- 'http://consultasaj.tjam.jus.br/cpopg/imagemCaptcha.do'
  nome <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
  a <- tempfile()
  r <- httr::POST(link_captcha,
                  body = list(timestamp = time_stamp,
                              uuidCaptcha = '',
                              conversationId = ''),
                  httr::write_disk(a, overwrite = TRUE))
  invisible(a)
}

uuid_captcha <- function(arq_captcha) {
  jsonlite::fromJSON(arq_captcha)$uuidCaptcha
}
uuid_captcha <- purrr::possibly(uuid_captcha, "xxxx", quiet = TRUE)

query_processo <- function(p) {
  list('conversationId' = '',
       'dadosConsulta.localPesquisa.cdLocal' = '-1',
       'cbPesquisa' = 'NUMPROC',
       'dadosConsulta.tipoNuProcesso' = 'UNIFICADO',
       'numeroDigitoAnoUnificado' = stringr::str_sub(p, 1, 15),
       'foroNumeroUnificado' = stringr::str_sub(p, -4, -1),
       'dadosConsulta.valorConsultaNuUnificado' = p,
       'dadosConsulta.valorConsulta' = '',
       'uuidCaptcha' = '',
       'vlCaptcha' = '',
       'novoVlCaptcha' = '')
}

tem_captcha <- function(r) {
  (r %>%
     httr::content('text', encoding = "ISO-8859-1") %>%
     xml2::read_html() %>%
     rvest::html_nodes('#captchaCodigo') %>%
     length()) > 0
}
tem_captcha <- purrr::possibly(tem_captcha, TRUE, quiet = TRUE)

baixar_tjam_um <- function(cod_processo, dir_processo = '.') {

  # Tentar 10 vezes no máximo
  for (i in 1:10) {

    # query e preprocess
    query <- query_processo(cod_processo)
    cod_processo_clean <- gsub('[^0-9]', '', cod_processo)

    # acesso inicial (cookies + handle)
    u_open <- 'http://consultasaj.tjam.jus.br/cpopg/open.do'
    r_open <- httr::GET(u_open)

    # captcha
    arq_captcha <- download_captcha_tjam(time_stamp = stringr::str_replace_all(lubridate::now(), "[^0-9]", ""))
    vl_captcha <- decrypt_captcha_tjam(arq_captcha)
    uuid <- uuid_captcha(arq_captcha)
    query$uuidCaptcha <- uuid
    query$vlCaptcha <- vl_captcha

    # baixa processo
    u_search <- 'http://consultasaj.tjam.jus.br/cpopg/search.do'
    arq_processo <- sprintf('%s/%s.html', dir_processo, cod_processo_clean)
    file.remove(arq_captcha)
    r <- httr::GET(u_search, query = query,
                   httr::write_disk(arq_processo, overwrite = TRUE))

    # Break
    if (!tem_captcha(r)) { break }
    else { file.remove(arq_processo) }
  }
}
