
# Remover fundo e riscos de uma imagem
tirar_fundo_e_riscos <- function(img) {
  img %>%
    dplyr::filter(y > 15) %>%
    dplyr::group_by(cor) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n < max(n)) %>%
    dplyr::filter(n > sort(unique(n), decreasing = TRUE)[3])
}

# Detectar a existência de um captcha no processo
tem_captcha <- function(arq) {
  (arq %>%
     httr::content('text', encoding = "ISO-8859-1") %>%
     xml2::read_html() %>%
     rvest::html_nodes('#captchaCodigo') %>%
     length()) > 0
}
tem_captcha <- purrr::possibly(tem_captcha, TRUE)

# Obter o uuid de um captcha
uuid_captcha <- function(arq) {
  jsonlite::fromJSON(arq)$uuidCaptcha
}
uuid_captcha <- purrr::possibly(uuid_captcha, "xxxx")

# Criar um query de processo
query_processo <- function(id) {
  list('conversationId' = '',
       'dadosConsulta.localPesquisa.cdLocal' = '-1',
       'cbPesquisa' = 'NUMPROC',
       'dadosConsulta.tipoNuProcesso' = 'UNIFICADO',
       'numeroDigitoAnoUnificado' = stringr::str_sub(id, 1, 13),
       'foroNumeroUnificado' = stringr::str_sub(id, -4, -1),
       'dadosConsulta.valorConsultaNuUnificado' = id,
       'dadosConsulta.valorConsulta' = '',
       'uuidCaptcha' = '',
       'vlCaptcha' = '',
       'novoVlCaptcha' = '')
}

# Quebrar um captcha com cores
quebrar_captcha_cor <- function(arq) {

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

  # Salvar imagem
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

  file.remove(tmp)
  return(res)
}
quebrar_captcha_cor <- purrr::possibly(quebrar_captcha_cor, "xxxx")

# Baixar um captcha com cores
baixar_captcha_cor <- function(u_captcha, dir = ".", ts = "") {
  tmp <- tempfile()
  r <- httr::POST(u_captcha,
    body = list(timestamp = ts, uuidCaptcha = "", conversationId = ""),
    httr::write_disk(tmp, overwrite = TRUE))

  invisible(tmp)
}
