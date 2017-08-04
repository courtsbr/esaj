
# Remove captcha's background and image
rm_bg_and_lines <- function(img) {
  img %>%
    dplyr::filter(y > 15) %>%
    dplyr::group_by(color) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n < max(n)) %>%
    dplyr::filter(n > sort(unique(n), decreasing = TRUE)[3])
}

# Detect whether file has a captcha
has_captcha <- function(file) {
  (httr::content(file, 'text', encoding = "ISO-8859-1") %>%
    xml2::read_html() %>%
    rvest::html_nodes('#captchaCodigo') %>%
    length()) > 0
}
has_captcha <- purrr::possibly(has_captcha, TRUE)

# Get captcha's UUID
captcha_uuid <- function(file) {
  jsonlite::fromJSON(file)$uuidCaptcha
}
captcha_uuid <- purrr::possibly(captcha_uuid, "xxxx")

# Create query to download lawsuit
lawsuit_query <- function(id) {
  list("conversationId" = "",
       "dadosConsulta.localPesquisa.cdLocal" = "-1",
       "cbPesquisa" = "NUMPROC",
       "dadosConsulta.tipoNuProcesso" = "UNIFICADO",
       "numeroDigitoAnoUnificado" = stringr::str_sub(id, 1, 13),
       "foroNumeroUnificado" = stringr::str_sub(id, -4, -1),
       "dadosConsulta.valorConsultaNuUnificado" = id,
       "dadosConsulta.valorConsulta" = "",
       "uuidCaptcha" = "",
       "vlCaptcha" = "",
       "novoVlCaptcha" = "")
}

# Break RGB captcha
break_rgb_captcha <- function(file) {

  # Get file's JSON
  json <- jsonlite::fromJSON(file)

  # Collect file's image
  image <- json %>%
    with(imagem) %>%
    stringr::str_split_fixed(",", 2) %>%
    magrittr::extract(TRUE, 2) %>%
    base64enc::base64decode()

  # Collect file's colors
  color_json <- json %>%
    with(labelValorCaptcha) %>%
    stringr::str_match("<b>([A-Za-z]+)") %>%
    magrittr::extract(TRUE, 2)

  # Create image data frame
  img_png <- png::readPNG(image)
  img_dim <- dim(img_png)
  img_df <- tibble::tibble(
      x = rep(1:img_dim[2], each = img_dim[1]),
      y = rep(img_dim[1]:1, img_dim[2]),
      r = as.vector(img_png[,,1]),
      g = as.vector(img_png[,,2]),
      b = as.vector(img_png[,,3])) %>%
    dplyr::mutate(color = rgb(r, g, b), id = 1:n()) %>%
    rm_bg_and_lines()

  # Fill in data frame
  tmp <- tempfile(fileext = ".png")
  complete_df <- purrr::cross_df(list(
    x = min(img_df$x):max(img_df$x),
    y = min(img_df$y):max(img_df$y)))

  # Save image to temporary file
  img_df %>%
    filter_color(color_json) %>%
    dplyr::mutate(black = 0) %>%
    dplyr::arrange(x, y) %>%
    dplyr::right_join(complete_df, c("x", "y")) %>%
    tidyr::replace_na(list(black = 1)) %>%
    dplyr::select(x, y, black) %>%
    tidyr::spread(x, black, fill = 1) %>%
    dplyr::select(-y) %>%
    as.matrix() %>%
    magrittr::extract(nrow(.):1, TRUE) %>%
    png::writePNG(tmp)

  # Guess captcha's solution
  sol <- tmp %>%
    magick::image_read() %>%
    magick::image_trim() %>%
    magick::image_scale("x50") %>%
    tesseract::ocr() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z]", "")

  file.remove(tmp)
  return(sol)
}
break_rgb_captcha <- purrr::possibly(break_rgb_captcha, "xxxx")

# Download an RGB captcha to a temporary file
download_rgb_captcha <- function(u_captcha, ts = "") {

  # Download captcha useing time stamp
  tmp <- tempfile()
  r <- httr::POST(u_captcha,
    body = list(timestamp = ts, uuidCaptcha = "", conversationId = ""),
    httr::write_disk(tmp, overwrite = TRUE))

  return(tmp)
}
