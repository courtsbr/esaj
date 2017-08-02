
# ids <- c("02575182220138040001", "0303349-44.2014.8.24.0020", "0552486-62.2015.8.05.0001")

# Download a lawsuit from a TJ
# @export
download_lawsuit <- function(id, path = ".") {

  # Strip ID down
  id <- stringr::str_replace_all(id, "[^0-9]", "")
  if (stringr::str_length(id) != 20) { stop("Invalid ID") }

  # Choose appropriate download function
  if (get_n(id) %in% c("05")) { download <- download_bw_lawsuit }
  else { download <- download_rgb_lawsuit }

  # Get URLs for the download
  data <- get_lwst_data(id)

  # Download lawsuit
  download(id, path, data$u_captcha, data$u_search)
}

# Download lawsuit from a TJ that uses RGB captchas
download_rgb_lawsuit <- function(id, path, u_captcha, u_search) {

  # Try at most 10 times
  for (i in 1:10) {

    # Download captcha
    time_stamp <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
    f_captcha <- baixar_captcha_cor(u_captcha, path, time_stamp)

    # Create GET query
    query <- query_processo(id)
    query$uuidCaptcha <- uuid_captcha(f_captcha)
    query$vlCaptcha <- quebrar_captcha_cor(f_captcha)

    # Download lawsuit
    f_lwst <- sprintf("%s/%s.html", path, id)
    f_search <- httr::GET(u_search, query = query, httr::write_disk(f_lwst, TRUE))

    # Free temporary file
    file.remove(f_captcha)

    # Breaking condition
    if (!tem_captcha(f_search)) { return(f_lwst) }
    else { file.remove(f_lwst) }
  }
}

# Download a lawsuit from a TJ that uses B&W captchas
download_bw_lawsuit <- function(id, path, u_captcha, u_search) {

  # Aux function for breaking captcha
  break_captcha <- purrr::possibly(captchasaj::decodificar, 'xxxxx')

  # Try at most 10 times
  for (i in 1:10) {

    # Download captcha
    f_captcha <- tempfile()
    writeBin(httr::content(httr::GET(u_captcha), "raw"), f_captcha)

    # Create GET query
    query <- query_processo(id)
    query$vlCaptcha <- break_captcha(f_captcha, captchasaj::modelo$modelo)

    # Download lawsuit
    f_lwst <- sprintf("%s/%s.html", path, id)
    f_search <- httr::GET(u_search, query = query, httr::write_disk(f_lwst, TRUE))

    # Free temporary file
    file.remove(f_captcha)

    # Breaking condition
    if (!tem_captcha(f_search)) { return(f_lwst) }
    else { file.remove(f_lwst) }
  }
}
