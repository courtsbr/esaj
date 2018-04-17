# Get URLs for download depending on its TJ
get_lwst_data <- function(id, deg = 1) {

  # Switch base for URLs depending on TJ's number
  urls <- switch(get_n(id),
    "04" = list(u_captcha = "consultasaj.tjam", u_search = "consultasaj.tjam"),
    "05" = list(u_captcha = "esaj.tjba", u_search = "esaj.tjba"),
    "12" = list(u_captcha = "tjms", u_search = "tjms"),
    "24" = list(u_captcha = "esaj.tjsc", u_search = "esaj.tjsc"),
    "26" = list(u_captcha = "esaj.tjsp", u_search = "esaj.tjsp"))

  # Fill rest of URLs
  if (deg == 1) {
    urls$u_captcha <- stringr::str_c(
      "http://", urls$u_captcha, ".jus.br/cpopg/imagemCaptcha.do")
    urls$u_search <- stringr::str_c(
      "http://", urls$u_search, ".jus.br/cpopg/search.do")
  }else {
    if(get_n(id) %in% c("12")){
      urls$u_captcha <- stringr::str_c(
        "http://", urls$u_captcha, ".jus.br/cposg/imagemCaptcha.do")
      urls$u_search <- stringr::str_c(
        "http://", urls$u_search, ".jus.br/cposg5/search.do")
    }
    else{
      urls$u_captcha <- stringr::str_c(
      "http://", urls$u_captcha, ".jus.br/cposg/imagemCaptcha.do")
    urls$u_search <- stringr::str_c(
      "http://", urls$u_search, ".jus.br/cposg/search.do")
    }
  }

  return(urls)
}

# Get TJ's number
get_n <- function(id) {
  if (stringr::str_length(id) == 20) { stringr::str_sub(id, 15, 16) }
  else if (stringr::str_length(id) == 25) { stringr::str_sub(id, 19, 20) }
  else { stop("Ivalid ID") }
}

# Download lawsuit from a TJ that uses RGB captchas
download_rgb_lawsuit <- function(id, path, u_captcha, u_search, query) {

  # Try at most 10 times
  for (i in 1:10) {

    # Download captcha
    time_stamp <- stringr::str_replace_all(lubridate::now("Brazil/East"), "[^0-9]", "")
    f_captcha <- download_rgb_captcha(u_captcha, time_stamp)

    # Change GET query
    query$uuidCaptcha <- captcha_uuid(f_captcha)
    query$vlCaptcha <- break_rgb_captcha(f_captcha)

    # Download lawsuit
    today <- stringr::str_replace_all(lubridate::today("Brazil/East"), "-", "")
    f_lwst <- stringr::str_c(path, today, "_", id, ".html")
    f_search <- httr::GET(u_search, query = query, httr::write_disk(f_lwst, TRUE))

    # Free temporary file
    file.remove(f_captcha)

    # Breaking condition
    if (!has_captcha(f_search)) { return(f_lwst) }
    else { file.remove(f_lwst) }
  }
}

# Download a lawsuit from a TJ that uses B&W captchas
download_bw_lawsuit <- function(id, path, u_captcha, u_search, query) {

  # Aux function for breaking captcha
  break_bw_captcha <- purrr::possibly(captchasaj::decodificar, "xxxxx")

  # Try at most 10 times
  for (i in 1:10) {

    # Download captcha
    f_captcha <- tempfile()
    writeBin(httr::content(httr::GET(u_captcha), "raw"), f_captcha)

    # Change GET query
    query$vlCaptcha <- break_bw_captcha(f_captcha, captchasaj::modelo$modelo)

    # Download lawsuit
    today <- stringr::str_replace_all(lubridate::today("Brazil/East"), "-", "")
    f_lwst <- stringr::str_c(path, today, "_", id, ".html")
    f_search <- httr::GET(u_search, query = query, httr::write_disk(f_lwst, TRUE))

    # Free temporary file
    file.remove(f_captcha)

    # Breaking condition
    if (!has_captcha(f_search)) { return(f_lwst) }
    else { file.remove(f_lwst) }
  }
}

# Download a lawsuit from a TJ that uses no captcha system at all
download_noc_lawsuit <- function(id, path, u_captcha, u_search, query) {

  # Download page
  today <- stringr::str_replace_all(lubridate::today("Brazil/East"), "-", "")
  f_lwst <- stringr::str_c(path, today, "_", id, ".html")
  f_search <- httr::GET(u_search, query = query)

  # Get new links in page
  links <- f_search %>%
    httr::content() %>%
    xml2::xml_find_all("//*[@class='nuProcesso']") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  # If links exist, fetch them, otherwise just download the page
  if (length(links) != 0) {

    # Get descriptions of new files
    descs <- f_search %>%
      httr::content() %>%
      xml2::xml_find_all("//*[@id='listagemDeProcessos']") %>%
      rvest::html_text() %>%
      str_replace_all("[\\n\\t]", "") %>%
      stringr::str_split(" {40}") %>%
      purrr::pluck(1) %>%
      magrittr::extract(2:3) %>%
      str_replace_all("  ", " ") %>%
      stringr::str_trim()

    # Extract subjects and dates from descriptions
    subjects <- descs %>%
      rm_diacritics() %>%
      stringr::str_to_lower() %>%
      stringr::str_extract("[a-z ]+./") %>%
      stringr::str_extract("[a-z ]+") %>%
      stringr::str_trim() %>%
      str_replace_all(" ", "_")
    dates <- descs %>%
      stringr::str_extract("[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
      lubridate::dmy() %>%
      str_replace_all("-", "_")

    # Map downloads
    links <- stringr::str_c(
      str_replace_all(u_captcha, "/cposg/[a-zA-Z\\.]+$", ""), links)
    f_lwst <- stringr::str_c(path, today, "_", id, "_", subjects, "_", dates, ".html")
    purrr::map2(links, f_lwst, ~httr::GET(.x, httr::write_disk(.y, TRUE)))
  }
  else {

    # Download page
    f_search <- httr::GET(u_search, query = query, httr::write_disk(f_lwst, TRUE))
  }

  return(f_lwst)
}
