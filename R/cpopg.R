#' @title Download lawsuits filed in Brazilian Tribinais de Justica
#' (Justice Courts)
#'
#' @description
#' This function downloads lawsuits as PDFs. Given a lawsuit ID, and
#' the path to a directory it will collect the lawsuit, and save it to
#' the provided directory.
#'
#' @section About lawsuits:
#' The lawsuits contemplated by this funtion have to be filed in a
#' Brazilian Tribunal de Justica (Justice Court). [download_lawsuit()]
#' finds the lawsuit in its state's online Sistema de Automacao de Justica
#' (Justice Automation System), solves the captcha withholding the
#' information, and collects the PDF.
#'
#' @section Implemented TJs:
#' Unfortunatelly [download_lawsuit()] doesn't yet work with all 27 TJs in
#' Brazil. Here are the ones already implemented:
#' \itemize{
#'   \item TJAM (Amazonas)
#'   \item TJBA (Bahia)
#'   \item TJSC (Santa Catarina)
#'   \item TJSP (Sao Paulo)
#' }
#'
#' @param id A lawsuit's unique identifier
#' @param path Path to the directory where the lawsuit should be downloaded
#'
#' @return A character vector with the path to the downloaded lawsuit
#'
#' @examples
#' \dontrun{
#' download_lawsuit("02575182220138040001")
#' download_lawsuit("0303349-44.2014.8.24.0020")
#' download_lawsuit("0552486-62.2015.8.05.0001")
#' }
#'
#' @export
download_lawsuit <- function(id, path = ".") {

  # Normalize path
  path <- normalizePath(path) %>%
    stringr::str_c("/")

  # Strip ID down
  id <- stringr::str_replace_all(id, "[^0-9]", "")
  if (stringr::str_length(id) != 20) { stop("Invalid ID") }

  # Choose appropriate download function
  if (get_n(id) %in% c("05")) { download <- download_bw_lawsuit }
  else if (get_n(id) %in% c("26")) { download <- download_noc_lawsuit }
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
    f_captcha <- download_rgb_captcha(u_captcha, time_stamp)

    # Create GET query
    query <- lawsuit_query(id)
    query$uuidCaptcha <- captcha_uuid(f_captcha)
    query$vlCaptcha <- break_rgb_captcha(f_captcha)

    # Download lawsuit
    f_lwst <- stringr::str_c(path, id, ".html")
    f_search <- httr::GET(u_search, query = query, httr::write_disk(f_lwst, TRUE))

    # Free temporary file
    file.remove(f_captcha)

    # Breaking condition
    if (!has_captcha(f_search)) { return(f_lwst) }
    else { file.remove(f_lwst) }
  }
}

# Download a lawsuit from a TJ that uses B&W captchas
download_bw_lawsuit <- function(id, path, u_captcha, u_search) {

  # Aux function for breaking captcha
  break_bw_captcha <- purrr::possibly(captchasaj::decodificar, "xxxxx")

  # Try at most 10 times
  for (i in 1:10) {

    # Download captcha
    f_captcha <- tempfile()
    writeBin(httr::content(httr::GET(u_captcha), "raw"), f_captcha)

    # Create GET query
    query <- lawsuit_query(id)
    query$vlCaptcha <- break_bw_captcha(f_captcha, captchasaj::modelo$modelo)

    # Download lawsuit
    f_lwst <- stringr::str_c(path, id, ".html")
    f_search <- httr::GET(u_search, query = query, httr::write_disk(f_lwst, TRUE))

    # Free temporary file
    file.remove(f_captcha)

    # Breaking condition
    if (!has_captcha(f_search)) { return(f_lwst) }
    else { file.remove(f_lwst) }
  }
}

# Download a lawsuit from a TJ that uses no captcha system at all
download_noc_lawsuit <- function(id, path, u_captcha, u_search) {

  # Create GET query
  query <- lawsuit_query(id)

  # Download lawsuit
  f_lwst <- stringr::str_c(path, id, ".html")
  f_search <- httr::GET(u_search, query = query, httr::write_disk(f_lwst, TRUE))

  return(f_lwst)
}
