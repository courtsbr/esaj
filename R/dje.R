#' @title Download Diarios da Justica Eletronicos (DJEs) from a Tribunal
#' de Justica (TJ)
#'
#' @description
#' This function downloads Diarios da Justica Eletronicos
#' (Electronic Court Records) as PDFs. Given a Tribunal de Justica
#' (Justice Court), a character vector of dates, and the path to a
#' directory it will collect the DJEs published on these dates, and
#' save them to the provided directory.
#'
#' @section About DJEs:
#' Electronic Court Records are documents published almost
#' daily by every state's Justice Court containing important information
#' regarding new lawsuits being filed, decisions being made by judges,
#' and so on. Note that DJEs may be split into multiple "booklets"; when
#' this happens it means that each booklet contains information about
#' only one type of lawsuit.
#'
#' @section Implemented TJs:
#' Unfortunatelly `download_dje()` doesn't yet work with all 27 TJs in
#' Brazil. Here are the ones already implemented:
#' \itemize{
#'   \item TJAC: Acre
#'   \item TJAL: Alagoas
#'   \item TJAM: Amazonas
#'   \item TJBA: Bahia
#'   \item TJCE: Ceara
#'   \item TJMS: Mato Grosso do Sul
#'   \item TJRN: Rio Grande do Norte
#'   \item TJSC: Santa Catarina
#'   \item TJSP: Sao Paulo
#' }
#'
#' @param tj Abbreviation of a Justice Court (i.e. "TJSP")
#' @param dates A character vector of dates of the form *YYYY-MM-DD*
#' @param path Path to the directory where the DJEs should be downloaded
#' @param verbose Whether to print download statuses
#'
#' @return A character vector with the paths to the downloaded DJEs
#'
#' @examples
#' \dontrun{
#' download_dje("TJSP", Sys.Date())
#' download_dje("TJSC", Sys.Date() - 0:3, verbose = TRUE)
#' }
#'
#' @export
download_dje <- function(tj, dates = Sys.Date(), path = '.', verbose = FALSE) {

  # Collect TJ-specific data
  tj <- stringr::str_to_lower(tj)
  u_dje <- get_dje_data(tj)$u_dje
  booklets <- get_dje_data(tj)$booklets

  # Paths
  paths <- stringr::str_c(path, "/", tj, "_dje_", sort(dates))
  rep_paths <- rep(paths, each = length(booklets))

  # Create folders
  invisible(purrr::map(paths, dir.create, showWarnings = FALSE))

  # Download files
  results <- expand.grid(date = dates, booklet = booklets) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      date_link = format(lubridate::as_date(date), "%d/%m/%Y"),
      link = purrr::map2_chr(date, booklet, ~get_dje_link(tj, .x, u_dje, .y)),
      file = stringr::str_c(rep_paths, "/", tj, "_", booklet, "_", date, ".pdf")) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, booklet, date_link, link, file) %>%
    dplyr::do(download_pdf(.$link, .$file, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, booklet, link, file, result)

  return(results)
}

# Download DJE file
download_pdf <- function(u_dje, file, verbose) {

  # Don't download if file exists
  if (file.exists(file)) { return(dplyr::data_frame(result = "EXISTS")) }

  # Set verbose variables
  msg <- stringr::str_c("Downloading '", file, "'... ")
  bkspc <- stringr::str_c(rep("\b", stringr::str_length(msg) + 1), collapse = "")

  # Print download message
  if (verbose) { message(msg) }

  # Wrapper to download file
  download <- purrr::possibly(function(u, f, verbose) {

    # Download page
    GET <- purrr::safely(httr::GET)
    r <- GET(u, httr::write_disk(f), httr::config(ssl_verifypeer = FALSE))

    # Return TIMEOUT
    if(!is.null(r$error) && stringr::str_detect(r$error, "Timeout")) {
      return(dplyr::data_frame(result = "TIMEOUT"))
    }

    # Get content type
    r <- r$result
    ct <- httr::headers(r)[["content-type"]] %>%
      ifelse(is.null(.), "application", .)

    # Return OK
    if (r$status_code == 200 && stringr::str_detect(ct, "application")) {
      return(dplyr::data_frame(result = "OK"))
    }
  }, {

    # Return ERROR
    dplyr::data_frame(result = "ERROR")
  })

  # Collect result
  result <- download(u_dje, file, verbose)
  if (verbose) { message(bkspc, msg, result[[1]], "!") }

  return(result)
}
