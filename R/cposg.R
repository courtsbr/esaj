
#' Download second degree lawsuits filed in Brazilian Tribinais de Justica
#' (Justice Courts)
#'
#' @section Implemented TJs:
#' Unfortunatelly [download_lawsuit()] doesn't yet work with all 27 TJs in
#' Brazil. Here are the ones already implemented:
#' \itemize{
#'   \item TJSP (Sao Paulo)
#' }
#'
#' @param id A character vector of one or more lawsuit IDs (only works with
#' TJSP for now)
#' @param path Path to the directory where the lawsuit should be saved
#'
#' @return A character vector with the path to the downloaded lawsuit
#'
#' @examples
#' \dontrun{
#' download_2deg_lawsuit("1001869-51.2017.8.26.0562")
#' }
#'
#' @export
download_2deg_lawsuit <- function(id, path = ".") {

  # Normalize path
  dir.create(path, FALSE, TRUE)
  path <- normalizePath(path) %>%
    stringr::str_c("/")

  # Strip ID down
  id <- stringr::str_replace_all(id, "[^0-9]", "")
  if (any(stringr::str_length(id) != 20)) { stop("Invalid ID") }

  # Iterate over IDs
  purrr::map_chr(id, download_2deg_lawsuit_, path)
}

# Download one lawsuit
download_2deg_lawsuit_ <- function(id, path) {

  # Choose appropriate download function
  if (get_n(id) %in% c("26")) { download <- download_noc_lawsuit }
  else { stop("ID must refer to a TJSP lawsuit") }

  # Get URLs for the download
  data <- get_lwst_data(id, deg = 2)

  # Download lawsuit
  download(id, path, data$u_captcha, data$u_search, lawsuit_2deg_query(id))
}
