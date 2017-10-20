
#' @title Download PDFs corresponding to judicial decisions
#'
#' @description Given the unique identifier of a judicial decision
#' (an 8 digit number), this function downloads the corresponding
#' PDF.
#'
#' @param decision A character vector with decision IDs
#' @param path Path to directory where to save PDF
#' @param tj TJ from which to get data (only works with TJSP for now)
#' @return A character vector with the path to the downloaded file
#'
#' @export
download_decision <- function(decision, path = ".", tj = "tjsp") {

  # Stop if TJ isn't TJSP
  stopifnot(tj == "tjsp")

  # Create directory if necessary
  dir.create(path, FALSE, TRUE)

  # Download decisions
  dwld <- purrr::possibly(download_decision_, "")
  pb <- progress::progress_bar$new(
    "Downloading [:bar] :percent eta: :eta", length(decision))
  downloaded <- c()
  for (i in seq_along(decision)) {
    downloaded <- append(downloaded, dwld(decision[i], path))
    pb$tick()
  }

  return(downloaded)
}

download_decision_ <- function(decision, path, ntry = 10, verbose = FALSE) {

  # Download page with captcha
  captcha <- httr::GET(
    "https://esaj.tjsp.jus.br/cjsg/getArquivo.do",
    query = list(cdAcordao = decision, cdForo = 0),
    httr::config(ssl_verifypeer = FALSE))

  # File where to save PDF (and return early if it exists)
  file <- stringr::str_c(normalizePath(path), "/", decision, ".pdf")
  if (file.exists(file)) { return(file) }

  # Try to download PDF at most ntry times
  for (i in 1:ntry) {

    # Message
    if (verbose) { message("Breaking captcha...") }

    # Download captcha itself
    time_stamp <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
    u_captcha <- "https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do"
    f_captcha <- download_rgb_captcha(u_captcha, time_stamp)

    # Query for GET request
    query_get <- list(
      conversationId = "",
      cdAcordao = decision,
      cdForo = 0,
      uuidCaptcha = captcha_uuid(f_captcha),
      vlCaptcha = break_rgb_captcha(f_captcha),
      novoVlCaptcha = "")

    # Try to open PDF
    pdf <- httr::GET(
      "https://esaj.tjsp.jus.br/cjsg/getArquivo.do",
      query = query_get, httr::config(ssl_verifypeer = FALSE))

    # Captcha was broken
    mime <- "application/pdf;charset=UTF-8"
    if (pdf$headers[["content-type"]] == mime) {
      writeBin(pdf$content, file); return(file)
    }
  }

  return("")
}
