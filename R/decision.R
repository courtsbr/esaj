
#' Download decision PDFs
#' @param id A character vector with decision IDs
#' @param path Path to directory where to save PDFs
#' @param tj TJ to download decisions (only works with TJSP for now)
#' @export
download_decision <- function(id, path, tj = "tjsp") {

  # Stop if TJ isn't TJSP
  stopifnot(tj == "tjsp")

  # Create directory if necessary
  dir.create(path, FALSE, TRUE)

  # Download decisions
  dwld <- purrr::possibly(download_decision_, "")
  p <- progress::progress_bar$new(total = length(id))
  purrr::map_chr(id, ~{ dwld(.x, path); p$tick() })
}

download_decision_ <- function(id, path, ntry = 10, verbose = FALSE) {

  # Download page with captcha
  captcha <- httr::GET(
    "https://esaj.tjsp.jus.br/cjsg/getArquivo.do",
    query = list(cdAcordao = id, cdForo = 0),
    httr::config(ssl_verifypeer = FALSE))

  # File where to save PDF
  file <- stringr::str_c(normalizePath(path), "/", id, ".pdf")

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
      cdAcordao = id,
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
