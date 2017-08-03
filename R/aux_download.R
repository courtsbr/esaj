# Get URLs for download depending on its TJ
get_lwst_data <- function(id) {

  # Switch base for URLs depending on TJ's number
  urls <- switch(get_n(id),
    "04" = list(u_captcha = "consultasaj.tjam", u_search = "consultasaj.tjam"),
    "05" = list(u_captcha = "esaj.tjba", u_search = "esaj.tjba"),
    "24" = list(u_captcha = "esaj.tjsc", u_search = "esaj.tjsc"))

  # Fill rest of URLs
  urls$u_captcha <- stringr::str_c(
    "http://", urls$u_captcha, ".jus.br/cpopg/imagemCaptcha.do")
  urls$u_search <- stringr::str_c(
    "http://", urls$u_search, ".jus.br/cpopg/search.do")

  return(urls)
}

# Get TJ's number
get_n <- function(id) {
  if (stringr::str_length(id) == 20) { stringr::str_sub(id, 15, 16) }
  else if (stringr::str_length(id) == 25) { stringr::str_sub(id, 19, 20) }
  else { stop("Ivalid ID") }
}

# Get data for downloading DJE depending on its TJ
get_dje_data <- function(tj) {
  switch (tj,
  "tjsp" = list(
    u_dje = "http://www.dje.tjsp.jus.br/cdje/downloadCaderno.do?",
    booklets = c(11:15, 18)),
  "tjal" = list(
    u_dje = "http://www2.tjal.jus.br/cdje/downloadCaderno.do?",
    booklets = c(2, 3)),
  "tjam" = list(
    u_dje = "http://esaj.tjam.jus.br/cdje/downloadCaderno.do?",
    booklets = c(1:3)),
  "tjce" = list(
    u_dje = "http://esaj.tjce.jus.br/cdje/downloadCaderno.do?",
    booklets = c(1:2)),
  "tjac" = list(
    u_dje = "",
    booklets = c(1)),
  "tjms" = list(
    u_dje = "http://www.tjms.jus.br/cdje/downloadCaderno.do?",
    booklets = c(1)))
}

# Download DJE file
download_arq <- function(u, a, verbose = FALSE) {
  if (file.exists(a)) {
    if (verbose) cat('\narquivo ',  a, ' ja existe!\n')
    return(dplyr::data_frame(result = 'exists'))
  }
  if (verbose) cat('\nbaixando ', a, '...', sep = '')
  res <- tryCatch({
    r <- suppressWarnings({
      httr::GET(u, httr::write_disk(a, overwrite = TRUE),
                httr::config(ssl_verifypeer = FALSE))
    })
    ct <- httr::headers(r)[['content-type']]
    ct <- ifelse(is.null(ct), 'application', ct)
    if (httr::status_code(r) == 200 && stringr::str_detect(ct, 'application')) {
      if (verbose) cat('OK!\n')
      return(dplyr::data_frame(result = 'ok'))
    }
  }, error = function(e) as.character(e))
  if (stringr::str_detect(res, 'Timeout')) {
    if (verbose) cat('ERRO!\n')
    return(dplyr::data_frame(result = 'timeout'))
  }
  if (verbose) cat('ERRO!\n')
  return(dplyr::data_frame(result = 'invalid dje'))
}
