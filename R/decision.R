download_decision_tjsp <- function(cod_decision, path,
                                   ntry = 10, verbose = FALSE) {
  link <- 'https://esaj.tjsp.jus.br/cjsg/getArquivo.do'
  query <- list(cdAcordao = cod_decision, cdForo = 0)
  configs <- httr::config(ssl_verifypeer = FALSE)
  r0 <- httr::GET(link, query = query, config = configs)
  pdf_file <- sprintf('%s/%s.pdf', path, cod_decision)
  tentativas <- 0
  mime <- "application/pdf;charset=UTF-8"
  while (r0$headers[['content-type']] != mime && tentativas < ntry) {
    if (verbose) cat('quebrando captcha...\n')
    # Hasn't downloaded, there's a captcha
    time_stamp <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
    u_captcha <- 'https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do'
    arq_captcha <- download_rgb_captcha(u_captcha, time_stamp)
    query$conversationId <- ''
    query$cdForo <- '0'
    query$uuidCaptcha <- captcha_uuid(arq_captcha)
    query$vlCaptcha <- break_rgb_captcha(arq_captcha)
    query$novoVlCaptcha <- ''
    r0 <- httr::GET(link, query = query, config = configs)
    tentativas <- tentativas + 1
  }
  writeBin(r0$content, pdf_file)
  invisible(pdf_file)
}

#' @title Download decisions
#'
#' @description Download decisions from TJSP
#'
#' @param cod_decision Column cd_acordao of d_cjsg
#' @param tj TJ to download decisions (only works with TJSP for now)
#' @param path Path to the directory where the lawsuit should be downloaded
#'
#' @export
download_decisions <- function(cod_decision, tj = "tjsp", path = '.') {
  stopifnot(tj == "tjsp")
  safe_download <- purrr::possibly(download_decision_tjsp, '')
  p <- progress::progress_bar$new(total = length(cod_decision))
  purrr::walk(cod_decision, ~{
    safe_download(.x, path = path)
    p$tick()
  })
}
