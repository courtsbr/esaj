baixar_captcha_cor_tjsp <- function(dir = '.', ts = '') {
  tmp <- tempfile()
  r <- httr::POST(
    'https://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do',
    body = list(timestamp = ts, uuidCaptcha = '', conversationId = ''),
    httr::config(ssl_verifypeer = FALSE),
    httr::write_disk(tmp, overwrite = TRUE)
  )
  invisible(tmp)
}

download_decision_tjsp <- function(cod_decision, path,
                                   ntry = 10, verbose = FALSE) {
  link <- 'https://esaj.tjsp.jus.br/cjsg/getArquivo.do'
  query <- list(cdAcordao = cod_decision, cdForo = 0)
  configs <- httr::config(ssl_verifypeer = FALSE)
  r0 <- httr::GET(link, query = query, config = configs)
  pdf_file <- sprintf('%s/%s.pdf', path, cod_decision)
  tentativas <- 0
  while (r0$headers[['content-type']] != "application/pdf;charset=UTF-8" &&
         tentativas < ntry) {
    if (verbose) cat('quebrando captcha...\n')
    # nao baixou, tem captcha
    time_stamp <- stringr::str_replace_all(lubridate::now(), "[^0-9]", "")
    arq_captcha <- baixar_captcha_cor_tjsp(dir_processo, time_stamp)
    query$conversationId <- ''
    query$cdForo <- '0'
    query$uuidCaptcha <- uuid_captcha(arq_captcha)
    query$vlCaptcha <- quebrar_captcha_cor(arq_captcha)
    query$novoVlCaptcha <- ''
    r0 <- httr::GET(link, query = query, config = configs)
    tentativas <- tentativas + 1
  }
  writeBin(r0$content, pdf_file)
  invisible(pdf_file)
}

#' Download decisions
#'
#' Download decisions from TJSP
#'
#' @param cod_decision column cd_acordao of d_cjsg
#' @param path folder
#'
#' @export
download_decisions_tjsp <- function(cod_decision, path = '.') {
  safe_download <- purrr::possibly(download_decision_tjsp, '')
  p <- progress::progress_bar$new(total = length(cod_decision))
  purrr::walk(cod_decision, ~{
    safe_download(.x, path = path)
    p$tick()
  })
}
