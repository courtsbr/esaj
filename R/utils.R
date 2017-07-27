
# Desacentuar uma string
desacentuar <- function(x) {
  gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
}

# Remover fundo e riscos de uma imagem
tirar_fundo_e_riscos <- function(img) {
  img %>%
    dplyr::filter(y > 15) %>%
    dplyr::group_by(cor) %>%
    dplyr::mutate(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n < max(n)) %>%
    dplyr::filter(n > sort(unique(n), decreasing = TRUE)[3])
}

# Detectar a existência de um captcha no processo
tem_captcha <- function(arq) {
  (arq %>%
     httr::content('text', encoding = "ISO-8859-1") %>%
     xml2::read_html() %>%
     rvest::html_nodes('#captchaCodigo') %>%
     length()) > 0
}
tem_captcha <- purrr::possibly(tem_captcha, TRUE)

# Obter o uuid de um captcha
uuid_captcha <- function(arq) {
  jsonlite::fromJSON(arq)$uuidCaptcha
}
uuid_captcha <- purrr::possibly(uuid_captcha, "xxxx")

# Criar um query de processo
query_processo <- function(p) {
  list('conversationId' = '',
       'dadosConsulta.localPesquisa.cdLocal' = '-1',
       'cbPesquisa' = 'NUMPROC',
       'dadosConsulta.tipoNuProcesso' = 'UNIFICADO',
       'numeroDigitoAnoUnificado' = stringr::str_sub(p, 1, 15),
       'foroNumeroUnificado' = stringr::str_sub(p, -4, -1),
       'dadosConsulta.valorConsultaNuUnificado' = p,
       'dadosConsulta.valorConsulta' = '',
       'uuidCaptcha' = '',
       'vlCaptcha' = '',
       'novoVlCaptcha' = '')
}

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %>%
NULL

#' Double-pipe operator
#'
#' See \code{\link[magrittr]{\%<>\%}} for more details.
#'
#' @name %<>%
#' @rdname pipe
#' @keywords internal
#' @importFrom magrittr %<>%
NULL

"%||%" <- function(x, y) {
  if (rlang::is_null(x)) y else x
}

globalVariables(c(
  ".", "Documento", "X1", "X2", "X3", "adv", "arq", "b", "caderno",
  "cor", "date_link", "desc", "edicao", "forma", "g", "head", "id",
  "info", "key", "link", "n", "n_processo", "nome", "r", "result",
  "rm_accent", "setNames", "value", "y"))
