#' Se????es de segunda inst??ncia.
#'
#' Baixa lista das se????es de segunda inst??ncia.
#'
#' @return \code{tibble} com as colunas \code{pai}, \code{secao} e \code{cod}. Usualmente \code{cod} ser?? usado na fun????o \code{\link{cjsg_parms}}
#'
#' @export
list_secoes_2inst <- function() {
  u <- 'https://esaj.tjsp.jus.br/cjsg/secaoTreeSelect.do?campoId=secoes'
  xp_child <- './/li[@class="leafItem"]//span[contains(@id, "secoes_tree")]'
  xp_parent <- './/span[contains(@id, "secoes_tree")]'
  u %>%
    httr::GET(httr::config(ssl_verifypeer = FALSE)) %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_nodes('li.open') %>%
    lapply(function(x) {
      pai <- x %>% rvest::html_node(xpath = xp_parent) %>% rvest::html_text()
      x %>%
        rvest::html_nodes(xpath = xp_child) %>%
        purrr::map(~tibble::tibble(
          cod = rvest::html_attr(.x, 'value'),
          secao = rvest::html_text(.x))
        ) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(pai = pai)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(pai, secao, cod)
}

get_obj_2inst <- function(tipo) {
  u_base <- 'https://esaj.tjsp.jus.br/cjsg/%sTreeSelect.do?campoId=%s'
  r <- httr::GET(sprintf(u_base, tipo[1], tipo[1]),
                 httr::config(ssl_verifypeer = FALSE))
}

#' Baixa tabela de itens da CJPG.
#'
#' Baixa uma tabela com os elementos de classe, assunto ou varas, para ajudar a baixar usando a fun????o CJPG.
#'
#' @param tipo classe, assunto ou varas
#'
#' @return \code{data.frame} com titulos e cods das folhas e titulos e cods dos pais (??ndice 0 ?? a raiz e quanto menor o ??ndice, mais pr??ximo da raiz)
#'
#' @export
cjsg_tabs <- function(tipo = c('classes', 'assuntos')) {
  r <- get_obj_2inst(tipo)
  tree <- r %>%
    httr::content('text') %>%
    XML::htmlParse(encoding = 'UTF-8') %>%
    XML::getNodeSet('//div[@class="treeView"]') %>%
    lapply(function(x) XML::xmlToList(x)) %>%
    dplyr::first() %>%
    dplyr::nth(2) %>%
    purrr::keep(~is.list(.x))
  tree_to_tibble(tree) %>%
    dplyr::select(dplyr::ends_with('leaf'),
                  dplyr::ends_with('0'),
                  dplyr::everything()) %>%
    dplyr::mutate(titulo0 = ifelse(is.na(titulo0), titulo_leaf, titulo0),
                  cod0 = ifelse(is.na(cod0), titulo_leaf, cod0))
}
