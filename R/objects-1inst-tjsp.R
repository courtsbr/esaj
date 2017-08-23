get_obj <- function(tipo) {
  u_base <- 'https://esaj.tjsp.jus.br/cjpg/%sTreeSelect.do?campoId=%s'
  r <- httr::GET(sprintf(u_base, tipo[1], tipo[1]), httr::config(ssl_verifypeer = FALSE))
}

tree_to_tibble <- function(tree, n = 0) {
  tree <- tree[names(tree) == 'li']
  f1 <- dplyr::failwith('', function(.x) {
    .x[[2]][['text']]
  }, quiet = TRUE)
  f2 <- dplyr::failwith('', function(.x) {
    .x[[2]][['.attrs']][['value']]
  }, quiet = TRUE)
  titulos <- purrr::map_chr(tree, f1)
  titulos <- titulos[titulos != '']
  cods <- purrr::map_chr(tree, f2)
  cods <- cods[cods != '']
  ns <- purrr::map_int(tree, ~length(.x))[seq_along(cods)]
  purrr::map(seq_along(ns), function(i) {
    if (ns[i] == 4) {
      dplyr::data_frame(titulo_leaf = titulos[i], cod_leaf = cods[i])
    } else {
      x <- tree_to_tibble(tree[[i]][[4]], n + 1) %>%
        dplyr::bind_rows()
      x[[paste0('titulo', n)]] <- titulos[i]
      x[[paste0('cod', n)]] <- cods[i]
      x
    }
  }) %>%
    dplyr::bind_rows()
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
cjpg_tabs <- function(tipo = c('classe', 'assunto', 'varas')) {
  r <- get_obj(tipo)
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
