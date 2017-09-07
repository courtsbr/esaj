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


#' @title Downloads table of CJPG items
#'
#' @description Downloads a table with the elements of class, subject and circuit to help with [cjpg()]
#'
#' @param tipo class, subject or circuit
#' @param tj TJ of the table (only works with TJSP for now)
#'
#' @return `data.frame` with titles and ID of the leaves and nodes of the table tree
#'
#' @export
cjpg_tables <- function(tipo = c('classe', 'assunto', 'varas'), tj = "tjsp") {
  stopifnot(tj == "tjsp")
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
