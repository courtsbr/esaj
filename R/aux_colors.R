filtrar_cor <- function(d, cor_nm) {
  cor_correta <- eval(call(tolower(cor_nm), d))
  dplyr::filter(d, cor == cor_correta)
}

laranja <- function(d) {
  d %>%
    dplyr::filter(b < .5) %>%
    dplyr::filter(r + g == max(r + g)) %>%
    head(1) %>%
    with(cor)
}

azul <- function(d) {
  d %>%
    dplyr::filter(r < .5, g < .8) %>%
    dplyr::filter(b == max(b)) %>%
    head(1) %>%
    with(cor)
}

verde <- function(d) {
  d %>%
    dplyr::filter(r < .5, b < .5) %>%
    dplyr::filter(g == max(g)) %>%
    head(1) %>%
    with(cor)
}

vermelho <- function(d) {
  d %>%
    dplyr::filter(b < .5, g < .5) %>%
    dplyr::filter(r == max(r)) %>%
    head(1) %>%
    with(cor)
}

rosa <- function(d) {
  d %>%
    dplyr::filter(g < .8) %>%
    dplyr::filter(r + b == max(r + b)) %>%
    head(1) %>%
    with(cor)
}

roxo <- function(d) {
  d %>%
    dplyr::filter(g < .5) %>%
    dplyr::filter(r + b == max(r + b)) %>%
    head(1) %>%
    with(cor)
}

preto <- function(d) {
  d %>%
    dplyr::filter(r + b + g == min(r + b + g)) %>%
    head(1) %>%
    with(cor)
}
