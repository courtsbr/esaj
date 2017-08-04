
# Filter captcha's requested color
filter_color <- function(img_df, color_nm) {
  right_color <- eval(call(tolower(color_nm), img_df)) %>%
    head(1) %>% with(color)
  dplyr::filter(img_df, color == right_color)
}

# Orange
laranja <- function(d) {
  dplyr::filter(d, b < .5) %>%
    dplyr::filter(r + g == max(r + g))
}

# Blue
azul <- function(d) {
  dplyr::filter(d, r < .5, g < .8) %>%
    dplyr::filter(b == max(b))
}

# Green
verde <- function(d) {
  dplyr::filter(d, r < .5, b < .5) %>%
    dplyr::filter(g == max(g))
}

# Red
vermelho <- function(d) {
  dplyr::filter(d, b < .5, g < .5) %>%
    dplyr::filter(r == max(r))
}

# Pink
rosa <- function(d) {
  dplyr::filter(d, g < .8) %>%
    dplyr::filter(r + b == max(r + b))
}

# Purple
roxo <- function(d) {
  dplyr::filter(d, g < .5) %>%
    dplyr::filter(r + b == max(r + b))
}

# Black
preto <- function(d) {
  dplyr::filter(d, r + b + g == min(r + b + g))
}
