desacentuar <- function(x) {
  gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
}
