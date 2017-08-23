tidy_nm <- function (x) {
  x %>% tolower() %>%
    abjutils::rm_accent() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("[^0-9a-z]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

parse_cjpg_um <- function(i, nodes) {
  node <- nodes[[i]]
  trim <- stringr::str_trim
  aux <- node %>%
    rvest::html_node(xpath = './/a[@title="Visualizar Inteiro Teor"]')
  id_processo <- aux %>% rvest::html_attr('name') %>% trim()
  n_processo <- aux %>% rvest::html_text() %>% trim() %>%
    stringr::str_replace_all('[^0-9]', '')
  keys <- node %>%
    rvest::html_nodes('table > tr > td > strong') %>%
    rvest::html_text() %>%
    trim() %>%
    tidy_nm()
  xp <- './/table/tr/td/strong/following-sibling::text()[1]'
  vals <- node %>%
    rvest::html_nodes(xpath = xp) %>%
    rvest::html_text() %>%
    trim()
  d_infos <- tibble::tibble(key = keys, val = vals) %>%
    dplyr::mutate(id = 1) %>%
    tidyr::spread(key, val) %>%
    dplyr::select(-id)
  txt <- node %>%
    rvest::html_node(xpath = './/table//div[@style="display: none;"]') %>%
    rvest::html_text() %>%
    trim()
  tab_infos <- tibble::tibble(n_processo = n_processo,
                              id_processo = id_processo) %>%
    dplyr::bind_cols(d_infos) %>%
    dplyr::mutate(txt = txt)
  tab_infos
}

parse_cjpg_arq <- function(arq) {
  itens <- xml2::read_html(arq, encoding = 'UTF-8') %>%
    rvest::html_nodes('.fundocinza1')
  abjutils::dvec(parse_cjpg_um, 1:length(itens),
                 nodes = itens, verbose = FALSE) %>%
    dplyr::select(-item)
}

#' Parser do CJPG
#'
#' Parser dos arquivos HTML baixados pela fun????o \code{\link{cjpg}}.
#'
#' @param arqs vetor de arquivos (caminho completo) a serem lidos.
#'
#' @return tibble com as colunas
#' \itemize{
#'   \item \code{arq} nome do arquivo lido.
#'   \item \code{id} id contido na p??gina lida.
#'   \item \code{cd_acordao} c??digo ??nico do ac??rd??o.
#'   \item \code{n_processo} n??mero do processo (pode repetir).
#'   \item \code{comarca} nome da comarca.
#'   \item \code{data_julgamento} data de julgamento em formato \%d/\%m/\%Y.
#'   \item \code{data_registro} data de registro no sistem em formato \%d/\%m/\%Y.
#'   \item \code{ementa} ementa do ac??rd??o (muitos vazios).
#'   \item \code{orgao_julgador} c??mara julgadora do recurso.
#'   \item \code{outros_numeros} n??meros antigos / complementares.
#'   \item \code{relatora} Nome do relator ou relatora do recurso.
#'   \item \code{classe_assunto} Classe / assunto, separados por " / ".
#'   \item \code{txt_ementa} Texto da ementa sem formata????o.
#' }
#' @export
parse_cjpg <- function(arqs) {
  abjutils::dvec(parse_cjpg_arq, arqs) %>%
    dplyr::rename(arq = item) %>%
    dplyr::distinct(id_processo, .keep_all = TRUE)
}
