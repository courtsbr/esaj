# Baixa diários oficiais
#
# Acessa os Diários de Justiça Eletrônicos dos Tribunais de Justiça e baixa
# os arquivos em PDF.
#
# @param tj character vector indicando o Tribunal. Atualmente funciona com
# TJSP, TJAC, TJAL, TJAM, TJMS, TJRN, TJSC, TJCE, TJBA. Default \code{'TJSP'}.
# @param dates Date vector ou character vector em YYYY-MM-DD com as datas
# que se deseja baixar. Default \code{Sys.Date()}.
# @param path pasta onde os arquivos serão gravados. Para cada data, uma pasta
# será criada e os arquivos PDF serão salvos nessa pasta. Default \code{'data-raw/dje_pdf'}
# @param verbose imprimir mensagens? Default \code{FALSE}.
#
# @return \code{tbl_df} com diagnóstico dos resultados.
#
# @examples
# \dontrun{
# dir.create('data-raw/dje_pdf', recursive = TRUE, showWarnings = FALSE)
# tjsp_dje <- dje(dates = Sys.Date() - 0:3)
# table(tjsp_dje$result)
#
# # --------------------------------------------------------------------------
# tjal_dje <- dje(tj = 'TJAL', dates = Sys.Date() - 0:3)
# tjam_dje <- dje(tj = 'TJAM', dates = Sys.Date() - 0:3)
# tjce_dje <- dje(tj = 'TJCE', dates = Sys.Date() - 0:3)
# tjba_dje <- dje(tj = 'TJBA', dates = Sys.Date() - 0:3)
# tjms_dje <- dje(tj = 'TJMS', dates = Sys.Date() - 0:3)
# tjsc_dje <- dje(tj = 'TJSC', dates = Sys.Date() - 0:3)
# tjrn_dje <- dje(tj = 'TJRN', dates = Sys.Date() - 0:3)
# tjac_dje <- dje(tj = 'TJAC', dates = Sys.Date() - 0:3)
# }
# # @export
download_dje <- function(tj, dates = Sys.Date(), path = '.', verbose = FALSE) {

  # Collect TJ-specific data
  tj <- stringr::str_to_lower(tj)
  data <- get_dje_data(tj)

  # Paths
  paths <- stringr::str_c(path, "/", tj, "_dje_", sort(dates))
  rep_paths <- rep(paths, each = length(booklets))

  # Create folders
  invisible(purrr::map(paths, dir.create, showWarnings = FALSE))

  # Download files
  results <- expand.grid(date = dates, booklet = booklets) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      date_link = format(lubridate::as_date(date), "%d/%m/%Y"),
      link = purrr::map2_chr(date, booklet, ~get_dje_link(tj, .x, u_dje, .y)),
      file = stringr::str_c(rep_paths, "/", tj, booklet, "_", date, ".pdf")) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, booklet, date_link, link, file) %>%
    dplyr::do(download_pdf(.$link, .$file, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, booklet, link, file, result)

  return(results)
}
