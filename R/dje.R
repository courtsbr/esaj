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

  # Set different types of DJE collection
  type_a <- c("tjsp", "tjal", "tjam", "tjce")

  # Run appropriate functions to download
  if (tj %in% type_a) {
    results <- dje_a(data$u_dje, dates, path, tj, data$booklet, verbose)
  } else {
    f <- sprintf('dje_%s', tj)
    eval(call(f, dates, path, verbose))
  }
}

# @rdname dje
#
# @param from,to Date vector ou character vector em formato YYYY-MM-DD.
# @inheritParams dje
#
# @export
dje_range <- function(from, to, tj = 'TJSP', path = 'data-raw/dje_pdf',
                      verbose = FALSE) {
  dates <- seq(as.Date(from), as.Date(to), by = 1)
  dje(tj, dates, path, verbose)
}

dje_a <- function(u_dje, dates, path, tj, booklets, verbose) {

  # Safe function for downloading files
  dwld <- purrr::possibly(download_arq, dplyr::data_frame(result = "error"))

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
      date_link = format(as.Date(date), "%d/%m/%Y"),
      link = stringr::str_c(u_dje, "dtDiario=", date_link, "&cdCaderno=", booklet),
      file = stringr::str_c(rep_paths, "/tjsp_dje_", booklet, "_", date, ".pdf")) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, booklet, date_link, link, file) %>%
    dplyr::do(dwld(.$link, .$file, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, booklet, link, file, result)

  return(results)
}

dje_tjac <- function(dates, path, verbose) {


  pastas <- sprintf('%s/tjac_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(1),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(link = sapply(date, link_tjac),
                  arq = sprintf('%s/tjac_dje_%s_%s.pdf',
                                rep(pastas, each = 1), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
}

dje_tjms <- function(dates, path, verbose) {
  # obs: lento!!!

  pastas <- sprintf('%s/tjms_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(1),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sapply(date_link, link_tjms),
                  arq = sprintf('%s/tjms_dje_%s_%s.pdf', rep(pastas, each = 1), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

dje_tjrn <- function(dates, path, verbose) {


  pastas <- sprintf('%s/tjrn_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))


  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- dplyr::data_frame(date = dates) %>%
    dplyr::group_by(date) %>%
    dplyr::do(link_tjrn(.$date)) %>%
    dplyr::ungroup() %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(arq = sprintf('%s/tjrn_dje_%s_%s.pdf',
                                rep(pastas, each = 2), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

dje_tjsc <- function(dates, path, verbose) {
  pastas <- sprintf('%s/tjsc_dje_%s', path, sort(dates))


  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))


  if (verbose) cat('obtendo links...\n')

  tjsc_link(date_link[3])

  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(c(1)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = plyr::laply(date_link, tjsc_link, .progress = 'text'),
                  arq = sprintf('%s/tjsc_dje_%s_%s.pdf', rep(pastas, each = 1), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

# @export
dje_tjba <- function(dates, path, verbose) {

  dates <- as.Date(dates)
  u <- 'http://www.tjba.jus.br/diario/internet/download.wsp?'
  pastas <- sprintf('%s/tjba_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  aux <- edicoes_tjba()
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(1),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::left_join(aux, 'date') %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(link = sprintf('%stmp.diario.nu_edicao=%s', u, edicao, caderno),
                  arq = sprintf('%s/tjba_dje_%s_%s.pdf', rep(pastas, each = 1), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}
