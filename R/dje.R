#' Baixa diários oficiais
#'
#' Acessa os Diários de Justiça Eletrônicos dos Tribunais de Justiça e baixa
#' os arquivos em PDF.
#'
#' @param tj character vector indicando o Tribunal. Atualmente funciona com
#' TJSP, TJAC, TJAL, TJAM, TJMS, TJRN, TJSC, TJCE, TJBA. Default \code{'TJSP'}.
#' @param dates Date vector ou character vector em YYYY-MM-DD com as datas
#' que se deseja baixar. Default \code{Sys.Date()}.
#' @param path pasta onde os arquivos serão gravados. Para cada data, uma pasta
#' será criada e os arquivos PDF serão salvos nessa pasta. Default \code{'data-raw/dje_pdf'}
#' @param verbose imprimir mensagens? Default \code{FALSE}.
#'
#' @return \code{tbl_df} com diagnóstico dos resultados.
#'
#' @examples
#'
#' dir.create('data-raw/dje_pdf', recursive = TRUE, showWarnings = FALSE)
#' tjsp_dje <- dje(dates = Sys.Date() - 0:3)
#' table(tjsp_dje$result)
#'
#' # --------------------------------------------------------------------------
#' tjal_dje <- dje(tj = 'TJAL', dates = Sys.Date() - 0:3)
#' tjam_dje <- dje(tj = 'TJAM', dates = Sys.Date() - 0:3)
#' tjce_dje <- dje(tj = 'TJCE', dates = Sys.Date() - 0:3)
#' tjba_dje <- dje(tj = 'TJBA', dates = Sys.Date() - 0:3)
#' tjms_dje <- dje(tj = 'TJMS', dates = Sys.Date() - 0:3)
#' tjsc_dje <- dje(tj = 'TJSC', dates = Sys.Date() - 0:3)
#' tjrn_dje <- dje(tj = 'TJRN', dates = Sys.Date() - 0:3)
#' tjac_dje <- dje(tj = 'TJAC', dates = Sys.Date() - 0:3)
#'
#' @export
dje <- function(tj = 'TJSP', dates = Sys.Date(), path = 'data-raw/dje_pdf',
                verbose = FALSE) {
  f <- sprintf('dje_%s', tolower(tj))
  eval(call(f, dates, path, verbose))
}

#' @rdname dje
#'
#' @param from,to Date vector ou character vector em formato YYYY-MM-DD.
#' @inheritParams dje
#'
#' @export
dje_range <- function(from, to, tj = 'TJSP', path = 'data-raw/dje_pdf',
                      verbose = FALSE) {
  dates <- seq(as.Date(from), as.Date(to), by = 1)
  dje(tj, dates, path, verbose)
}

dje_tjsp <- function(dates, path, verbose = FALSE) {
  u <- 'http://www.dje.tjsp.jus.br/cdje/downloadCaderno.do?'
  pastas <- sprintf('%s/tjsp_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(c(11:15, 18)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sprintf('%sdtDiario=%s&cdCaderno=%s', u, date_link, caderno),
                  arq = sprintf('%s/tjsp_dje_%s_%s.pdf', rep(pastas, each = 6), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

dje_tjac <- function(dates, path, verbose) {

  pega_link <- function(d) {
    d <- as.Date(d)
    u <- sprintf('http://diario.tjac.jus.br/edicoes.php?Ano=%d&Mes=%d',
                 lubridate::year(d), lubridate::month(d))
    r <- httr::GET(u)
    l <- r %>%
      httr::content('text') %>%
      xml2::read_html() %>%
      rvest::html_nodes('table tr.texto_normal') %>% {
        date <- rvest::html_node(., 'a.texto_normal') %>%
          rvest::html_text() %>%
          stringr::str_trim() %>%
          lubridate::dmy() %>%
          as.Date()
        link <- rvest::html_node(., xpath = './/a[@title="Baixar"]') %>%
          rvest::html_attr('href') %>%
          {paste0('http://diario.tjac.jus.br', .)}
        dplyr::data_frame(date, link)
      } %>%
      dplyr::filter(date %in% d) %>%
      with(link)
    if (length(l) == 0) return('http://diario.tjac.jus.br/edicoes.php')
    l
  }
  pastas <- sprintf('%s/tjac_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(1),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(link = sapply(date, pega_link),
                  arq = sprintf('%s/tjac_dje_%s_%s.pdf',
                                rep(pastas, each = 1), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
}

dje_tjal <- function(dates, path, verbose) {
  u <- 'http://www2.tjal.jus.br/cdje/downloadCaderno.do?'
  pastas <- sprintf('%s/tjal_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(c(2,3)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sprintf('%sdtDiario=%s&cdCaderno=%s', u, date_link, caderno),
                  arq = sprintf('%s/tjal_dje_%s_%s.pdf', rep(pastas, each = 2), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)

}

dje_tjam <- function(dates, path, verbose) {
  u <- 'http://esaj.tjam.jus.br/cdje/downloadCaderno.do?'
  pastas <- sprintf('%s/tjam_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(c(1:3)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sprintf('%sdtDiario=%s&cdCaderno=%s', u, date_link, caderno),
                  arq = sprintf('%s/tjam_dje_%s_%s.pdf', rep(pastas, each = 3), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

dje_tjms <- function(dates, path, verbose) {
  # obs: lento!!!
  pega_link <- function(d) {
    q <- list(
      'calendarCurrentDate' = d,
      'opt' = 'doCalendarSearch',
      'number' = '',
      'startDate' = '',
      'endDate' = '',
      'searchText' = '',
      'searchTextType' = 'exact'
    )
    u <- 'https://www.tjms.jus.br/DailyWeb/dailyAction.do?'
    u0 <- 'http://www.tjms.jus.br/webfiles/producao/GP/diarios/'
    r <-  httr::GET(u, query = q, httr::config(ssl_verifypeer = FALSE))
    nao_tem <- r %>%
      httr::content('text') %>%
      stringr::str_detect('Nothing found to display')
    if (nao_tem) return(u0)
    if (r$status_code != 200) return(r$status_code)
    r %>%
      httr::content('text') %>%
      xml2::read_html() %>%
      rvest::html_node('#daily tbody a') %>%
      rvest::html_attr('href') %>%
      stringr::str_match("'(.*)'") %>%
      as.character() %>%
      dplyr::last() %>%
      {paste0(u0, .)}
  }
  pastas <- sprintf('%s/tjms_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(1),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sapply(date_link, pega_link),
                  arq = sprintf('%s/tjms_dje_%s_%s.pdf', rep(pastas, each = 1), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

dje_tjrn <- function(dates, path, verbose) {

  pega_link <- function(d) {
    pega_jsf <- function(r) {
      r %>%
        httr::content('text') %>%
        xml2::read_html() %>%
        rvest::html_nodes(xpath = '//input[contains(@id, "_64")]') %>%
        rvest::html_attr('value')
    }
    u_inicial <- 'http://www.diario.tjrn.jus.br/djonline/inicial.jsf'
    r_inicial <- httr::GET(u_inicial, httr::config(ssl_verifypeer = FALSE))
    u <- 'https://www.diario.tjrn.jus.br/djonline/goto.jsf'
    jsf <- r_inicial %>% pega_jsf()
    dados_pesq <- list(
      'jsf_tree_64' = jsf[1],
      'jsf_state_64' = jsf[2],
      'jsf_viewid' = '/goto.jsp',
      'menu:formMenu_SUBMIT' = '1',
      'menu:formMenu:_link_hidden_' = 'menu:formMenu:_id16'
    )
    r_pesq <- httr::POST(u, body = dados_pesq,
                         httr::config(ssl_verifypeer = FALSE))
    jsf <- r_pesq %>% pega_jsf()
    date_link0 <- format(as.Date(d), '%d/%m/%Y')
    dados0 <- list(
      'jsf_tree_64' = jsf[1],
      'jsf_state_64' = jsf[2],
      'jsf_viewid' = '/goto.jsp',
      'pesquisarEdicaoCompletaBean:pesquisa_:dataInicio' = date_link0,
      'pesquisarEdicaoCompletaBean:pesquisa_:dataFim' = date_link0,
      'pesquisarEdicaoCompletaBean:pesquisa_:cmdPesquisar' = 'Pesquisar',
      'pesquisarEdicaoCompletaBean:pesquisa__SUBMIT' = '1',
      'edicao' = '',
      'pesquisarEdicaoCompletaBean:pesquisa_:_link_hidden_' = ''
    )
    r1 <- httr::POST(u, body = dados0,
                     httr::config(ssl_verifypeer = FALSE),
                     encode = 'form')
    itens <- r1 %>%
      httr::content('text') %>%
      xml2::read_html() %>%
      rvest::html_nodes(
        xpath = '//tbody[@id="pesquisarEdicaoCompletaBean:dados:tbody_element"]//a'
      ) %>% {
        if (length(.) == 2) {
          id <- rvest::html_attr(., 'id')
          nm <- rvest::html_attr(., 'onclick')
          nm <- stringr::str_match(nm, "\\.value='([0-9]{8}_[A-Z]{3})'")[,2]
          list(list(id[1], nm[1]), list(id[2], nm[2]))
        } else {
          FALSE
        }
      }
    if (is.logical(itens)) return(dplyr::data_frame(link = rep(u, 2), caderno = 1:2))
    jsf <- r1 %>% pega_jsf()
    dados <- lapply(itens, function(x) {
      list('jsf_tree_64' = jsf[1],
           'jsf_state_64' = jsf[2],
           'jsf_viewid' = '/goto.jsp',
           'linkDummyForm:_link_hidden_' = x[[1]],
           'pesquisarEdicaoCompletaBean:scroll_1' = '',
           'edicao' = x[[2]]
      )
    })
    links <- sapply(dados, function(x) {
      r <- httr::POST(u, body = x,
                      httr::config(ssl_verifypeer = FALSE),
                      encode = 'form')
      u_final <- 'https://www.diario.tjrn.jus.br/djonline/pages/edicao/edicaoVisualizadorTopo.jsf'
      r_final <- httr::GET(u_final, httr::config(ssl_verifypeer = FALSE))
      r_final %>%
        httr::content('text', encoding = 'ISO-8859-1') %>%
        xml2::read_html() %>%
        rvest::html_node('a') %>%
        rvest::html_attr('href') %>%
        {paste0('https://www.diario.tjrn.jus.br', gsub('\\', '/', ., fixed = T))}
    })
    dplyr::data_frame(link = links, caderno = 1:2)
  }
  pastas <- sprintf('%s/tjrn_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))


  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- dplyr::data_frame(date = dates) %>%
    dplyr::group_by(date) %>%
    dplyr::do(pega_link(.$date)) %>%
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

  tjsc_link <- function(date_link) {
    u <- 'http://busca.tjsc.jus.br/consultadje/visualizadiario.action'
    r0 <- httr::POST(u, body = list('dtselecionada' = date_link),
                     httr::config(followlocation = 0L),
                     encode = 'form')
    if (r0$status_code == 302) return(r0$headers$location)
    existe <- !(stringr::str_detect(httr::content(r0, 'text'), 'Não há|não hav'))
    if (existe) return(u)
    if (r0$status_code != 302) return(r0$status_code)
  }
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(c(1)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sapply(date_link, tjsc_link),
                  arq = sprintf('%s/tjsc_dje_%s_%s.pdf', rep(pastas, each = 1), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

dje_tjce <- function(dates, path, verbose) {
  u <- 'http://esaj.tjce.jus.br/cdje/downloadCaderno.do?'
  pastas <- sprintf('%s/tjce_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(dplyr::data_frame(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(c(1:2)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    dplyr::tbl_df() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sprintf('%sdtDiario=%s&cdCaderno=%s', u, date_link, caderno),
                  arq = sprintf('%s/tjce_dje_%s_%s.pdf', rep(pastas, each = 2), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}


dje_tjba <- function(dates, path, verbose) {
  edicoes_tjba <- function() {
    # essa foi dificil!
    u0 <- 'http://www2.tjba.jus.br/diario/internet/pesquisar.wsp'
    r0 <- httr::GET(u0, httr::config(followlocation = 0L))
    u <- httr::headers(r0)[['location']]
    r00 <- httr::GET(u)
    token <- r00 %>%
      httr::content('text', encoding = 'ISO-8859-1') %>%
      xml2::read_html() %>%
      rvest::html_node(xpath = '//input[@id="wi.token"]') %>%
      rvest::html_attr('value')
    b <- list('tmp_origem' = '',
              'tmp.diario.dt_inicio' = '25/01/2001',
              'tmp.diario.dt_fim' = '04/02/2100',
              'tmp.diario.cd_caderno' = '',
              'tmp.diario.cd_secao' = '',
              'tmp.diario.pal_chave' = '',
              'wi.token' = token,
              'tmp.diario.id_advogado' = '')
    r1 <- httr::POST(u, body = b, encode = 'form')
    d_edicoes <- r1 %>%
      httr::content('text', encoding = 'ISO-8859-1') %>%
      xml2::read_html() %>%
      rvest::html_node('table.grid') %>%
      rvest::html_table() %>%
      dplyr::tbl_df() %>%
      setNames(letters[1:length(.)]) %>%
      tidyr::gather() %>%
      dplyr::filter(stringr::str_trim(value) != '') %>%
      tidyr::separate(value, c('edicao', 'date'), sep = '\\(') %>%
      dplyr::mutate(date = lubridate::dmy(date, locale = "pt_BR.UTF-8"),
                    date = as.Date(date))
    d_edicoes
  }
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

download_arq <- function(u, a, verbose = FALSE) {
  if (file.exists(a)) {
    if (verbose) cat('\narquivo ',  a, ' ja existe!\n')
    return(dplyr::data_frame(result = 'exists'))
  }
  if (verbose) cat('\nbaixando ', a, '...', sep = '')
  res <- tryCatch({
    r <- suppressWarnings({
      httr::GET(u, httr::write_disk(a, overwrite = TRUE),
                httr::config(ssl_verifypeer = FALSE))
    })
    ct <- httr::headers(r)[['content-type']]
    ct <- ifelse(is.null(ct), 'application', ct)
  }, error = function(e) as.character(e))
  if (stringr::str_detect(res, 'Timeout')) {
    if (verbose) cat('ERRO!\n')
    return(dplyr::data_frame(result = 'timeout'))
  }
  if (httr::status_code(r) == 200 && stringr::str_detect(ct, 'application')) {
    if (verbose) cat('OK!\n')
    return(dplyr::data_frame(result = 'ok'))
  }
  if (verbose) cat('ERRO!\n')
  return(dplyr::data_frame(result = 'nao tem dje'))
}


