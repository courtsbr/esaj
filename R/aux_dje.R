
get_dje_link <- function(tj, date, ...) {
  switch(tj,
    "tjac" = tjac_link(date),
    "tjba" = tjba_link(date),
    "tjrn" = tjrn_link(date),
    "tjsc" = tjsc_link(date),
    default_link(date, ...)
  )
}

default_link <- function(date, ...) {
  u_dje <- ..1
  date_link <- format(lubridate::as_date(date), "%d/%m/%Y")
  booklet <- ..2
  stringr::str_c(u_dje, "dtDiario=", date_link, "&cdCaderno=", booklet)
}

tjba_link <- function(date) {
  # essa foi dificil!
  u0 <- 'http://www2.tjba.jus.br/diario/internet/pesquisar.wsp'
  r0 <- httr::GET(u0, httr::config(followlocation = 0L))
  #u <- httr::headers(r0)[['location']]
  #r00 <- httr::GET(u)
  #F: comentei essa requisição porque não tem "location" no cabeçalho desse GET.
  token <- r0 %>%
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
  r1 <- httr::POST(u0, body = b, encode = 'form')
  d_edicoes <- r1 %>%
    httr::content('text', encoding = 'ISO-8859-1') %>%
    xml2::read_html() %>%
    rvest::html_node('table.grid') %>%
    rvest::html_table() %>%
    janitor::clean_names() %>%
    #os nomes podem vir repetidos
    tidyr::gather() %>%
    dplyr::filter(stringr::str_trim(value) != '') %>%
    tidyr::separate(value, c('edicao', 'date'), sep = '\\(') %>%
    dplyr::mutate(date = lubridate::dmy(date, locale = "pt_BR.UTF-8"),
                  date = as.Date(date))

  u_dje <- "http://www.tjba.jus.br/diario/internet/download.wsp?tmp.diario.nu_edicao="
  edicao <- d_edicoes$edicao[match(as.Date(date), d_edicoes$date)]

  stringr::str_c(u_dje, edicao)
}

tjsc_link <- function(date_link) {

  date_link <- format(lubridate::as_date(date_link), '%d/%m/%Y')

  u <- 'http://busca.tjsc.jus.br/consultadje/visualizadiario.action'
  r0 <- httr::POST(u, body = list('dtselecionada' = date_link),
                   httr::config(followlocation = 0L),
                   encode = 'form')
  if (r0$status_code == 302) {
    loc <- r0$headers$location
    # print(loc)
    u_base <- sprintf('http://www.tjsc.jus.br/institucional/diario/a%d/',
                      lubridate::year(lubridate::dmy(date_link)))
    u_final <- paste0(u_base, stringr::str_match(loc, '/([^/]+)$')[, 2])
    return(u_final)
  }
  existe <- !(stringr::str_detect(httr::content(r0, 'text'), 'N\u00e3o h\u00e1|n\u00e3o hav'))
  if (existe) return(u)
  if (r0$status_code != 302) return(r0$status_code)
}

tjrn_link <- function(d) {
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

tjac_link <- function(d) {
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
        purrr::map_chr(conv_months) %>%
        lubridate::dmy() %>%
        lubridate::as_date()
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

conv_months <- function(str) {
  str <- stringr::str_replace_all(str, " de ", "-")
  month <- stringr::str_extract(str, "[a-z]+")

  month <- switch (month,
    "janeiro" = "1",
    "fevereiro" = "2",
    "mar\u00e7o" = "3",
    "abril" = "4",
    "maio" = "5",
    "junho" = "6",
    "julho" = "7",
    "agosto" = "8",
    "setembro" = "9",
    "outubro" = "10",
    "novembro" = "11",
    "dezembro" = "12")

  stringr::str_replace(str, "[a-z]+", month)
}

# Download DJE file
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
    if (httr::status_code(r) == 200 && stringr::str_detect(ct, 'application')) {
      if (verbose) cat('OK!\n')
      return(dplyr::data_frame(result = 'ok'))
    }
  }, error = function(e) as.character(e))
  if (stringr::str_detect(res, 'Timeout')) {
    if (verbose) cat('ERRO!\n')
    return(dplyr::data_frame(result = 'timeout'))
  }
  if (verbose) cat('ERRO!\n')
  return(dplyr::data_frame(result = 'invalid dje'))
}
