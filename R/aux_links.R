edicoes_tjba <- function() {
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
  d_edicoes
}

tjsc_link <- function(date_link) {
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

link_tjrn <- function(d) {
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

link_tjms <- function(d) {
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

link_tjac <- function(d) {
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
