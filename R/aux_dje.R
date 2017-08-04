# Get data for downloading DJE depending on its TJ
get_dje_data <- function(tj) {
  switch (tj,
    "tjac" = list(
      u_dje = "http://diario.tjac.jus.br/edicoes.php?",
      booklets = c(1)),
    "tjal" = list(
      u_dje = "http://www2.tjal.jus.br/cdje/downloadCaderno.do?",
      booklets = c(2, 3)),
    "tjam" = list(
      u_dje = "http://esaj.tjam.jus.br/cdje/downloadCaderno.do?",
      booklets = c(1:3)),
    "tjba" = list(
      u_dje = "http://www.tjba.jus.br/diario/internet/download.wsp?",
      booklets = c(1)),
    "tjce" = list(
      u_dje = "http://esaj.tjce.jus.br/cdje/downloadCaderno.do?",
      booklets = c(1:2)),
    "tjms" = list(
      u_dje = "http://www.tjms.jus.br/cdje/downloadCaderno.do?",
      booklets = c(1)),
    "tjrn" = list(
      u_dje = "https://www.diario.tjrn.jus.br/djonline/pages/edicao/edicaoVisualizadorTopo.jsf",
      booklets = c(1:2)),
    "tjsc" = list(
      u_dje = "http://busca.tjsc.jus.br/dje-consulta/rest/diario/caderno?",
      booklets = c(1:4)),
    "tjsp" = list(
      u_dje = "http://www.dje.tjsp.jus.br/cdje/downloadCaderno.do?",
      booklets = c(11:15, 18)))
}

# Call appropriate function to get link depending on TJ
get_dje_link <- function(tj, date, u_dje, booklet) {
  switch(tj,
    "tjac" = get_tjac_link(date, u_dje),
    "tjba" = get_tjba_link(date, u_dje),
    "tjrn" = get_tjrn_link(date, u_dje, booklet),
    "tjsc" = get_tjsc_link(date, u_dje, booklet),
    get_default_link(date, u_dje, booklet)
  )
}

# Get link for many TJ"s DJEs
get_default_link <- function(date, u_dje, booklet) {
  date_link <- format(lubridate::as_date(date), "%d/%m/%Y")
  stringr::str_c(u_dje, "dtDiario=", date_link, "&cdCaderno=", booklet)
}

# Get link for TJAC"s DJE
get_tjac_link <- function(date, u_dje) {

  # Convert date to appropriate format
  date <- lubridate::as_date(date)

  # Get text in page
  text <- stringr::str_c(
    u_dje, "Ano=", lubridate::year(date),
    "&Mes=", lubridate::month(date)) %>%
    httr::GET() %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    rvest::html_nodes("table tr.texto_normal")

  # Get dates in page
  dates <- rvest::html_node(text, "a.texto_normal") %>%
    rvest::html_text() %>%
    stringr::str_trim() %>%
    lubridate::dmy(locale = "pt_BR.UTF-8")

  # Get links in page
  links <- rvest::html_node(text, xpath = './/a[@title="Baixar"]') %>%
    rvest::html_attr("href") %>%
    stringr::str_c("http://diario.tjac.jus.br", .)

  # Get only pertinent link
  link <- dplyr::data_frame(dates, links) %>%
    dplyr::filter(dates == date) %>%
    with(links)

  return(link)
}

# Get link for TJBA"s DJE
get_tjba_link <- function(date, u_dje) {

  # Get token for access
  token <- "http://www2.tjba.jus.br/diario/internet/pesquisar.wsp" %>%
    httr::GET(httr::config(followlocation = 0L)) %>%
    httr::content("text", encoding = "ISO-8859-1") %>%
    xml2::read_html() %>%
    rvest::html_node(xpath = '//input[@id="wi.token"]') %>%
    rvest::html_attr("value")

  # Build body of POST request
  body <- list(
    "tmp_origem" = "",
    "tmp.diario.dt_inicio" = "25/01/2001",
    "tmp.diario.dt_fim" = "04/02/2100",
    "tmp.diario.cd_caderno" = "",
    "tmp.diario.cd_secao" = "",
    "tmp.diario.pal_chave" = "",
    "wi.token" = token,
    "tmp.diario.id_advogado" = "")

  # Get
  editions <- "http://www2.tjba.jus.br/diario/internet/pesquisar.wsp" %>%
    httr::POST(body = body, encode = "form") %>%
    httr::content("text", encoding = "ISO-8859-1") %>%
    xml2::read_html() %>%
    rvest::html_node("table.grid") %>%
    rvest::html_table() %>%
    janitor::clean_names() %>%
    tidyr::gather() %>%
    dplyr::filter(stringr::str_trim(value) != "") %>%
    tidyr::separate(value, c("edition", "date"), sep = "\\(") %>%
    dplyr::mutate(date = lubridate::dmy(date, locale = "pt_BR.UTF-8"))

  # Return link with matching edition
  return(stringr::str_c(
    "tmp.diario.nu_edicao=", u_dje,
    editions$edition[match(date, editions$date)]))
}

# Get link for TJRN"s DJE
get_tjrn_link <- function(date, u_dje, booklet) {

  # Conver date to link format
  date <- format(lubridate::as_date(date), "%d/%m/%Y")

  # Get JSF from a search result
  get_jsf <- function(result) {
    httr::content(result, "text") %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//input[contains(@id, "_64")]') %>%
      rvest::html_attr("value")
  }

  # Get JSF from simple search
  jsf_1 <- httr::GET(
      "http://www.diario.tjrn.jus.br/djonline/inicial.jsf",
      httr::config(ssl_verifypeer = FALSE)) %>%
    get_jsf()

  # Body for second request
  body_2 <- list(
    "jsf_tree_64" = jsf_1[1],
    "jsf_state_64" = jsf_1[2],
    "jsf_viewid" = "/goto.jsp",
    "menu:formMenu_SUBMIT" = "1",
    "menu:formMenu:_link_hidden_" = "menu:formMenu:_id16"
  )

  # Get JSF from first POSTed search
  jsf_2 <- httr::POST(
      "https://www.diario.tjrn.jus.br/djonline/goto.jsf",
      body = body_2, httr::config(ssl_verifypeer = FALSE)) %>%
    get_jsf()

  # Body for third request
  body_3 <- list(
    "jsf_tree_64" = jsf_2[1],
    "jsf_state_64" = jsf_2[2],
    "jsf_viewid" = "/goto.jsp",
    "pesquisarEdicaoCompletaBean:pesquisa_:dataInicio" = date,
    "pesquisarEdicaoCompletaBean:pesquisa_:dataFim" = date,
    "pesquisarEdicaoCompletaBean:pesquisa_:cmdPesquisar" = "Pesquisar",
    "pesquisarEdicaoCompletaBean:pesquisa__SUBMIT" = "1",
    "edicao" = "",
    "pesquisarEdicaoCompletaBean:pesquisa_:_link_hidden_" = ""
  )

  # Get relevant nodes from third request
  nodes <- httr::POST(
      "https://www.diario.tjrn.jus.br/djonline/goto.jsf",
      body = body_3, httr::config(ssl_verifypeer = FALSE), encode = "form") %>%
    httr::content("text") %>%
    xml2::read_html() %>%
    rvest::html_nodes(
      xpath = '//tbody[@id="pesquisarEdicaoCompletaBean:dados:tbody_element"]//a')

  # Extract data items from nodes
  if (length(nodes) == 2) {
    id <- rvest::html_attr(nodes, "id")
    nm <- rvest::html_attr(nodes, "onclick")
    nm <- stringr::str_match(nm, "\\.value='([0-9]{8}_[A-Z]{3})'")[,2]
    items <- list(list(id[1], nm[1]), list(id[2], nm[2]))
  } else {
    return(NA)
  }

  # Get JSF from second POSTed search
  jsf_3 <- httr::POST(
      "https://www.diario.tjrn.jus.br/djonline/goto.jsf",
      body = body_3, httr::config(ssl_verifypeer = FALSE), encode = "form") %>%
    get_jsf()

  # Create request bodies from items
  items_body <- purrr::map(items, function(x) {
    list(
      "jsf_tree_64" = jsf_3[1],
      "jsf_state_64" = jsf_3[2],
      "jsf_viewid" = "/goto.jsp",
      "linkDummyForm:_link_hidden_" = x[[1]],
      "pesquisarEdicaoCompletaBean:scroll_1" = "",
      "edicao" = x[[2]])
  })

  # Get links using body_items
  links <- purrr::map_chr(items_body, function(x) {
    r <- httr::POST(
      "https://www.diario.tjrn.jus.br/djonline/goto.jsf",
      body = x, httr::config(ssl_verifypeer = FALSE), encode = "form")

    httr::GET(u_dje, httr::config(ssl_verifypeer = FALSE)) %>%
      httr::content("text", encoding = "ISO-8859-1") %>%
      xml2::read_html() %>%
      rvest::html_node("a") %>%
      rvest::html_attr("href") %>%
      stringr::str_replace_all("\\\\", "/") %>%
      stringr::str_c("https://www.diario.tjrn.jus.br", .)
  })

  # Only return link associated with requested booklet
  return(links[booklet])
}

# Get link for TJSC"s DJE
get_tjsc_link <- function(date, u_dje, booklet) {

  # Get edition number for date
  edition <- stringr::str_c(
      "http://busca.tjsc.jus.br/dje-consulta/rest/diario/dia?dia=",
      format(lubridate::as_date(date), "%d/%m/%Y")) %>%
    httr::GET() %>%
    xml2::read_html() %>%
    stringr::str_extract("\"edicao\":[0-9]+") %>%
    stringr::str_extract("[0-9]+")

  # Return link
  return(stringr::str_c(u_dje, "edicao=", edition, "&cdCaderno=", booklet))
}
