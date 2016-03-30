#' @export
pesquisar_processos <- function(processos, path) {
  pesquisar_processo <- function(x) {
    arq <- sprintf('%s/%s.html', path, gsub('[^0-9]', '', x))
    if (file.exists(arq)) return(dplyr::data_frame())
    # p <- '0001956-29.2010.8.24.0011'
    Sys.sleep(1)
    u0 <- 'http://esaj.tjsc.jus.br/cpopg/open.do'
    r0 <- httr::GET(u0, httr::set_cookies(NULL))
    if (tem_captcha(r0)) {
      tmp <- tempfile()
      u_captcha <- 'http://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do'
      r_captcha <- httr::GET(u_captcha)
      # obs: a funcao "write_disk" não é apropriada pois salva o arq duas vezes.
      writeBin(httr::content(r_captcha, "raw"), tmp)
      captcha <- tryCatch(captchasaj::decodificar(tmp, captchasaj::modelo$modelo),
                          error = function(e) 'xxxxx')
      parm <- build_url_cpopg_p_captcha(x, captcha)
    } else {
      parm <- build_url_cpopg_p(x)
    }
    u <- "http://esaj.tjsc.jus.br/cpopg/search.do"
    arq <- sprintf('%s/%s.html', path, gsub('[^0-9]', '', x))
    r <- httr::GET(u, query = parm, httr::write_disk(arq, overwrite = TRUE))
    while (tem_captcha(r)) {
      message('errei captcha')
      tmp <- tempfile()
      u_captcha <- 'http://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do'
      r_captcha <- httr::GET(u_captcha)
      # obs: a funcao "write_disk" não é apropriada pois salva o arq duas vezes.
      writeBin(httr::content(r_captcha, "raw"), tmp)
      captcha <- tryCatch(captchasaj::decodificar(tmp, captchasaj::modelo$modelo),
                          error = function(e) 'xxxxx')
      parm <- build_url_cpopg_p_captcha(x, captcha)
      r <- httr::GET(u, query = parm, httr::write_disk(arq, overwrite = TRUE))
    }
    r %>%
      httr::content('text') %>%
      xml2::read_html() %>%
      plyr::ldply(raspar_processo)
  }
  f <- dplyr::failwith(dplyr::data_frame(n_processo = NA), pesquisar_processo)
  dplyr::data_frame(n_processo = processos) %>%
    dplyr::distinct(n_processo) %>%
    dplyr::group_by(n_processo) %>%
    dplyr::do({f(.$n_processo)}) %>%
    dplyr::ungroup()
}

build_url_cpopg_p <- function(p) {
  list(conversationId = '',
       dadosConsulta.localPesquisa.cdLocal = '-1',
       cbPesquisa = 'NUMPROC',
       dadosConsulta.tipoNuProcesso = 'UNIFICADO',
       numeroDigitoAnoUnificado = substr(p, 1, 15),
       foroNumeroUnificado = substr(p, 22, 25),
       dadosConsulta.valorConsultaNuUnificado = p,
       dadosConsulta.valorConsulta = '',
       pbEnviar = 'Pesquisar')
}

build_url_cpopg_p_captcha <- function(p, captcha) {
  list(conversationId = '',
       dadosConsulta.localPesquisa.cdLocal = '-1',
       cbPesquisa = 'NUMPROC',
       dadosConsulta.tipoNuProcesso = 'UNIFICADO',
       numeroDigitoAnoUnificado = substr(p, 1, 15),
       foroNumeroUnificado = substr(p, 22, 25),
       dadosConsulta.valorConsultaNuUnificado = p,
       dadosConsulta.valorConsulta = '',
       vlCaptcha = tolower(captcha))
}

raspar_processo <- function(h) {
  dplyr::data_frame()
}

raspar_node <- function(node) {
  a <- node %>% rvest::html_node('.linkProcesso')
  n_processo <- a %>% rvest::html_text() %>% stringr::str_trim()
  cod_processo <- node %>% rvest::html_attr('id') %>%
    stringr::str_replace('divProcesso', '')
  link_processo <- a %>% rvest::html_attr('href')
  # classe_assunto <- ''
  # forma_participacao <- node %>%
  #   rvest::html_nodes('.espacamentoLinhas') %>%
  #   dplyr::first() %>%
  #   rvest::html_node('span') %>%
  #   rvest::html_text() %>%
  #   stringr::str_trim()
  # nome <- node %>%
  #   rvest::html_nodes('.espacamentoLinhas') %>%
  #   dplyr::first() %>%
  #   rvest::html_text() %>%
  #   stringr::str_trim() %>%
  #   stringr::str_split_fixed(':', 2) %>%
  #   as.character() %>%
  #   dplyr::last() %>%
  #   stringr::str_trim()
  txt <- node %>%
    rvest::html_text() %>%
    stringr::str_replace_all('\t', '') %>%
    stringr::str_replace_all(' +', ' ') %>%
    stringr::str_replace_all('\n+', '\n') %>%
    stringr::str_replace_all('(\n +)', '\n') %>%
    stringr::str_replace_all('\n+', '\n') %>%
    stringr::str_replace_all('^\n|\n$', '')
  dplyr::data_frame(
    n_processo = n_processo,
    cod_processo = cod_processo,
    link_processo = link_processo,
    txt = txt
  )
}

raspar_arqs <- function(arqs) {
  raspar_arq <- function(arq) {
    arq %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = "//*[starts-with(@id, 'divProcesso')]") %>%
      plyr::ldply(raspar_node)
  }
  f <- dplyr::failwith(dplyr::data_frame(n_processo = NA), raspar_arq)
  dplyr::data_frame(arq = arqs) %>%
    dplyr::distinct(arq) %>%
    dplyr::group_by(arq) %>%
    dplyr::do({f(.$arq)}) %>%
    dplyr::ungroup()
}

raspar_arq_unico <- function(arq) {
  # arq <- 'data-raw/nomes/ABILIO_COMERCIO_DE_ALUMINIO_E_FERRO_LTDA/ABILIO_COMERCIO_DE_ALUMINIO_E_FERRO_LTDA.html'
  x <- readr::read_file(arq)
  mascara_cnj <- '[0-9]{7}-[0-9]{2}\\.[0-9]{4}\\.[0-9]{1}\\.[0-9]{2}\\.[0-9]{4}'
  x %>%
    xml2::read_html() %>%
    rvest::html_nodes('.secaoFormBody') %>%
    dplyr::last() %>%
    rvest::html_text() %>%
    stringr::str_extract(mascara_cnj)
}


build_url_cpopg_nome <- function(nm) {
  u <- paste0('http://esaj.tjsc.jus.br/cpopg/search.do?conversationId=',
              '&dadosConsulta.localPesquisa.cdLocal=-1&cbPesquisa=NMPARTE&dados',
              'Consulta.tipoNuProcesso=UNIFICADO&dadosConsulta.valorConsulta=%s',
              '&pbEnviar=Pesquisar')
  sprintf(u, gsub(' ', '+', nm))
}

build_url_cpopg_nome_captcha <- function(nm, captcha) {
  # parametros <- list(conversationId = '',
  #                    dadosConsulta.localPesquisa.cdLocal = '-1',
  #                    cbPesquisa = 'NUMPROC',
  #                    dadosConsulta.tipoNuProcesso = 'UNIFICADO',
  #                    numeroDigitoAnoUnificado = substr(p, 1, 15),
  #                    foroNumeroUnificado = substr(p, 22, 25),
  #                    dadosConsulta.valorConsultaNuUnificado = p,
  #                    dadosConsulta.valorConsulta = '',
  #                    vlCaptcha = tolower(txt))

  u <- paste0('http://esaj.tjsc.jus.br/cpopg/search.do?conversationId=',
              '&dadosConsulta.localPesquisa.cdLocal=-1&cbPesquisa=NMPARTE&dados',
              'Consulta.tipoNuProcesso=UNIFICADO&dadosConsulta.valorConsulta=%s',
              '&vlCaptcha=%s')
  sprintf(u, gsub(' ', '+', nm), tolower(captcha))
}

build_url_cpopg_nome_pag <- function(nm, pag) {
  u <- paste0('http://esaj.tjsc.jus.br/cpopg/trocarPagina.do?paginaConsulta=%d',
              '&conversationId=&dadosConsulta.localPesquisa.cdLocal=-1&',
              'cbPesquisa=NMPARTE&dadosConsulta.tipoNuProcesso=UNIFICADO',
              '&dadosConsulta.valorConsulta=%s&pbEnviar=Pesquisar')
  sprintf(u, pag, gsub(' ', '+', nm))
}

build_url_cpopg_nome_pag_captcha <- function(nm, pag, captcha) {
  u <- paste0('http://esaj.tjsc.jus.br/cpopg/trocarPagina.do?paginaConsulta=%d',
              '&conversationId=&dadosConsulta.localPesquisa.cdLocal=-1&',
              'cbPesquisa=NMPARTE&dadosConsulta.tipoNuProcesso=UNIFICADO',
              '&dadosConsulta.valorConsulta=%s&vlCaptcha=%s')
  sprintf(u, pag, gsub(' ', '+', nm), tolower(captcha))
}

tem_captcha <- function(r) {
  (r %>%
     httr::content('text') %>%
     xml2::read_html() %>%
     rvest::html_nodes('#captchaCodigo') %>%
     length()) > 0
}

diagnostico <- function(arqs) {
  diag_um <- function(a) {
    x <- readr::read_file(a)
    if (stringr::str_detect(x, 'Dados do processo'))
      return(dplyr::data_frame(result = '1 processo',
                               n_processos = 1))
    if ((xml2::read_html(x) %>%
         rvest::html_nodes(xpath = '//a[@title="Próxima página"]') %>%
         length()) > 0) {
      num_docs <- x %>%
        stringr::str_match('</strong>[ \n\t]+de [0-9]+[ \n\t]+</td>') %>%
        stringr::str_replace_all('[^0-9]', '') %>%
        as.numeric()
      return(dplyr::data_frame(result = 'mais paginas',
                               n_processos = num_docs))
    }
    if ((xml2::read_html(x) %>%
         rvest::html_nodes('#listagemDeProcessos') %>%
         length()) > 0) {
      num_docs <- x %>%
        stringr::str_match('</strong>[ \n\t]+de [0-9]+[ \n\t]+</td>') %>%
        stringr::str_replace_all('[^0-9]', '') %>%
        as.numeric()
      return(dplyr::data_frame(result = '1 pagina',
                               n_processos = num_docs))
    }
    if (stringr::str_detect(x, 'Digite o código aqui'))
      return(dplyr::data_frame(result = 'captcha errado',
                               n_processos = NA_real_))
    if (stringr::str_detect(x, 'Não existem informações'))
      return(dplyr::data_frame(result = 'Não achou',
                               n_processos = NA_real_))
    if (stringr::str_detect(x, 'muitos processos para os'))
      return(dplyr::data_frame(result = 'Processos demais',
                               n_processos = NA_real_))
    return(dplyr::data_frame(result = '?'))
  }
  f <- dplyr::failwith(dplyr::data_frame(result = NA), diag_um)
  dplyr::data_frame(arq = arqs) %>%
    dplyr::distinct(arq) %>%
    dplyr::group_by(arq) %>%
    dplyr::do({f(.$arq)}) %>%
    dplyr::ungroup()
}

pesquisar_nomes <- function(nm, path) {
  pesquisar_nome <- function(x) {
    httr::handle_find('http://esaj.tjsc.jus.br')
    httr::handle_reset('http://esaj.tjsc.jus.br')
    arq <- sprintf('%s/%s/%s.html', path, gsub(' ', '_', x), gsub(' ', '_', x))
    if (file.exists(arq)) return(dplyr::data_frame(n_processo = 'ja foi'))
    Sys.sleep(1)
    # nm <- 'EDSON LUIZ BARBOZA DE DEOS'
    u0 <- 'http://esaj.tjsc.jus.br/cpopg/open.do'
    r0 <- httr::GET(u0, httr::set_cookies(NULL))
    if (tem_captcha(r0)) {
      tmp <- tempfile()
      u_captcha <- 'http://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do'
      r_captcha <- httr::GET(u_captcha)
      # obs: a funcao "write_disk" não é apropriada pois salva o arq duas vezes.
      writeBin(httr::content(r_captcha, "raw"), tmp)
      captcha <- tryCatch(captchasaj::decodificar(tmp, captchasaj::modelo$modelo),
                          error = function(e) 'xxxxx')
      u <- build_url_cpopg_nome_captcha(x, captcha)
    } else {
      u <- build_url_cpopg_nome(x)
    }
    r <- httr::GET(u, httr::write_disk(arq))
    while (tem_captcha(r)) {
      message('errei captcha')
      tmp <- tempfile()
      u_captcha <- 'http://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do'
      r_captcha <- httr::GET(u_captcha)
      # obs: a funcao "write_disk" não é apropriada pois salva o arq duas vezes.
      writeBin(httr::content(r_captcha, "raw"), tmp)
      captcha <- tryCatch(captchasaj::decodificar(tmp, captchasaj::modelo$modelo),
                          error = function(e) 'xxxxx')
      u <- build_url_cpopg_nome_captcha(x, captcha)
      r <- httr::GET(u, httr::write_disk(arq, overwrite = TRUE))
    }
    Sys.sleep(1)
    num_docs <- r %>%
      httr::content('text') %>%
      stringr::str_extract('</strong>[ \n\t]+de [0-9]+[ \n\t]+</td>') %>%
      stringr::str_replace_all('[^0-9]', '') %>%
      as.numeric()
    cat(sprintf('\n%s %s\n', x, as.character(num_docs)))
    if (!is.na(num_docs) && num_docs > 25) {
      # significa que temos muitas paginas
      n_pags <- (num_docs %/% 25) + ((num_docs %% 25) > 0)
      for (i in seq_len(n_pags)[-1]) {
        arq <- sprintf('%s/%s/%s_pag%02d.html',
                       path, gsub(' ', '_', x), gsub(' ', '_', x), i)
        u <- build_url_cpopg_nome_pag(x, i)
        r <- httr::GET(u, httr::write_disk(arq))
        while (tem_captcha(r)) {
          message('errei captcha na pagina!')
          tmp <- tempfile()
          u_captcha <- 'http://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do'
          r_captcha <- httr::GET(u_captcha)
          # obs: a funcao "write_disk" não é apropriada pois salva o arq duas vezes.
          writeBin(httr::content(r_captcha, "raw"), tmp)
          captcha <- tryCatch(captchasaj::decodificar(tmp, captchasaj::modelo$modelo),
                              error = function(e) 'xxxxx')
          u <- build_url_cpopg_nome_pag_captcha(x, captcha)
          r <- httr::GET(u, httr::write_disk(arq, overwrite = TRUE))
        }
      }
    }
    dplyr::data_frame(n_processo = 'OK')
  }
  f <- dplyr::failwith(dplyr::data_frame(n_processo = NA), pesquisar_nome)
  dplyr::data_frame(nome = nm) %>%
    dplyr::distinct(nome) %>%
    dplyr::group_by(nome) %>%
    dplyr::do({f(.$nome)}) %>%
    dplyr::ungroup()
}

n_nomes <- function(nm, path) {
  n_nome <- function(x) {
    # httr::handle_reset('http://esaj.tjsc.jus.br')
    arq <- sprintf('%s/%s.html', path, gsub(' ', '_', x), gsub(' ', '_', x))
    if (file.exists(arq)) return(dplyr::data_frame(n_processo = 'ja foi'))
    Sys.sleep(1)
    # nm <- 'EDSON LUIZ BARBOZA DE DEOS'
    u0 <- 'http://esaj.tjsc.jus.br/cpopg/open.do'
    r0 <- httr::GET(u0, httr::set_cookies(NULL))
    if (tem_captcha(r0)) {
      tmp <- tempfile()
      u_captcha <- 'http://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do'
      r_captcha <- httr::GET(u_captcha)
      # obs: a funcao "write_disk" não é apropriada pois faz request duas vezes
      writeBin(httr::content(r_captcha, "raw"), tmp)
      captcha <- captchasaj::decodificar(tmp, captchasaj::modelo$modelo)
      u <- build_url_cpopg_nome_captcha(x, captcha)
    } else {
      u <- build_url_cpopg_nome(x)
    }
    r <- httr::GET(u, httr::write_disk(arq))
    return(dplyr::data_frame(x = ''))
  }
  f <- dplyr::failwith(dplyr::data_frame(x = ''), n_nome)
  dplyr::data_frame(nome = nm) %>%
    dplyr::distinct(nome) %>%
    dplyr::group_by(nome) %>%
    dplyr::do({f(.$nome)}) %>%
    dplyr::ungroup()
}



