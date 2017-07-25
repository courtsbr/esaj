# @export
cpo_pg <- function(processos, path = "data-raw/cpo-pg", tj = 'TJSP', .parallel = F) {
  d <- dplyr::data_frame(n_processo = unique(processos))
  d <- dplyr::mutate(d, id = 1:n(), path = path, tj = tj)
  if(.parallel){
    clust <- multidplyr::create_cluster(parallel::detectCores())
    d <- multidplyr::partition(d, id, n_processo, cluster = clust)
    parallel::clusterExport(clust, list('cpo_pg_um',
                                        'tem_captcha',
                                        'quebra_captcha',
                                        'build_url_cpo_pg'))
    # parallel::clusterCall(clust, function() library(magrittr))
    d <- dplyr::do(d,{
        cpo_pg_um(.$n_processo, path = .$path, tj = .$tj)
      })
    d <- dplyr::collect(d)
  } else {
    d <- d %>%
      dplyr::select(-id) %>%
      dplyr::rename(p = n_processo) %>%
      plyr::mlply(cpo_pg_um, .progress = 'text')
  }
  d <- dplyr::ungroup(d)
  d
}

# @export
build_url_cpo_pg <- function(p, tj, captcha = NULL, tipo_processo = 'UNIFICADO', uid = NULL) {
  #  p <- gsub("[^0-9]", "", as.character(p))

  dados_url <- list(conversationId = "",
                    dadosConsulta.localPesquisa.cdLocal = "-1",
                    cbPesquisa = "NUMPROC",
                    dadosConsulta.tipoNuProcesso = tipo_processo,
                    numeroDigitoAnoUnificado = "",
                    foroNumeroUnificado = "",
                    dadosConsulta.valorConsultaNuUnificado = "",
                    dadosConsulta.valorConsulta = "")

  if(tipo_processo == 'UNIFICADO'){

    dados_url[["numeroDigitoAnoUnificado"]] <- stringr::str_sub(p, start = 1, end = 15)
    dados_url[["foroNumeroUnificado"]] <- stringr::str_sub(p, start = 22)
    dados_url[["dadosConsulta.valorConsultaNuUnificado"]] <- p

  } else {

    dados_url[["dadosConsulta.valorConsulta"]] <- p

  }
  if (tj == 'TJSP') {
    url1 <- "https://esaj.tjsp.jus.br/cpopg/search.do"
  } else if (tj == 'TJAL') {
    url1 <- 'http://www2.tjal.jus.br/cpopg/search.do'
  } else if (tj == 'TJSC') {
    url1 <- "https://esaj.tjsc.jus.br/cpopg/search.do"
    if(!is.null(captcha)){
      dados_url[['vlCaptcha']] = tolower(captcha)
      dados_url[['uuidCaptcha']] = uid
      dados_url[['novoVlCaptcha']] = ''
    }
    # No TJSC o cpopg_um não consegue baixar via link quando tem captcha,
    # precisa fazer a requisição via formulário, com os parâmetros
    # de dados_url.
    return(dados_url)
  }
  parametros <- paste(names(dados_url), unlist(dados_url), sep = "=")

  paste(url1, paste0(parametros, collapse = "&"), sep = "?")
}

# @export
cpo_pg_um <- function(p, path, tj){

  f <- function(p, path, tj) {
    p <- gsub("[^0-9]", "", p)
    arq <- sprintf("%s/%s.html", path, p)
    if (!is.null(path) & file.exists(arq)){
      return(dplyr::data_frame(result = "arquivo existe"))
    }
    if (tj == 'TJSC') {
      Sys.sleep(1)

      httr::handle_reset('http://esaj.tjsc.jus.br/cpopg/open.do')
      httr::GET('http://esaj.tjsc.jus.br/cpopg/open.do')
      tmp <- tempfile()
      # timestamp <- V8::v8()$eval("new Date().getTime();")
      s <- rvest::html_session('http://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do')

      aff <- s$response %>% httr::content() %>% captchaTJSC:::ler_new()
      # aff %>% desenhar()
      captcha <- aff %>% captchaTJSC:::limpar_new() %>% captchaTJSC:::ocr()
      # captcha
      uid <- aff$uuid[1]
      if(nchar(p) == 20){
        params <- build_url_cpo_pg(p,tj,captcha, 'UNIFICADO', uid)
      } else {
        params <- build_url_cpo_pg(p,tj,captcha, 'SAJ', uid)
      }
      httr::GET(url = 'http://esaj.tjsc.jus.br/cpopg/search.do',
                query = params,
                handle = s$handle,
                httr::write_disk(arq, overwrite = T)) -> r
    } else {
      u <- build_url_cpo_pg(p,tj)
      r <- httr::GET(u, httr::write_disk(arq, overwrite = T),
                     httr::config(ssl_verifypeer = FALSE))
    }
    i <- 0
    while (r$status_code != 200 & i < 10) {
      cat('bugou...')
      i <- i + 1
      if (!file.exists(arq)) {
        r <- httr::GET(u, httr::config(ssl_verifypeer = FALSE),
                       httr::write_disk(arq))
      }
    }
    return(dplyr::data_frame(result = "OK"))
  }

  tryCatch(f(p,path,tj),
           error = function(e) dplyr::data_frame(result = 'erro'))
}

# @export
tem_captcha <- function(r) {
  (r %>%
     httr::content('text') %>%
     xml2::read_html() %>%
     rvest::html_nodes('#captchaCodigo') %>%
     length()) > 0
}

# @export
quebra_captcha <- function(u_captcha){
  tmp <- tempfile()
  #  u_captcha <- 'http://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do'
  r_captcha <- httr::GET(u_captcha)
  # obs: a funcao "write_disk" não é apropriada pois salva o arq duas vezes.
  writeBin(httr::content(r_captcha, "raw"), tmp)
  captcha <- tryCatch(captchasaj::decodificar(tmp, captchasaj::modelo$modelo),
                      error = function(e) 'xxxxx')
}

# set_values2 <-  function (form, l)
#   {
#     new_values <- l
#     no_match <- setdiff(names(new_values), names(form$fields))
#     if (length(no_match) > 0) {
#       stop("Unknown field names: ", paste(no_match, collapse = ", "),
#            call. = FALSE)
#     }
#     for (field in names(new_values)) {
#       type <- form$fields[[field]]$type %||% "non-input"
#       if (type == "hidden") {
#         warning("Setting value of hidden field '", field,
#                 "'.", call. = FALSE)
#       }
#       else if (type == "submit") {
#         stop("Can't change value of submit input '", field,
#              "'.", call. = FALSE)
#       }
#       form$fields[[field]]$value <- new_values[[field]]
#     }
#     form
# }
