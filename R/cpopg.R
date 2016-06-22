#' @export
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
    parallel::clusterCall(clust, function() library(magrittr))
    d <- dplyr::do(d,{
        cpo_pg_um(.$n_processo, path = .$path, tj = .$tj)
      })
    d <- dplyr::collect(d)
  } else {
    d <- d %>%
      dplyr::group_by(n_processo) %>%
      dplyr::do({
        cpo_pg_um(.$n_processo, path = .$path, tj = .$tj)
        })
  }
  d <- dplyr::ungroup(d)
  d
}

#' @export
build_url_cpo_pg <- function(p, tj, captcha = NULL) {
  p <- gsub("[^0-9]", "", as.character(p))
  dados_url <- list(conversationId = "",
                    dadosConsulta.localPesquisa.cdLocal = "-1",
                    cbPesquisa = "NUMPROC",
                    dadosConsulta.tipoNuProcesso = "UNIFICADO",
                    numeroDigitoAnoUnificado = "",
                    foroNumeroUnificado = "",
                    dadosConsulta.valorConsultaNuUnificado = "",
                    dadosConsulta.valorConsulta = "")
  dados_url[["numeroDigitoAnoUnificado"]] <- stringr::str_sub(p, start = 1, end = 13)
  dados_url[["foroNumeroUnificado"]] <- stringr::str_sub(p, start = 17)
  dados_url[["dadosConsulta.valorConsultaNuUnificado"]] <- p
  if (tj == 'TJSP') {
   url1 <- "https://esaj.tjsp.jus.br/cpopg/search.do"
 } else if (tj == 'TJAL') {
   url1 <- 'http://www2.tjal.jus.br/cpopg/search.do'
 } else if (tj == 'TJSC') {
   url1 <- "https://esaj.tjsc.jus.br/cpopg/search.do"
   if(!is.null(captcha)){dados_url[['vlCaptcha']] = tolower(captcha)}

   # No TJSC o cpopg_um não consegue baixar via link quando tem captcha,
   # precisa fazer a requisição via formulário, com os parâmetros
   # de dados_url.
   return(dados_url)
 }
  parametros <- paste(names(dados_url), unlist(dados_url), sep = "=")

  paste(url1, paste0(parametros, collapse = "&"), sep = "?")
}

#' @export
cpo_pg_um <- function(p, path, tj){

  f <- function(p, path, tj) {

    p <- gsub("[^0-9]", "", p)
    arq <- sprintf("%s/%s.html", path, p)

    if (!is.null(path) & file.exists(arq)){
      return(dplyr::data_frame(result = "arquivo existe"))
    }

    # Sys.sleep(1)
    if(tj == 'TJSC'){
      tmp <- tempfile()

      cpopg <- 'http://esaj.tjsc.jus.br/cpopg/'

      captcha <- NULL
      link_im <- paste0(cpopg,'imagemCaptcha.do')
      link_som <- paste0(cpopg,'somCaptcha.do')
      link_form <- paste0(cpopg,'open.do')

      s <- rvest::html_session(link_form)

      if(tem_captcha(s$response)){

        s <- rvest::html_session(link_im) %>%
          rvest::jump_to(link_som)

        s %>%
          `$`('response') %>%
          httr::content('raw')  %>%
          writeBin(tmp)

      captcha <- captchaTJSC::decifrar(tmp)

      s %<>%
        rvest::jump_to(link_form)
      }

      params <- build_url_cpo_pg(p,tj,captcha)

      form <- s %>%
        rvest::html_form() %>%
        dplyr::first() %>%
        set_values2(params)

      s %>%
        rvest::submit_form(form) %>%
        `$`('response') -> r

      cat(httr::content(r, 'text'), file = arq)
    } else {
      u <- build_url_cpo_pg(p,tj)
      r <- httr::GET(u, httr::write_disk(arq, overwrite = T), httr::config(ssl_verifypeer = FALSE))
    }

#    k <- TRUE
    while (r$status_code != 200) {
      # if (k)
      #   cat("\nesperando...")
      # else cat("...")
      if (!file.exists(arq)) {
        r <- httr::GET(u,
                       httr::config(ssl_verifypeer = FALSE),
                       httr::write_disk(arq))
      }
#      k <- FALSE
    }
    # if (!k) cat("\n")
    return(dplyr::data_frame(result = "OK"))
  }

  tryCatch(f(p,path,tj),
           error = function(e) dplyr::data_frame(result = 'erro'))
}

#' @export
tem_captcha <- function(r) {
  (r %>%
     httr::content('text') %>%
     xml2::read_html() %>%
     rvest::html_nodes('#captchaCodigo') %>%
     length()) > 0
}

#' @export
quebra_captcha <- function(u_captcha){
  tmp <- tempfile()
  #  u_captcha <- 'http://esaj.tjsc.jus.br/cpopg/imagemCaptcha.do'
  r_captcha <- httr::GET(u_captcha)
  # obs: a funcao "write_disk" não é apropriada pois salva o arq duas vezes.
  writeBin(httr::content(r_captcha, "raw"), tmp)
  captcha <- tryCatch(captchasaj::decodificar(tmp, captchasaj::modelo$modelo),
                      error = function(e) 'xxxxx')
}

set_values2 <-  function (form, l)
  {
    new_values <- l
    no_match <- setdiff(names(new_values), names(form$fields))
    if (length(no_match) > 0) {
      stop("Unknown field names: ", paste(no_match, collapse = ", "),
           call. = FALSE)
    }
    for (field in names(new_values)) {
      type <- form$fields[[field]]$type %||% "non-input"
      if (type == "hidden") {
        warning("Setting value of hidden field '", field,
                "'.", call. = FALSE)
      }
      else if (type == "submit") {
        stop("Can't change value of submit input '", field,
             "'.", call. = FALSE)
      }
      form$fields[[field]]$value <- new_values[[field]]
    }
    form
}

`%||%` <- rvest:::`%||%`
`%<>%` <- magrittr:::`%<>%`
