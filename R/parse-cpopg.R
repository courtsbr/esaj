#' @export
parse_cpopg <- function(arqs, .parallel = TRUE) {
  fun <- function(i) {
    if(runif(1) < 0.01) cat(i, "de", n, "\n")
    x <- arqs[i]
    h <- xml2::read_html(x)
    if(length(rvest::html_nodes(h, '#spwTabelaMensagem')) > 0) {
      # nao existe
      infos <- partes <- movs <- list(dplyr::data_frame(erro = 'nao existe'))
      return(dplyr::data_frame(arq = x, infos, partes, movs))
    }
    if(length(rvest::html_nodes(h, '#tableTodasPartes')) > 0) {
      # segredo de justiça
      infos <- partes <- movs <- list(dplyr::data_frame(erro = 'segredo'))
      return(dplyr::data_frame(arq = x, infos, partes, movs))
    }
    fail <- list(dplyr::data_frame(erro = 'erro'))
    infos <- tryCatch(list(esaj::parse_cpopg_infos_(h)), error = function(e) fail)
    partes <- tryCatch(list(esaj::parse_cpopg_partes_(h)), error = function(e) fail)
    movs <- tryCatch(list(esaj::parse_cpopg_movs_(h)), error = function(e) fail)
    d <- dplyr::data_frame(arq = x, infos, partes, movs)
    saveRDS(d, gsub('.html$', '.rds', x))
    d
  }
  # fail <- list(dplyr::data_frame(erro = 'nao existe'))
  # d_fail <- dplyr::data_frame(arq = NA, infos = fail, partes = fail, movs = fail)
  f <- dplyr::failwith(d_fail, fun)
  if(.parallel) {
    cl <- parallel::makeCluster(parallel::detectCores(), outfile = "")
    doParallel::registerDoParallel(cl)
    n <- length(arqs)
    opts <- list(.packages = c('esaj', 'magrittr'), .export = c('arqs', 'n'))
    d <- n %>%
      seq_len() %>%
      plyr::llply(fun, .parallel = TRUE, .paropts = opts) %>%
      dplyr::bind_rows() %>%
      dplyr::tbl_df()
    parallel::stopCluster(cl)
  } else {
    n <- length(arqs)
    d <- n %>%
      seq_len() %>%
      plyr::llply(fun) %>%
      dplyr::bind_rows() %>%
      dplyr::tbl_df()
  }
  d
}

#' @export
parse_cpopg_infos_ <- function(html) {
  arrumar_key <- function(x) desacentuar(stringr::str_replace_all(tolower(x), " +", "_"))
  infos <- html %>%
    rvest::html_nodes(".secaoFormBody") %>%
    dplyr::last() %>%
    rvest::html_nodes("tr") %>%
    rvest::html_text() %>%
    stringr::str_replace_all("[\n\r\t]+", " ") %>%
    stringr::str_replace_all(" +", " ") %>%
    stringr::str_trim() %>%
    unique() %>%
    {dplyr::data_frame(info = .)} %>%
    tidyr::separate(info, c("key", "value"), sep = "\\:", extra = "merge", fill = "left") %>%
    dplyr::mutate(key = stringr::str_trim(key), value = stringr::str_trim(value)) %>%
    dplyr::distinct(value) %>%
    dplyr::mutate(key = stringr::str_replace_na(key, "Lugar")) %>%
    dplyr::mutate(key = arrumar_key(key))
  infos_cdp <- html %>%
    rvest::html_text() %>%
    stringr::str_match("processoPK\\.cdProcesso=([^&]+)&") %>%
    as.character() %>%
    dplyr::last() %>%
    {dplyr::data_frame(key = "cdprocesso", value = .)}
  infos_p <- infos %>%
    dplyr::filter(key == "processo") %>%
    tidyr::separate(value, c("n_processo", "status"), sep = " ",
                    extra = "merge", fill = "right") %>%
    dplyr::select(-key) %>%
    tidyr::gather(convert = TRUE)
  infos_digital <- html %>%
    rvest::html_nodes(".linkPasta") %>% {
      if (length(.) == 0)
        ""
      else rvest::html_text(dplyr::first(.))
    } %>% {
      digital <- stringr::str_detect(., "Este processo é digital")
      dplyr::data_frame(key = "digital", value = as.character(digital))
    }
  dplyr::bind_rows(infos, infos_p, infos_cdp, infos_digital) %>%
    dplyr::tbl_df()
}

#' @export
parse_cpopg_partes_ <- function(html) {
  arrumar_forma <- function(x) {
    x <- desacentuar(stringr::str_replace_all(tolower(x), " +", "_"))
    x <- gsub("[^a-z]", "", x)
    x
  }
  html %>%
    rvest::html_nodes("#tableTodasPartes") %>% {
      if (length(.) == 0)
        rvest::html_nodes(html, "#tablePartesPrincipais")
      else .
    } %>%
    dplyr::first() %>% {
      if (gsub("[\n\r\t]", "", rvest::html_text(.)) == "") {
        dplyr::data_frame()
      }
      else {
        rvest::html_table(.) %>%
          tidyr::separate(X2, c("parte", "adv"), sep = "\r\n\t",
                          extra = "merge", fill = "right") %>%
          dplyr::mutate(adv = stringr::str_trim(adv)) %>%
          dplyr::rename(forma = X1) %>%
          dplyr::mutate(forma = arrumar_forma(forma)) %>%
          dplyr::mutate(adv = gsub(" *\r[ \r\t\n]+ *", "\n", adv),
                        adv = gsub("\\&nbsp", " ", adv)) %>%
          dplyr::tbl_df()
      }
    }
}

#' @export
parse_cpopg_movs_ <- function(html) {
  html %>%
    rvest::html_node("#tabelaTodasMovimentacoes") %>%
    rvest::html_table() %>%
    dplyr::select(data_mov = X1, X3) %>%
    tidyr::separate(X3, c("titulo", "mov"), sep = "\r\n\t", extra = "merge", fill = "right") %>%
    dplyr::tbl_df()
}

#' @import xml2
#' @import rvest
#' @import plyr
#' @import dplyr
#' @import tidyr
#' @import stringi
#' @export
parse_cposg_outro <- function(arq, print_arq = F, tipos_de_info =  c('Apensos','1ª Instância','Partes do Processo','Subprocessos e Recursos','Composição do Julgamento','Petições diversas','Julgamentos','Exibindo ')){
  if(print_arq){print(arq)}
  if(nchar(arq) < 41){return(NULL)}
  h <- read_html(arq)

  tabelas <- h %>%
    html_nodes('.esajCelulaConteudoServico > table') %>%
    lapply(
      function(x){
        if(xml_length(x) > 0){
          r <- html_table(x, fill = T) %>%
            as.data.frame(stringAsFactors = F) %>%
            mutate_each(funs(as.character))
          if(ncol(r) > 1){
            return(r)
          } else {
            return(NULL)
          }
        } else {
          return(NULL)
        }
      })

  if(length(tabelas) == 0){return(NULL)}

  labels <- h %>%
    html_nodes('.esajCelulaConteudoServico > *')

  tags <-  html_name(labels)

  classes <- tipos_de_info %>% paste(collapse = '|')

  d <- labels[which(tags == 'table')-1] %>%
    html_text %>%
    stri_replace_all(regex = '[\n\r\t]',' ') %>%
    stri_replace_all(regex = ' +',' ') %>%
    data.frame(stringsAsFactors = F) %>%
    tbl_df() %>%
    setNames('label') %>%
    mutate(is_name = stri_detect(label, regex = classes),
           group = cumsum(is_name))

  if(grep('Exibindo',d$label) %>% length == 1){
    d[grep('Exibindo',d$label),'label']  = 'Movimentacoes'
  }else{
    d[grep('Exibindo',d$label),'label']  = c('partes_do_processo','Movimentacoes')
  }

  nomes <- d  %>% filter(is_name == 1)  %>% select(label)  %>% first %>% stri_trim  %>% unique

  grupos <- d$group %>% unique %>% setdiff(0)

  info_segunda_instancia = lapply(grupos,
                                  function(x){
                                    bind_rows(tabelas[which(d$group == x)])
                                  }) %>%
    data_frame(tipo_info = nomes) %>%
    setNames(c('value','tipo_info')) %>%
    mutate(x = 1) %>%
    spread(tipo_info, value) %>%
    select(-x) %>%
    clean_names %>%
    mutate(n_processo = arq)

  return(info_segunda_instancia)
}

#' @import xml2
#' @import rvest
#' @import plyr
#' @import dplyr
#' @import tidyr
#' @import stringi
#' @export
parse_cposg_info_outro <- function(arq,print_arq){
  if(print_arq){print(arq)}
  if(nchar(arq) < 41){return(NULL)}
  tabela_infos <- arq %>%
    read_html %>%
    html_nodes('.esajCelulaConteudoServico > div[class=""] > table[class="secaoFormBody"]') %>%
    html_table(fill = T)
  if(length(tabela_infos) ==0){return(NULL)}
  return(data_frame(infos = tabela_infos, n_processo = arq))
}



