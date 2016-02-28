#' Consulta processos primeiro grau TJSP
#'
#' @export
cpo_pg <- function(processos, path = "data-raw/cpo-pg") {
  # f <- dplyr::failwith(dplyr::data_frame(result = "erro"), cpo_pg_um)
  d <- dplyr::data_frame(n_processo = unique(processos))
  d <- dplyr::mutate(d, id = 1:n())
  clust <- multidplyr::create_cluster(parallel::detectCores())
  d <- multidplyr::partition(d, id, n_processo, cluster = clust)
  d <- dplyr::do(d, {
    cpo_pg_um <- function(p, path) {
      p <- gsub("[^0-9]", "", p)
      arq <- sprintf("%s/%s.html", path, p)
      if (!is.null(path) & file.exists(arq)) {
        return(dplyr::data_frame(result = "arquivo existe"))
      }
      # Sys.sleep(1)
      u <- brunoSalama:::build_url_cpo_pg(p)
      if (!file.exists(arq)) {
        r <- httr::GET(u, httr::config(ssl_verifypeer = FALSE),
                       httr::write_disk(arq))
      }
      k <- TRUE
      while (r$status_code != 200) {
        if (k)
          cat("\nesperando...")
        else cat("...")
        if (!file.exists(arq)) {
          r <- httr::GET(u,
                         httr::config(ssl_verifypeer = FALSE),
                         httr::write_disk(arq))
        }
        k <- FALSE
      }
      if (!k) cat("\n")
      return(dplyr::data_frame(result = "OK"))
    }
    # cat(.$id, '\n', file = 'data-raw/log.txt', append = TRUE)
    f <- dplyr::failwith(dplyr::data_frame(result = "erro"), cpo_pg_um)
    f(.$n_processo, path = path)
  })
  d <- dplyr::collect(d)
  d <- dplyr::ungroup(d)
  d
}

#' @export
parse_cpopg <- function (arqs, .parallel = TRUE) {
  if (.parallel) {
    fun <- function(i, a, len) {
      "%>%" <- dplyr::`%>%`
      if (runif(1) < 0.01)
        cat(i, "de", len, "\n")
      x <- a[i]
      dplyr::data_frame(arq = x, infos = list(esaj::parse_cpopg_info_(x)),
                        partes = list(esaj::parse_cpopg_partes_(x)),
                        movs = list(esaj::parse_cpopg_movs_(x)))
    }
    d_fail_tbl <- list(dplyr::data_frame("error"))
    d_fail <- dplyr::data_frame(arq = NA, infos = d_fail_tbl,
                                partes = d_fail_tbl, movs = d_fail_tbl)
    f <- dplyr::failwith(d_fail, fun)
    cl <- parallel::makeCluster(parallel::detectCores(),
                                outfile = "")
    doParallel::registerDoParallel(cl)
    n <- length(arqs)
    d <- dplyr::tbl_df(plyr::ldply(seq_len(n), f, a = arqs,
                                   len = n, .parallel = TRUE))
    parallel::stopCluster(cl)
  }
  else {
    d <- dplyr::data_frame(arq = arqs) %>%
      dplyr::distinct(arq) %>%
      dplyr::group_by(arq) %>%
      dplyr::do(infos = parse_cpopg_info_(.$arq),
                partes = parse_cpopg_partes_(.$arq),
                movs = parse_cpopg_movs_(.$arq)) %>%
      dplyr::ungroup()
  }
  d
}

#' @export
parse_cpopg_info_ <- function (a) {
  html <- xml2::read_html(a)
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
  infos_p <- infos %>% dplyr::filter(key == "processo") %>%
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


arrumar_forma <- function(x) {
  x <- desacentuar(stringr::str_replace_all(tolower(x), " +", "_"))
  x <- gsub("[^a-z]", "", x)
  x
}

#' @export
parse_cpopg_partes_ <- function(a) {
  html <- xml2::read_html(a)
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
parse_cpopg_movs_ <- function (a) {
  a %>%
    xml2::read_html() %>%
    rvest::html_node("#tabelaTodasMovimentacoes") %>%
    rvest::html_table() %>%
    dplyr::select(data_mov = X1, X3) %>%
    tidyr::separate(X3, c("titulo", "mov"), sep = "\r\n\t", extra = "merge", fill = "right") %>%
    dplyr::tbl_df()
}



