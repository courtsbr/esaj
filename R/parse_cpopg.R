
#' Parse data
#' @param parser A parser returned by [make_parser()]
#' @export
parse_data.cpopg <- function(parser) {

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting data
  get_data <- function(html) {

    # Get most of the data
    data <- html %>%
      xml2::xml_find_all("//*[@class='secaoFormBody']") %>%
      rvest::html_table(fill = TRUE) %>%
      purrr::pluck(2) %>%
      dplyr::as_tibble() %>%
      dplyr::select(-X3) %>%
      dplyr::filter(!is.na(X2)) %>%
      dplyr::mutate(
        X1 = str_replace_all(X1, ":", ""),
        X2 = str_replace_all(X2, "[\n\r\t]+", " "),
        X2 = str_replace_all(X2, " +", " "),
        X2 = str_replace_all(X2, ".+: ", "")) %>%
      dplyr::add_row(X2 = stringr::str_extract(.[[1, 2]], ".+(?= )")) %>%
      dplyr::add_row(X2 = stringr::str_extract(.[[1, 2]], "\\(.+\\)")) %>%
      dplyr::select(data = X1, value = X2)

    # Correct formatting issue
    data[c(3, 7, 11, 12), 1] <- c("\u00C1rea", "Lugar", "N\u00B0 Processo", "Status")

    # Get some extra data
    cdp <- html %>%
      rvest::html_text() %>%
      stringr::str_match("processoPK\\.cdProcesso=([^&]+)&") %>%
      as.character() %>%
      dplyr::last()
    digital <- html %>%
      rvest::html_nodes(".linkPasta") %>%
      purrr::when(length(.) == 0 ~ "", ~ rvest::html_text(.)) %>%
      dplyr::first() %>%
      stringr::str_detect(., "Este processo \u00e9 digital") %>%
      as.character()

    # Add new rows
    data %>%
      dplyr::add_row(data = "CD Processo", value = cdp) %>%
      dplyr::add_row(data = "Digital", value = digital)
  }

  # Add get_data to getters
  purrr::list_merge(parser, name = "data", getter = get_data)
}

#' Parse movements
#' @param parser A parser returned by [make_parser()]
#' @export
parse_movs.cpopg <- function(parser) {

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting movs
  get_movs <- function(html) {
    html %>%
      rvest::html_node("#tabelaTodasMovimentacoes") %>%
      xml2::xml_parent() %>%
      rvest::html_table(fill = TRUE) %>%
      dplyr::select(-2) %>%
      dplyr::as_tibble() %>%
      dplyr::select(date = Data, movement = Movimento) %>%
      tidyr::separate(movement,
        c("movement", "description"), sep = "\\n\\t\\t",
        extra = "merge", fill = "right") %>%
      dplyr::mutate(
        description = str_replace_all(description, "[\\t\\n]", ""),
        description = str_replace_all(description, "\\r", " "),
        description = str_replace_all(description, " +", " "),
        description = stringr::str_trim(description)) %>%
      dplyr::filter(date != "") %>%
      dplyr::distinct()
  }

  # Add get_movs to getters
  purrr::list_merge(parser, name = "movs", getter = get_movs)
}

#' Parse parts
#' @param parser A parser returned by [make_parser()]
#' @export
parse_parts.cpopg <- function(parser) {

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting parts
  get_parts <- function(html) {
    html %>%
      rvest::html_nodes("#tableTodasPartes") %>%
      purrr::when(length(.) == 0 ~ rvest::html_nodes(html, "#tablePartesPrincipais"), ~ .) %>%
      dplyr::first() %>%
      rvest::html_table() %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(
        X2 = stringr::str_split(X2, "\\t "),
        id = 1:nrow(.)) %>%
      tidyr::unnest(X2) %>%
      dplyr::mutate(
        part = str_replace_all(X1, "[^a-zA-Z]", ""),
        role = stringr::str_extract(X2, "(?<=\\t)[a-zA-Z]+(?=:)"),
        X2 = str_replace_all(X2, "(?<=\\t)[a-zA-Z]+(?=:)", ""),
        role = ifelse(is.na(role), part, role),
        name = str_replace_all(X2, "[^\\p{Alphabetic} ]", ""),
        name = stringr::str_trim(name)) %>%
      dplyr::select(id, name, part, role)
  }

  # Add get_parts to getters
  purrr::list_merge(parser, name = "parts", getter = get_parts)
}

#' Parse history
#' @param parser A parser returned by [make_parser()]
#' @export
parse_hist.cpopg <- function(parser) {

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting hist
  get_hist <- function(html) {

    # Fail table
    fail_tbl <- dplyr::tibble(
      a = character(), b = character(), c = character(),
      d = character(), e = character())

    html %>%
      rvest::html_node(xpath = '//table[@id="tdHistoricoDeClasses"]') %>%
      purrr::when(length(.) == 0 ~ fail_tbl, ~ rvest::html_table(., header = FALSE)) %>%
      purrr::set_names(c('date', 'type', 'class', 'area', 'reason')) %>%
      dplyr::as_tibble()
  }

  # Add get_hist to getters
  purrr::list_merge(parser, name = "hist", getter = get_hist)
}

#' Parse hearings
#' @param parser A parser returned by [make_parser()]
#' @export
parse_hearings.cpopg <- function(parser) {

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting hist
  get_hearings <- function(html) {

    xp <- "//a[@name='audienciasPlaceHolder']/following-sibling::table"
    d <- html %>%
      rvest::html_nodes(xpath = xp) %>%
      dplyr::first() %>%
      rvest::html_table(header = FALSE)
    if (any(stringr::str_detect(d$X1, 'N\u00e3o h\u00e1 Audi\u00eancias futuras'))) {
      d <- dplyr::data_frame(erro = 'nao_tem')
    } else {
      d <- html %>%
        rvest::html_nodes(xpath = xp) %>%
        rvest::html_table(header = FALSE)
      #     {.[1:(which(purrr::map_lgl(., ~any(.x$X3 == 'Classe')))[1] - 1)]} %>%

      X3 <- unlist(lapply(d, function(x){x$X3[1]}))
      k_max = ifelse(sum(X3 == 'Classe') > 0, (which(X3 == 'Classe')[1]-1), length(X3))
      d <- lapply(1:k_max,function(i){d[[i]]})

      d <- d %>%
        lapply(function(x) dplyr::mutate_each(x, dplyr::funs(as.character))) %>%
        dplyr::bind_rows() %>%
        dplyr::filter(X1 != '') %>%
        setNames(as.character(gsub('\\.', '', arrumar_key(.[1,])))) %>%
        dplyr::slice(-1) %>%
        dplyr::tbl_df()
    }
    d
  }

  # Add get_hearings to getters
  purrr::list_merge(parser, name = "hearings", getter = get_hearings)
}

#' Parse police department
#' @param parser A parser returned by [make_parser()]
#' @export
parse_pd.cpopg <- function(parser) {

  # Check class
  stopifnot("parser" %in% class(parser))

  # Function for getting pd
  get_pd <- function(html) {
    html %>%
      rvest::html_node(xpath = "//tbody[@id='dadosDaDelegacia']/..") %>%
      xml2::xml_parent() %>%
      purrr::when(length(.) == 0 ~ dplyr::tibble(), ~ rvest::html_table(., header = TRUE)) %>%
      dplyr::filter(Documento != "") %>%
      setNames(arrumar_key(names(.))) %>%
      dplyr::tbl_df()
  }

  # Add get_pd to getters
  purrr::list_merge(parser, name = "pd", getter = get_pd)
}








arrumar_key <- function(x) {
  desacentuar(stringr::str_replace_all(tolower(x), " +", "_"))
}

# @export
desacentuar <- function(x) {
  gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
}
