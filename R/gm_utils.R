gm_resolve_census_code <- function(code, filter_var = NULL, state = NULL) {

  if (is.null(code)) {
    return(list(
      download_code = NULL,
      filter_code = NULL,
      needs_filter = FALSE
    ))
  }

  code_chr <- as.character(code)

  if (code_chr == "all") {
    return(list(
      download_code = "all",
      filter_code = NULL,
      needs_filter = FALSE
    ))
  }

  if (grepl("^[A-Z]{2}$", code_chr)) {
    return(list(
      download_code = code_chr,
      filter_code = NULL,
      needs_filter = FALSE
    ))
  }

  if (grepl("^[0-9]{2}$", code_chr)) {
    return(list(
      download_code = as.numeric(code_chr),
      filter_code = NULL,
      needs_filter = FALSE
    ))
  }

  if (grepl("^[0-9]{7}$", code_chr)) {
    return(list(
      download_code = as.numeric(code_chr),
      filter_code = NULL,
      needs_filter = FALSE
    ))
  }

  if (grepl("^[0-9]+$", code_chr) && nchar(code_chr) > 7) {

    if (is.null(filter_var)) {
      stop(
        "Para filtrar por código territorial diferente de UF ou município, ",
        "informe 'filter_var'."
      )
    }

    return(list(
      download_code = as.numeric(substr(code_chr, 1, 2)),
      filter_code = code_chr,
      needs_filter = TRUE
    ))
  }

  # Caso: filtro por texto, como nome de bairro, distrito etc.
  if (!grepl("^[0-9]+$", code_chr)) {

    if (is.null(filter_var)) {
      stop("Para filtrar por texto, informe 'filter_var'.")
    }

    if (is.null(state)) {
      stop(
        "Para filtrar por texto, informe 'state', por exemplo state = 'RJ'."
      )
    }

    return(list(
      download_code = state,
      filter_code = code_chr,
      needs_filter = TRUE
    ))
  }

  stop("Formato de 'code' não reconhecido.")
}


gm_filter_by_code <- function(x, filter_var, filter_code) {

  if (is.null(filter_var) || is.null(filter_code)) {
    return(x)
  }

  if (!filter_var %in% names(x)) {
    stop("A variável '", filter_var, "' não foi encontrada na base.")
  }

  dplyr::filter(
    x,
    as.character(.data[[filter_var]]) == as.character(filter_code)
  )
}

gm_resolve_data_filter <- function(code, filter_var = NULL, state = NULL) {

  if (is.null(code) || as.character(code) == "all") {
    return(list(
      filter_var = NULL,
      filter_code = NULL,
      state_filter_var = NULL,
      state_filter_code = NULL,
      needs_filter = FALSE
    ))
  }

  code_chr <- as.character(code)

  if (is.null(filter_var)) {
    filter_var <- dplyr::case_when(
      grepl("^[A-Z]{2}$", code_chr)  ~ "code_state",
      grepl("^[0-9]{2}$", code_chr)  ~ "code_state",
      grepl("^[0-9]{7}$", code_chr)  ~ "code_muni",
      grepl("^[0-9]{9}$", code_chr)  ~ "code_district",
      grepl("^[0-9]{11}$", code_chr) ~ "code_subdistrict",
      grepl("^[0-9]{15}$", code_chr) ~ "code_tract",
      TRUE ~ NA_character_
    )
  }

  if (is.na(filter_var)) {
    stop(
      "Não foi possível inferir 'filter_var' para code = ", code_chr, ".\n",
      "Informe manualmente, por exemplo: 'name_neighborhood', ",
      "'code_neighborhood', 'name_district', 'code_favela' etc."
    )
  }

  filter_code <- if (grepl("^[A-Z]{2}$", code_chr) && filter_var == "code_state") {
    gm_state_abbrev_to_code(code_chr)
  } else {
    code_chr
  }

  state_filter_var <- NULL
  state_filter_code <- NULL

  if (!grepl("^[0-9]+$", code_chr) && !grepl("^[A-Z]{2}$", code_chr)) {
    if (!is.null(state)) {
      state_filter_var <- "code_state"
      state_filter_code <- gm_state_abbrev_to_code(state)
    }
  }

  list(
    filter_var = filter_var,
    filter_code = filter_code,
    state_filter_var = state_filter_var,
    state_filter_code = state_filter_code,
    needs_filter = TRUE
  )
}

gm_state_abbrev_to_code <- function(abbrev) {
  uf <- c(
    RO = 11, AC = 12, AM = 13, RR = 14, PA = 15, AP = 16, TO = 17,
    MA = 21, PI = 22, CE = 23, RN = 24, PB = 25, PE = 26, AL = 27, SE = 28, BA = 29,
    MG = 31, ES = 32, RJ = 33, SP = 35,
    PR = 41, SC = 42, RS = 43,
    MS = 50, MT = 51, GO = 52, DF = 53
  )

  abbrev <- toupper(abbrev)

  if (!abbrev %in% names(uf)) {
    stop("Sigla de UF inválida: ", abbrev)
  }

  unname(uf[abbrev])
}
