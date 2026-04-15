#' Calcula a idade mediana aproximada por linha a partir de faixas etárias
#'
#' @param df data frame com as colunas das faixas etárias.
#' @param colunas vetor com os nomes das colunas de faixas etárias.
#' @param breaks_inf vetor com os limites inferiores das faixas.
#' @param breaks_sup vetor com os limites superiores das faixas.
#' @param na_strategy estratégia para tratamento de NA:
#'   "zero" substitui NA por 0;
#'   "ignore" retorna NA para a linha se houver qualquer NA.
#'
#' @return vetor numérico com a idade mediana aproximada por linha.
#' @export
gm_mediana_idade <- function(df,
                             colunas = c(
                               "pop_abs_00_04",
                               "pop_abs_05_09",
                               "pop_abs_10_14",
                               "pop_abs_15_19",
                               "pop_abs_20_24",
                               "pop_abs_25_29",
                               "pop_abs_30_39",
                               "pop_abs_40_49",
                               "pop_abs_50_59",
                               "pop_abs_60_69",
                               "pop_abs_70_mais"
                             ),
                             breaks_inf = c(0, 5, 10, 15, 20, 25, 30, 40, 50, 60, 70),
                             breaks_sup = c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70, 100),
                             na_strategy = c("zero", "ignore")) {

  na_strategy <- match.arg(na_strategy)

  pops <- df |>
    dplyr::select(dplyr::all_of(colunas)) |>
    as.matrix()

  if (length(colunas) != length(breaks_inf) || length(colunas) != length(breaks_sup)) {
    stop("O número de colunas deve ser igual ao número de intervalos definidos em breaks_inf e breaks_sup.")
  }

  h <- breaks_sup - breaks_inf
  mediana <- rep(NA_real_, nrow(pops))

  for (i in seq_len(nrow(pops))) {
    linha <- pops[i, ]

    if (na_strategy == "ignore" && anyNA(linha)) {
      mediana[i] <- NA_real_
      next
    }

    if (na_strategy == "zero") {
      linha[is.na(linha)] <- 0
    }

    total <- sum(linha)

    if (is.na(total) || total == 0) {
      mediana[i] <- NA_real_
      next
    }

    half <- total / 2
    cum <- cumsum(linha)
    classe <- which(cum >= half)[1]

    if (is.na(classe)) {
      mediana[i] <- NA_real_
      next
    }

    if (classe == 1) {
      freq_acum_anterior <- 0
    } else {
      freq_acum_anterior <- cum[classe - 1]
    }

    freq_classe <- linha[classe]
    limite_inferior <- breaks_inf[classe]

    if (is.na(freq_classe) || freq_classe == 0) {
      mediana[i] <- NA_real_
    } else {
      mediana[i] <- limite_inferior +
        ((half - freq_acum_anterior) / freq_classe) * h[classe]
    }
  }

  mediana
}
