#' Calcula a idade média aproximada por linha a partir de faixas etárias
#'
#' @param df data frame com as colunas das faixas etárias.
#' @param colunas vetor com os nomes das colunas de faixas etárias.
#' @param mid vetor com os pontos médios de cada faixa.
#' @param na_strategy estratégia para tratamento de NA:
#'   "zero" substitui NA por 0;
#'   "ignore" retorna NA para a linha se houver qualquer NA.
#'
#' @return vetor numérico com a idade média aproximada por linha.
#' @export
gm_media_idade <- function(df,
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
                           mid = c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5,
                                   34.5, 44.5, 54.5, 64.5, 75),
                           na_strategy = c("zero", "ignore")) {

  na_strategy <- match.arg(na_strategy)

  pops <- df |>
    dplyr::select(dplyr::all_of(colunas)) |>
    as.matrix()

  if (length(colunas) != length(mid)) {
    stop("O número de colunas deve ser igual ao número de pontos médios em 'mid'.")
  }

  media <- rep(NA_real_, nrow(pops))

  for (i in seq_len(nrow(pops))) {
    linha <- pops[i, ]

    if (na_strategy == "ignore" && anyNA(linha)) {
      media[i] <- NA_real_
      next
    }

    if (na_strategy == "zero") {
      linha[is.na(linha)] <- 0
    }

    total <- sum(linha)

    if (is.na(total) || total == 0) {
      media[i] <- NA_real_
      next
    }

    media[i] <- sum(linha * mid) / total
  }

  media
}
