#' Calcular idade média aproximada a partir de faixas etárias
#'
#' Calcula a idade média aproximada por linha de um conjunto de dados,
#' utilizando a distribuição da população em faixas etárias e os pontos
#' médios de cada faixa.
#'
#' Essa abordagem é necessária porque os dados do Censo são disponibilizados
#' em grupos de idade, e não como idade individual.
#'
#' @param df Data frame contendo as colunas com as contagens populacionais
#'   por faixa etária.
#' @param colunas Vetor de caracteres com os nomes das colunas que representam
#'   as faixas etárias.
#' @param mid Vetor numérico com os pontos médios de cada faixa etária.
#'   Deve ter o mesmo comprimento que \code{colunas}.
#' @param na_strategy Estratégia para tratamento de valores ausentes:
#'   \itemize{
#'     \item \code{"zero"}: substitui \code{NA} por 0 antes do cálculo
#'     \item \code{"ignore"}: retorna \code{NA} para a linha se houver qualquer \code{NA}
#'   }
#'
#' @return Vetor numérico com a idade média aproximada para cada linha.
#'
#' @details
#' A idade média é calculada como uma média ponderada, em que os pesos são
#' as populações em cada faixa etária e os valores são os pontos médios das
#' respectivas faixas.
#'
#' Como os dados são agregados, o resultado é uma aproximação e depende da
#' escolha dos pontos médios.
#'
#' @examples
#' \dontrun{
#' dados <- gm_read_attributes(gm_example_data("variaveis.rds"))
#'
#' dados <- dados |>
#'   dplyr::mutate(
#'     idade_media = gm_media_idade(.)
#'   )
#' }
#'
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
