#' Calcular idade mediana aproximada a partir de faixas etárias
#'
#' Calcula a idade mediana aproximada por linha de um conjunto de dados,
#' utilizando a distribuição da população em faixas etárias e os limites
#' inferior e superior de cada faixa.
#'
#' Essa abordagem é necessária porque os dados do Censo são disponibilizados
#' em grupos de idade, e não como idade individual.
#'
#' @param df Data frame contendo as colunas com as contagens populacionais
#'   por faixa etária.
#' @param colunas Vetor de caracteres com os nomes das colunas que representam
#'   as faixas etárias.
#' @param breaks_inf Vetor numérico com os limites inferiores das faixas etárias.
#' @param breaks_sup Vetor numérico com os limites superiores das faixas etárias.
#'   Deve ter o mesmo comprimento que \code{colunas}.
#' @param na_strategy Estratégia para tratamento de valores ausentes:
#'   \itemize{
#'     \item \code{"zero"}: substitui \code{NA} por 0 antes do cálculo
#'     \item \code{"ignore"}: retorna \code{NA} para a linha se houver qualquer \code{NA}
#'   }
#'
#' @return Vetor numérico com a idade mediana aproximada para cada linha.
#'
#' @details
#' A idade mediana é estimada a partir da distribuição acumulada das
#' frequências nas faixas etárias, identificando a classe em que se encontra
#' o percentil de 50\% da população. Dentro dessa classe, é realizada uma
#' interpolação linear para obter o valor aproximado da mediana.
#'
#' Como os dados são agregados em intervalos, o resultado é uma aproximação
#' e depende da definição das faixas etárias.
#'
#' @examples
#' \dontrun{
#' dados <- gm_read_attributes(gm_example_data("variaveis.rds"))
#'
#' dados <- dados |>
#'   dplyr::mutate(
#'     idade_mediana = gm_mediana_idade(.)
#'   )
#' }
#'
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
