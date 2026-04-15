#' Ler bairros de um município
#'
#' @param code_muni código IBGE do município
#' @return objeto sf
#' @export
gm_read_neighborhoods <- function(code_muni) {
  bairros <- geobr::read_neighborhood(year = 2010, simplified = FALSE)
  bairros |>
    dplyr::filter(code_muni == !!code_muni)
}
