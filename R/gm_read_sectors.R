#' Ler setores censitários
#'
#' @param path caminho para geopackage/shapefile dos setores.
#' @param code_muni código IBGE do município, opcional.
#' @param year ano da malha.
#' @return objeto sf
#' @export
gm_read_sectors <- function(path = NULL, code_muni = NULL, year = 2022) {
  if (!is.null(path)) {
    setores <- sf::st_read(path, quiet = TRUE)
    return(setores)
  }

  if (!is.null(code_muni)) {
    setores <- geobr::read_census_tract(
      code_tract = code_muni,
      year = year,
      simplified = FALSE
    )
    return(setores)
  }

}
