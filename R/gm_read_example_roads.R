#' Ler vias de exemplo
#'
#' @return objeto sf com vias principais.
#' @export
gm_read_example_roads <- function() {
  path <- system.file("extdata", "vias_principais.gpkg", package = "geomarkbr")
  sf::st_read(path, quiet = TRUE)
}
