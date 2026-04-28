#' Ler vias de exemplo do OpenStreetMap
#'
#' Carrega uma camada de vias principais previamente preparada e incluída no
#' pacote para uso em aula. Os dados foram obtidos do OpenStreetMap e filtrados
#' para representar vias de maior hierarquia (por exemplo, \code{primary},
#' \code{secondary} e \code{tertiary}).
#'
#' @return Objeto \code{sf} contendo linhas representando vias.
#'
#' @examples
#' \dontrun{
#' vias <- gm_read_example_roads()
#' plot(sf::st_geometry(vias), col = "grey40")
#' }
#'
#' @export
gm_read_example_roads <- function() {

  path <- system.file("extdata", "vias_principais.gpkg", package = "geomarkbr")

  if (path == "") {
    stop("Arquivo de exemplo não encontrado no pacote.")
  }

  sf::st_read(path, quiet = TRUE)
}
