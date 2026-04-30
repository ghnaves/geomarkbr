#' Ler ciclovias de exemplo do OpenStreetMap
#'
#' Carrega dados de infraestrutura cicloviária previamente preparados e
#' incluídos no pacote para uso em aula. Os dados foram obtidos do
#' OpenStreetMap em Campos dos Gotytacazes
#'
#'
#' @return Objeto \code{sf} contendo linhas representando ciclovias.
#'
#' @examples
#' \dontrun{
#' ciclovias <- gm_read_example_cycleways()
#'
#' plot(sf::st_geometry(ciclovias), col = "blue")
#'
#' }
#' @export
gm_read_example_cycleways <- function() {
  file = 'ciclovias.gpkg'

  path <- system.file("extdata", file, package = "geomarkbr")
  if (path == "") {
    stop("Arquivo de exemplo não encontrado no pacote.")
  }

  sf::st_read(
    system.file("extdata", file, package = "geomarkbr"),
    quiet = TRUE
  )
}
