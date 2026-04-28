#' Ler ciclovias de exemplo do OpenStreetMap
#'
#' Carrega dados de infraestrutura cicloviária previamente preparados e
#' incluídos no pacote para uso em aula. Os dados foram obtidos do
#' OpenStreetMap e representam duas formas distintas de modelagem de
#' ciclovias:
#'
#' \itemize{
#'   \item \code{"cycleway"}: vias exclusivas para bicicletas
#'   (tag \code{highway = cycleway})
#'   \item \code{"cycleway_tag"}: ciclovias associadas a vias existentes
#'   (tag \code{cycleway = *})
#' }
#'
#' Essa distinção é importante, pois a infraestrutura cicloviária pode
#' estar representada tanto como uma geometria própria quanto como um
#' atributo de uma via.
#'
#' @param type Tipo de dado a ser carregado. Deve ser um dos seguintes:
#'   \code{"cycleway"} ou \code{"cycleway_tag"}.
#'
#' @return Objeto \code{sf} contendo linhas representando ciclovias.
#'
#' @examples
#' \dontrun{
#' ciclovias <- gm_read_example_cycleways("cycleway")
#' ciclofaixas <- gm_read_example_cycleways("cycleway_tag")
#'
#' plot(sf::st_geometry(ciclovias), col = "blue")
#' plot(sf::st_geometry(ciclofaixas), add = TRUE, col = "green")
#' }
#'
#' @export
gm_read_example_cycleways <- function(type = c("cycleway", "cycleway_tag")) {
  type <- match.arg(type)

  file <- switch(
    type,
    "cycleway" = "osm_ciclovias_highway_cycleway.gpkg",
    "cycleway_tag" = "osm_ciclovias_cycleway_tag.gpkg"
  )

  path <- system.file("extdata", file, package = "geomarkbr")
  if (path == "") {
    stop("Arquivo de exemplo não encontrado no pacote.")
  }

  sf::st_read(
    system.file("extdata", file, package = "geomarkbr"),
    quiet = TRUE
  )
}
