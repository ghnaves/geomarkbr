#' Baixar vias do OpenStreetMap
#'
#' Realiza o download de dados de vias do OpenStreetMap (OSM) a partir da
#' extensão espacial de um objeto \code{sf}. A consulta é feita via Overpass API,
#' retornando geometrias do tipo linha (\code{LINESTRING}).
#'
#' @param shape Objeto \code{sf} usado para definir a área de busca.
#'   A extensão do objeto é convertida para coordenadas geográficas antes
#'   da consulta.
#' @param highway Vetor de caracteres com os tipos de vias a serem retornados,
#'   conforme a classificação do OSM (por exemplo: \code{"primary"},
#'   \code{"secondary"}, \code{"residential"}).
#' @param timeout Tempo máximo da consulta, em segundos. Consultas em áreas
#'   maiores podem exigir valores mais altos.
#'
#' @return Objeto \code{sf} contendo as vias retornadas pela consulta.
#'
#' @details
#' Os dados são obtidos a partir da Overpass API, que pode apresentar limitações
#' de desempenho ou indisponibilidade temporária. Em alguns casos, consultas
#' extensas podem resultar em erros ou demoras significativas.
#'
#' Recomenda-se utilizar filtros mais restritivos em \code{highway} ou áreas
#' menores para garantir melhor desempenho.
#'
#' @examples
#' \dontrun{
#' base <- gm_join_data(
#'   gm_read_sectors(gm_example_data("setores.gpkg")),
#'   gm_read_attributes(gm_example_data("variaveis.rds"))
#' )
#'
#' vias <- gm_osm_roads(base)
#'
#' plot(sf::st_geometry(vias), col = "grey40")
#' }
#'
#' @export
gm_osm_roads <- function(shape,
                         highway = c("motorway", "trunk", "primary", "secondary", "tertiary"),
                         timeout = 180) {

  bbox <- sf::st_bbox(sf::st_transform(shape, 4674))

  osm <- osmdata::opq(bbox = bbox, timeout = timeout) |>
    osmdata::add_osm_feature(key = "highway", value = highway) |>
    osmdata::osmdata_sf()

  osm$osm_lines
}
