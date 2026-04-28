#' Baixar vias do OpenStreetMap
#'
#' @param shape objeto sf usado para definir a área de busca.
#' @param highway tipos de vias do OSM.
#' @param timeout tempo máximo da consulta, em segundos.
#'
#' @return objeto sf com vias.
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
