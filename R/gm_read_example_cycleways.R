gm_read_example_cycleways <- function(type = c("cycleway", "cycleway_tag")) {
  type <- match.arg(type)

  file <- switch(
    type,
    "cycleway" = "osm_ciclovias_highway_cycleway.gpkg",
    "cycleway_tag" = "osm_ciclovias_cycleway_tag.gpkg"
  )

  sf::st_read(
    system.file("extdata", file, package = "geomarkbr"),
    quiet = TRUE
  )
}
