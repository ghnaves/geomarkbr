#' Criar grade regular
#'
#' @param x objeto sf
#' @param cellsize tamanho da célula em metros
#' @return objeto sf com grid
#' @export
gm_make_grid <- function(x, cellsize = 500) {
  x_proj <- sf::st_transform(x, 31983) # exemplo SIRGAS 2000 / UTM 23S
  grd <- sf::st_make_grid(x_proj, cellsize = cellsize, square = TRUE)
  grd <- sf::st_as_sf(grd)
  names(grd) <- "geometry"
  grd$grid_id <- seq_len(nrow(grd))
  grd
}
