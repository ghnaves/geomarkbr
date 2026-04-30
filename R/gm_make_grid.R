#' Criar grade regular
#'
#' Cria uma grade regular a partir da extensão espacial de um objeto `sf`.
#'
#' A grade pode ser criada em coordenadas métricas, como UTM, ou em coordenadas
#' geográficas, alinhada à latitude e longitude.
#'
#' @param shape Objeto `sf`, como a malha de setores censitários.
#' @param type Tipo de grade. Use `"metric"` para grade em metros ou `"latlon"`
#'   para grade em graus.
#' @param cellsize Tamanho da célula. Quando `type = "metric"`, deve ser um
#'   valor numérico em metros. Quando `type = "latlon"`, pode ser um valor
#'   numérico em graus ou um dos valores: `"1d"`, `"30m"`, `"15m"`, `"1m"`,
#'   `"30s"` ou `"15s"`.
#' @param offset Origem da grade. O default é latitude e longitude zero
#' @param crs_out Sistema de coordenadas usado para criar a grade. Se `NULL`,
#'   usa `31984` para `type = "metric"` e `4674` para `type = "latlon"`.
#' @param clip Se `TRUE`, mantém apenas as células que intersectam a geometria
#'   de `shape`.
#'
#' @return Objeto `sf` com a grade regular.
#'
#' @examples
#' \dontrun{
#' base <- gm_join_data(
#'   gm_read_sectors(gm_example_data("setores.gpkg")),
#'   gm_read_attributes(gm_example_data("variaveis.rds"))
#' )
#'
#' grid_utm <- gm_make_grid(base, type = "metric", cellsize = 1000)
#' grid_latlon <- gm_make_grid(base, type = "latlon", cellsize = "15s")
#' }
#'
#' @export
gm_make_grid <- function(
    shape,
    type = c("metric", "latlon"),
    cellsize = 100,
    offset = NULL,
    crs_out = NULL,
    clip = TRUE
) {

  type <- match.arg(type)

  if (!inherits(shape, "sf")) {
    stop("O objeto informado em 'shape' precisa ser um objeto sf.")
  }

  if (is.na(sf::st_crs(shape))) {
    stop("O objeto informado em 'shape' não possui CRS definido.")
  }

  if (is.null(crs_out)) {
    crs_out <- if (type == "metric") 31984 else 4674
  }

  # --- reprojetar ---
  shape_grid <- sf::st_transform(shape, crs_out)

  # --- tratar cellsize ---
  if (type == "metric") {

    if (sf::st_is_longlat(shape_grid)) {
      warning("CRS parece geográfico. Para grade métrica use CRS projetado.")
    }

    if (!is.numeric(cellsize)) {
      stop("Para type='metric', cellsize deve ser numérico (metros).")
    }

    cellsize_use <- cellsize

  } else {

    if (!sf::st_is_longlat(shape_grid)) {
      warning("CRS não parece geográfico.")
    }

    if (is.character(cellsize)) {
      cellsize <- switch(
        cellsize,
        "1d"  = 1,
        "30m" = 30/60,
        "15m" = 15/60,
        "1m"  = 1/60,
        "30s" = 30/3600,
        "15s" = 15/3600,
        stop("cellsize inválido")
      )
    }

    cellsize_use <- c(cellsize, cellsize)
  }

  # --- offset ---
  if (is.null(offset)) {
    offset_use <- NULL
  } else {
    if (!is.numeric(offset) || length(offset) != 2) {
      stop("offset deve ser numérico de comprimento 2")
    }
    offset_use <- offset
  }

  # --- criar grid ---
  if (is.null(offset)) {

    grid <- sf::st_make_grid(
      shape_grid,
      cellsize = cellsize_use,
      square = TRUE
    )

  } else {

    grid <- sf::st_make_grid(
      shape_grid,
      cellsize = cellsize_use,
      offset = offset,
      square = TRUE
    )

  }

  # --- converter ---
  grid <- sf::st_as_sf(grid)
  names(grid) <- "geom"
  sf::st_geometry(grid) <- "geom"
  grid$id_grid <- seq_len(nrow(grid))

  # --- recorte ---
  if (clip) {
    shape_union <- sf::st_union(sf::st_geometry(shape_grid))
    keep <- sf::st_intersects(grid, shape_union, sparse = FALSE)[, 1]
    grid <- grid[keep, ]
  }

  grid
}
