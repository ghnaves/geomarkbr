#' Criar grade regular
#'
#' Cria uma grade regular a partir da extensão espacial de um objeto `sf`.
#'
#' A grade pode ser criada em coordenadas métricas, como UTM, ou em coordenadas
#' geográficas, alinhada a latitude e longitude.
#'
#' @param shape Objeto `sf`, como, p. ex. a malha de setores censitários.
#' @param type Tipo de grade. Use `"metric"` para grade em metros ou `"latlon"`
#'   para grade em graus.
#' @param cellsize Tamanho da célula. Quando `type = "metric"`, deve ser um
#'   valor numérico em metros. Quando `type = "latlon"`, pode ser um valor
#'   numérico em graus ou um dos valores: `"1d"`, `"30m"`, `"15m"`, `"1m"`,
#'   `"30s"` ou `"15s"`.
#' @param crs_out Sistema de coordenadas usado para criar a grade métrica.
#'   O padrão é `31984`, correspondente a SIRGAS 2000 / UTM zone 24S.
#'   Para SIRGAS2000 latlong use `4674`
#' @param clip Recortar a grid no formato da shape
#'
#' @return Objeto `sf` com a grade regular.
#' @export
gm_make_grid <- function(shape,
                         type = c("metric", "latlon"),
                         cellsize = 100,
                         crs_out = 31984,
                         clip = TRUE) {

  type <- match.arg(type)

  if (!inherits(shape, "sf")) {
    stop("O objeto informado em 'shape' precisa ser um objeto sf.")
  }

  if (is.null(sf::st_geometry(shape))) {
    stop("O objeto sf não possui coluna de geometria válida.")
  }

  if (type == "metric") {

    shape_grid <- sf::st_transform(shape, crs_out)

    if (sf::st_is_longlat(shape_grid)) {
      warning("O CRS informado em 'crs_out' parece ser geográfico (graus). ",
              "Para grade métrica, use um CRS projetado (ex: UTM).")
    }

    grid <- sf::st_make_grid(
      shape_grid,
      cellsize = cellsize,
      square = TRUE
    )

  } else {

    shape_grid <- sf::st_transform(shape, crs_out)

    if (!sf::st_is_longlat(shape_grid)) {
      warning("A transformação para coordenadas geográficas pode não ter sido aplicada corretamente.")
    }

    if (is.character(cellsize)) {
      cellsize <- switch(
        cellsize,
        "1d"  = 1,
        "30m" = 30 / 60,
        "15m" = 15 / 60,
        "1m"  = 1 / 60,
        "30s" = 30 / 3600,
        "15s" = 15 / 3600,
        stop(
          "cellsize inválido para grade latlon. ",
          "Valores válidos: 1d, 30m, 15m, 1m, 30s e 15s."
        )
      )
    }

    grid <- sf::st_make_grid(
      shape_grid,
      cellsize = c(cellsize, cellsize),
      square = TRUE
    )
  }

  grid <- sf::st_as_sf(grid)
  names(grid) <- "geom"
  sf::st_geometry(grid) <- "geom"
  grid$id_grid <- seq_len(nrow(grid))

  if (clip) {
    shape_union <- sf::st_union(sf::st_geometry(shape_grid))
    keep <- sf::st_intersects(grid, shape_union, sparse = FALSE)[, 1]
    grid <- grid[keep, ]
    sf::st_geometry(grid) <- "geom"
  }

  grid
}
