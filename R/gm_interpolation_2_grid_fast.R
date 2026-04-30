#' Interpolação areal rápida para grade
#'
#' Versão simplificada e rápida da interpolação areal utilizando
#' \code{sf::st_interpolate_aw()}. Indicada para uso didático e
#' situações em que o desempenho é prioritário.
#'
#' @param grid Objeto \code{sf} com a grade de destino.
#' @param base Objeto \code{sf} com os dados de origem.
#' @param vars_extensive Variáveis extensivas (ex: população).
#' @param vars_intensive Variáveis intensivas (ex: médias, percentuais).
#'
#' @return Objeto \code{sf} com variáveis interpoladas.
#'
#' @details
#' Esta função utiliza interpolação areal ponderada por área.
#' Assume distribuição uniforme das variáveis dentro de cada polígono.
#'
#' @examples
#' \dontrun{
#' grid_fast <- gm_interpolation_2_grid_fast(
#'   grid = grid,
#'   base = base,
#'   vars_extensive = "pop_abs",
#'   vars_intensive = "idade_media"
#' )
#' }
#'
#' @export
gm_interpolation_2_grid_fast <- function(
    grid,
    base,
    vars_extensive = NULL,
    vars_intensive = NULL
) {

  # --- checagens ---
  if (!inherits(grid, "sf") || !inherits(base, "sf")) {
    stop("grid e base devem ser objetos sf.")
  }

  if (is.na(sf::st_crs(grid)) || is.na(sf::st_crs(base))) {
    stop("grid e base devem possuir CRS definido.")
  }

  if (sf::st_crs(grid) != sf::st_crs(base)) {
    stop("grid e base devem estar no mesmo CRS.")
  }

  # --- resultado base ---
  grid_out <- grid

  # --- extensivas ---
  if (!is.null(vars_extensive)) {

    ext_sf <- sf::st_interpolate_aw(
      base[vars_extensive],
      grid,
      extensive = TRUE
    )

    grid_out <- dplyr::bind_cols(
      grid_out,
      sf::st_drop_geometry(ext_sf)
    )
  }

  # --- intensivas ---
  if (!is.null(vars_intensive)) {

    int_sf <- sf::st_interpolate_aw(
      base[vars_intensive],
      grid,
      extensive = FALSE
    )

    grid_out <- dplyr::bind_cols(
      grid_out,
      sf::st_drop_geometry(int_sf)
    )
  }

  grid_out
}
