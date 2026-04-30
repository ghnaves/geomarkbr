#' Interpolação areal para grade regular
#'
#' Realiza interpolação espacial de dados de polígonos (por exemplo, setores
#' censitários) para uma grade regular, utilizando o método de ponderação por área.
#'
#' A função distingue entre variáveis extensivas (contagens, que devem ser
#' redistribuídas proporcionalmente à área) e variáveis intensivas (médias ou
#' proporções, que devem ser calculadas por média ponderada).
#'
#' @param grid Objeto \code{sf} representando a grade de destino.
#' @param base Objeto \code{sf} com os dados de origem (por exemplo, setores).
#' @param vars_extensive Vetor de nomes de variáveis extensivas (ex: população).
#' @param vars_intensive Vetor de nomes de variáveis intensivas (ex: médias ou percentuais).
#'
#' @return Objeto \code{sf} da grade com variáveis interpoladas.
#'
#' @details
#' A interpolação assume distribuição uniforme das variáveis dentro de cada
#' polígono de origem. Para variáveis extensivas, a soma é preservada.
#' Para variáveis intensivas, calcula-se uma média ponderada pela área.
#'
#' @examples
#' \dontrun{
#' grid <- gm_make_grid(base, type = "metric", cellsize = 1000)
#'
#' grid_interp <- gm_interpolation_2_grid(
#'   grid = grid,
#'   base = base,
#'   vars_extensive = c("pop_abs"),
#'   vars_intensive = c("idade_media")
#' )
#' }
#'
#' @export
gm_interpolation_2_grid <- function(
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

  if (is.null(vars_extensive) && is.null(vars_intensive)) {
    stop("Informe ao menos uma variável em vars_extensive ou vars_intensive.")
  }

  # --- garantir IDs ---
  if (!"id_grid" %in% names(grid)) {
    grid$id_grid <- seq_len(nrow(grid))
  }

  if (!"id_base" %in% names(base)) {
    base$id_base <- seq_len(nrow(base))
  }

  # --- interseção ---
  inter_sf <- sf::st_intersection(
    sf::st_make_valid(base),
    sf::st_make_valid(grid)
  )

  # --- áreas ---
  inter_sf$area_inter <- sf::st_area(inter_sf)

  base_area <- sf::st_area(base)
  inter_sf$area_base <- base_area[match(inter_sf$id_base, base$id_base)]

  # --- peso ---
  inter_sf$w <- as.numeric(inter_sf$area_inter / inter_sf$area_base)

  # --- extensivas ---
  if (!is.null(vars_extensive)) {

    for (v in vars_extensive) {
      inter_sf[[v]] <- inter_sf[[v]] * inter_sf$w
    }

    extensive_res <- inter_sf |>
      dplyr::group_by(id_grid) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(vars_extensive),
          ~ sum(.x, na.rm = TRUE)
        ),
        .groups = "drop"
      )
  } else {
    extensive_res <- NULL
  }

  # --- intensivas ---
  if (!is.null(vars_intensive)) {

    intensive_res <- inter_sf |>
      dplyr::group_by(id_grid) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(vars_intensive),
          ~ sum(.x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        ),
        .groups = "drop"
      )
  } else {
    intensive_res <- NULL
  }

  # --- join final ---
  grid_out <- grid
  if (!is.null(extensive_res)) {
    grid_out <- dplyr::left_join(
      grid_out,
      sf::st_drop_geometry(extensive_res),
      by = "id_grid"
    )
  }

  if (!is.null(intensive_res)) {
    grid_out <- dplyr::left_join(
      grid_out,
      sf::st_drop_geometry(intensive_res),
      by = "id_grid"
    )
  }

  grid_out
}
