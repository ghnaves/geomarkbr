#' Criar mapa com duas camadas espaciais
#'
#' Cria um mapa simples com uma camada de base e uma camada sobreposta.
#' A função identifica automaticamente se a camada sobreposta é formada por
#' pontos, linhas ou polígonos.
#'
#' @param base Objeto `sf` da camada de base, por exemplo setores censitários.
#' @param overlay Objeto `sf` da camada sobreposta, por exemplo vias, ciclovias
#'   ou uma grade regular.
#' @param titulo Título do mapa. Se `NULL`, o mapa é criado sem título.
#' @param base_fill Cor de preenchimento da camada de base.
#' @param base_col Cor da borda da camada de base.
#' @param base_alpha Transparência do preenchimento da camada de base.
#' @param overlay_fill Cor de preenchimento da camada sobreposta, usada quando
#'   `overlay` é uma camada de polígonos.
#' @param overlay_col Cor da camada sobreposta.
#' @param overlay_alpha Transparência da camada sobreposta.
#' @param overlay_lwd Espessura da camada sobreposta, usada quando `overlay`
#'   é uma camada de linhas.
#'
#' @return Objeto `tmap`.
#'
#' @examples
#' \dontrun{
#' base <- gm_join_data(
#'   gm_read_sectors(gm_example_data("setores.gpkg")),
#'   gm_read_attributes(gm_example_data("variaveis.rds"))
#' )
#'
#' grid <- gm_make_grid(base, type = "metric", cellsize = 1000)
#'
#' gm_plot_overlay(
#'   base = base,
#'   overlay = grid,
#'   titulo = "Grade regular sobre setores"
#' )
#' }
#'
#' @export
gm_plot_overlay <- function(base,
                            overlay,
                            titulo = NULL,
                            base_fill = "grey90",
                            base_col = "white",
                            base_alpha = 0.7,
                            overlay_fill = NA,
                            overlay_col = "red",
                            overlay_alpha = 0.7,
                            overlay_lwd = 2) {

  if (!inherits(base, "sf")) {
    stop("O objeto 'base' deve ser um objeto sf.")
  }

  if (!inherits(overlay, "sf")) {
    stop("O objeto 'overlay' deve ser um objeto sf.")
  }

  mapa <- tmap::tm_shape(base) +
    tmap::tm_polygons(
      fill = base_fill,
      col = base_col,
      fill_alpha = base_alpha
    )

  geom_type <- unique(as.character(sf::st_geometry_type(overlay)))

  if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
    mapa <- mapa +
      tmap::tm_shape(overlay) +
      tmap::tm_lines(
        col = overlay_col,
        lwd = overlay_lwd,
        col_alpha = overlay_alpha
      )

  } else if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    mapa <- mapa +
      tmap::tm_shape(overlay) +
      tmap::tm_polygons(
        fill = overlay_fill,
        col = overlay_col,
        fill_alpha = overlay_alpha
      )

  } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
    mapa <- mapa +
      tmap::tm_shape(overlay) +
      tmap::tm_dots(
        col = overlay_col,
        size = 0.05,
        fill_alpha = overlay_alpha
      )

  } else {
    stop("Tipo de geometria da camada overlay não suportado.")
  }

  mapa <- mapa +
    tmap::tm_layout(frame = FALSE)

  if (!is.null(titulo)) {
    mapa <- mapa + tmap::tm_title(titulo)
  }

  mapa
}
