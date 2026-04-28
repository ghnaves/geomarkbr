#' Mapa com duas camadas espaciais
#'
#' Cria um mapa simples com uma camada de base e uma camada sobreposta.
#'
#' @param base objeto sf da camada de base, por exemplo setores censitários.
#' @param overlay objeto sf da camada sobreposta, por exemplo uma grade regular.
#' @param titulo título do mapa.
#' @param base_fill cor de preenchimento da camada de base.
#' @param base_col cor da borda da camada de base.
#' @param overlay_fill cor de preenchimento da camada sobreposta.
#' @param overlay_col cor da borda da camada sobreposta.
#' @param overlay_alpha transparência da camada sobreposta.
#' @param overlay_lwd espessura da camada sobreposta
#'
#' @return objeto tmap.
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

  mapa <- tmap::tm_shape(base) +
    tmap::tm_polygons(
      fill = base_fill,
      col = base_col,
      fill_alpha = base_alpha,
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
