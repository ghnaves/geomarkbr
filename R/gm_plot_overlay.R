#' Criar mapa com camadas espaciais sobrepostas
#'
#' Cria um mapa simples com uma camada de base e uma ou mais camadas
#' sobrepostas. A função identifica automaticamente se cada camada sobreposta
#' é formada por pontos, linhas ou polígonos.
#'
#' @param base Objeto \code{sf} da camada de base, por exemplo setores censitários.
#' @param overlay Objeto \code{sf} ou lista de objetos \code{sf} a serem sobrepostos à
#'   camada de base, por exemplo vias, ciclovias ou uma grade regular.
#' @param titulo Título do mapa. Se \code{NULL}, o mapa é criado sem título.
#' @param base_fill Cor de preenchimento da camada de base, quando ela for
#'   formada por polígonos.
#' @param base_col Cor da camada de base ou da borda, quando for polígono.
#' @param base_alpha Transparência da camada de base.
#' @param base_lwd Espessura da camada de base, quando ela for formada por
#'   linhas.
#' @param overlay_fill Cor de preenchimento das camadas sobrepostas, usada
#'   quando a camada for formada por polígonos.
#' @param overlay_col Cor das camadas sobrepostas.
#' @param overlay_alpha Transparência das camadas sobrepostas.
#' @param overlay_lwd Espessura das camadas sobrepostas, usada quando a camada
#'   for formada por linhas.
#' @param overlay_group Nome das camadas sobrepostas no modo interativo. Se
#'   \code{NULL}, usa nomes automáticos.
#'
#' @return Objeto \code{tmap}.
#'
#' @examples
#' \dontrun{
#' base <- gm_join_data(
#'   gm_read_sectors(gm_example_data("setores.gpkg")),
#'   gm_read_attributes(gm_example_data("variaveis.rds"))
#' )
#'
#' grid <- gm_make_grid(base, type = "metric", cellsize = 1000)
#' vias <- gm_read_example_roads()
#'
#' gm_plot_overlay(
#'   base = base,
#'   overlay = list(grid, vias),
#'   titulo = "Grade e vias sobre setores",
#'   overlay_col = c("black", "red"),
#'   overlay_lwd = c(1, 2),
#'   overlay_group = c("Grade", "Vias")
#' )
#' }
#'
#' @export
gm_plot_overlay <- function(base,
                            overlay,
                            titulo = NULL,
                            base_fill = "grey90",
                            base_col = "grey70",
                            base_alpha = 0.5,
                            base_lwd = 1,
                            overlay_fill = NA,
                            overlay_col = "red",
                            overlay_alpha = 0.7,
                            overlay_lwd = 1,
                            overlay_group = NULL) {

  if (!inherits(base, "sf")) {
    stop("O objeto 'base' deve ser um objeto sf.")
  }

  if (inherits(overlay, "sf")) {
    overlay <- list(overlay)
  }

  if (!is.list(overlay)) {
    stop("O argumento 'overlay' deve ser um objeto sf ou uma lista de objetos sf.")
  }

  for (i in seq_along(overlay)) {
    if (!inherits(overlay[[i]], "sf")) {
      stop("Todos os elementos de 'overlay' devem ser objetos sf.")
    }
  }

  n_overlay <- length(overlay)

  rep_arg <- function(x, n) {
    if (length(x) == 1) rep(x, n) else x
  }

  overlay_fill  <- rep_arg(overlay_fill, n_overlay)
  overlay_col   <- rep_arg(overlay_col, n_overlay)
  overlay_alpha <- rep_arg(overlay_alpha, n_overlay)
  overlay_lwd   <- rep_arg(overlay_lwd, n_overlay)

  if (is.null(overlay_group)) {
    overlay_group <- paste0("Overlay ", seq_len(n_overlay))
  } else {
    overlay_group <- rep_arg(overlay_group, n_overlay)
  }

  add_layer <- function(mapa, shape, fill, col, alpha, lwd, group) {
    geom_type <- unique(as.character(sf::st_geometry_type(shape)))

    if (any(geom_type %in% c("LINESTRING", "MULTILINESTRING"))) {
      mapa +
        tmap::tm_shape(shape) +
        tmap::tm_lines(
          col = col,
          lwd = lwd,
          col_alpha = alpha,
          group = group
        )

    } else if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
      mapa +
        tmap::tm_shape(shape) +
        tmap::tm_polygons(
          fill = fill,
          col = col,
          fill_alpha = alpha,
          group = group
        )

    } else if (any(geom_type %in% c("POINT", "MULTIPOINT"))) {
      mapa +
        tmap::tm_shape(shape) +
        tmap::tm_dots(
          col = col,
          size = 0.05,
          fill_alpha = alpha,
          group = group
        )

    } else {
      stop("Tipo de geometria não suportado.")
    }
  }

  mapa <- tmap::tm_shape(base)

  mapa <- add_layer(
    mapa = mapa,
    shape = base,
    fill = base_fill,
    col = base_col,
    alpha = base_alpha,
    lwd = base_lwd,
    group = "Base"
  )

  for (i in seq_along(overlay)) {
    mapa <- add_layer(
      mapa = mapa,
      shape = overlay[[i]],
      fill = overlay_fill[i],
      col = overlay_col[i],
      alpha = overlay_alpha[i],
      lwd = overlay_lwd[i],
      group = overlay_group[i]
    )
  }

  mapa <- mapa +
    tmap::tm_layout(frame = FALSE)

  if (!is.null(titulo)) {
    mapa <- mapa + tmap::tm_title(titulo)
  }

  mapa
}
