#' Mapa básico de setores
#'
#' Cria um mapa temático simples a partir de um objeto sf.
#'
#' @param x objeto sf.
#' @param var nome da variável a ser mapeada.
#' @param titulo título do mapa. Se NULL, usa o nome da variável.
#' @param legenda título da legenda. Se NULL, usa o nome da variável.
#'
#' @return objeto tmap.
#' @export
gm_plot_basic <- function(x, var, titulo = NULL, legenda = NULL) {

  var <- rlang::ensym(var)
  var_name <- rlang::as_string(var)

  if (is.null(titulo)) {
    titulo <- var_name
  }

  if (is.null(legenda)) {
    legenda <- var_name
  }

  tmap::tm_shape(x) +
    tmap::tm_polygons(
      fill = var_name,
      fill.scale = tmap::tm_scale(values = "viridis"),
      fill.legend = tmap::tm_legend(title = legenda),
      col = NA
    ) +
    tmap::tm_title(titulo) +
    tmap::tm_layout(
      legend.outside = TRUE,
      frame = FALSE
    )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
