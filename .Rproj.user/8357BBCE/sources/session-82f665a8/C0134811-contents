#' Mapa básico de setores
#'
#' @param x objeto sf
#' @param var nome da variável
#' @param titulo título do mapa
#' @return ggplot
#' @export
gm_plot_basic <- function(x, var, titulo = NULL) {
  var <- rlang::ensym(var)

  ggplot2::ggplot(x) +
    ggplot2::geom_sf(ggplot2::aes(fill = !!var), color = NA) +
    ggplot2::scale_fill_viridis_c(na.value = "grey90") +
    ggplot2::labs(
      title = titulo %||% rlang::as_string(var),
      fill = rlang::as_string(var)
    ) +
    ggplot2::theme_minimal()
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
