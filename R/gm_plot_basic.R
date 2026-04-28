#' Criar mapa temático simples
#'
#' Cria um mapa temático a partir de um objeto \code{sf}, utilizando uma variável
#' numérica para o preenchimento dos polígonos.
#'
#' @param x Objeto \code{sf}.
#' @param var Nome da variável a ser mapeada (sem aspas).
#' @param titulo Título do mapa. Se \code{NULL}, usa o nome da variável.
#' @param legenda Título da legenda. Se \code{NULL}, usa o nome da variável.
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
#' gm_plot_basic(base, pop_abs)
#' }
#'
#' @export
gm_plot_basic <- function(x, var, titulo = NULL, legenda = NULL) {

  if (!inherits(x, "sf")) {
    stop("O objeto 'x' deve ser um objeto sf.")
  }

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
