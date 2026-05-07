#' Ler setores de exemplo de Campos
#'
#' Carrega a shape dos setores censitários incluídos no pacote para uso em aula.
#' Os dados são do Censo 2022 para Campos dos Gotytacazes
#'
#'
#' @return Objeto \code{sf} contendo linhas representando os setores censitários
#'
#' @examples
#' \dontrun{
#' setores <- gm_read_example_sectors()
#'
#' plot(sf::st_geometry(setores), col = "blue")
#'
#' }
#' @export
gm_read_example_sectors <- function() {
  file = 'setores.gpkg'

  path <- system.file("extdata", file, package = "geomarkbr")
  if (path == "") {
    stop("Arquivo de exemplo não encontrado no pacote.")
  }

  sf::st_read(
    system.file("extdata", file, package = "geomarkbr"),
    quiet = TRUE
  )
}
