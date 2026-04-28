#' Ler setores censitários
#'
#' Lê a malha de setores censitários a partir de um arquivo local ou realiza
#' o download diretamente via pacote \code{geobr}.
#'
#' @param path Caminho para um arquivo local contendo os setores (por exemplo,
#'   \code{.gpkg} ou \code{.shp}). Se informado, tem prioridade sobre
#'   \code{code_muni}.
#' @param code_muni Código IBGE do município. Se informado, os setores são
#'   baixados automaticamente.
#' @param year Ano da malha censitária (por padrão, \code{2022}).
#'
#' @return Objeto \code{sf} contendo a geometria dos setores censitários.
#'
#' @details
#' Se \code{path} for informado, os dados são lidos a partir do arquivo local.
#' Caso contrário, se \code{code_muni} for informado, os dados são baixados via
#' \code{geobr::read_census_tract}.
#'
#' @examples
#' \dontrun{
#' # Ler a partir de arquivo local
#' setores <- gm_read_sectors("dados/setores.gpkg")
#'
#' # Baixar via geobr
#' setores <- gm_read_sectors(code_muni = 3301009)
#' }
#'
#' @export
gm_read_sectors <- function(path = NULL, code_muni = NULL, year = 2022) {

  if (!is.null(path)) {

    if (!file.exists(path)) {
      stop("Arquivo não encontrado: ", path)
    }

    setores <- sf::st_read(path, quiet = TRUE)
    return(setores)
  }

  if (!is.null(code_muni)) {

    setores <- geobr::read_census_tract(
      code_tract = code_muni,
      year = year,
      simplified = FALSE
    )

    return(setores)
  }

  stop("Informe 'path' ou 'code_muni'.")
}
