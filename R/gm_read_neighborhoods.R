#' Ler bairros ou vizinhanças
#'
#' Lê uma camada de bairros/vizinhanças a partir de um arquivo local ou realiza
#' o download via pacote \code{geobr}, quando disponível.
#'
#' @param path Caminho para um arquivo local contendo bairros ou vizinhanças
#'   (por exemplo, \code{.gpkg} ou \code{.shp}). Se informado, tem prioridade
#'   sobre \code{code_muni}.
#' @param code_muni Código IBGE do município. Se informado, a função tenta
#'   baixar os bairros via \code{geobr}.
#' @param year Ano da base. Por padrão, \code{2010}.
#'
#' @return Objeto \code{sf} contendo a geometria dos bairros/vizinhanças.
#'
#' @details
#' Nem todos os municípios possuem bairros disponíveis no \code{geobr}.
#' Para uso em aula, recomenda-se utilizar um arquivo local previamente
#' preparado quando a base de bairros não estiver disponível.
#'
#' @examples
#' \dontrun{
#' # Ler a partir de arquivo local
#' bairros <- gm_read_neighborhoods("dados/bairros.gpkg")
#'
#' # Tentar baixar via geobr
#' bairros <- gm_read_neighborhoods(code_muni = 3301009)
#' }
#'
#' @export
gm_read_neighborhoods <- function(path = NULL,
                                  code_muni = NULL,
                                  year = 2010) {

  if (!is.null(path)) {

    if (!file.exists(path)) {
      stop("Arquivo não encontrado: ", path)
    }

    bairros <- sf::st_read(path, quiet = TRUE)
    return(bairros)
  }

  if (!is.null(code_muni)) {

    bairros <- geobr::read_neighborhood(
      code_muni = code_muni,
      year = year,
      simplified = FALSE
    )

    return(bairros)
  }

  stop("Informe 'path' ou 'code_muni'.")
}
