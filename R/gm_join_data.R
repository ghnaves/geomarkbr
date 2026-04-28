#' Integrar setores censitários e tabela de atributos
#'
#' Realiza a junção entre um objeto espacial (\code{sf}) contendo a geometria
#' dos setores censitários e uma tabela de atributos (por exemplo, dados do
#' Censo). A função utiliza \code{left_join}, preservando todas as geometrias.
#'
#' @param setores Objeto \code{sf} com a geometria dos setores censitários.
#' @param dados Tabela (data.frame ou tibble) contendo atributos a serem
#'   associados aos setores.
#' @param id_setor_geom Nome da coluna de identificação no objeto espacial.
#' @param id_setor_tab Nome da coluna de identificação na tabela de atributos.
#'
#' @return Objeto \code{sf} com os atributos integrados.
#'
#' @details
#' A função mantém todas as geometrias do objeto \code{setores}, mesmo que não
#' haja correspondência na tabela de atributos. Valores ausentes são preenchidos
#' com \code{NA}.
#'
#' @examples
#' \dontrun{
#' setores <- gm_read_sectors(gm_example_data("setores.gpkg"))
#' dados   <- gm_read_attributes(gm_example_data("variaveis.rds"))
#'
#' base <- gm_join_data(setores, dados)
#' }
#'
#' @export
gm_join_data <- function(setores, dados,
                         id_setor_geom = "code_tract",
                         id_setor_tab  = "code_tract") {

  dplyr::left_join(
    setores,
    dados,
    by = stats::setNames(id_setor_tab, id_setor_geom)
  )
}
