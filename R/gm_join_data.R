#' Integrar setores censitários e tabela de atributos
#'
#' Realiza a junção entre um objeto espacial (\code{sf}) contendo a geometria
#' dos setores censitários e uma tabela de atributos (por exemplo, dados do
#' Censo). A função utiliza \code{left_join}, preservando todas as geometrias.
#'
#'#' @param dados Tabela (data.frame ou tibble) contendo atributos a serem
#'   associados aos setores.
#' @param setores Objeto \code{sf} com a geometria dos setores censitários.
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
gm_join_data <- function(dados, setores,
                         id_setor_geom = "code_tract",
                         id_setor_tab  = "code_tract") {

  # remover colunas duplicadas de dados (exceto chave)
  cols_to_drop <- intersect(
    names(dados),
    names(setores)
  )

  cols_to_drop <- setdiff(cols_to_drop, id_setor_tab)

  if(!is.null(cols_to_drop)){
    dados <- dplyr::select(dados, -dplyr::all_of(cols_to_drop))}

  dplyr::left_join(
    setores,
    dados,
    by = stats::setNames(id_setor_tab, id_setor_geom)
  )
}
