#' Juntar setores e atributos
#'
#' @param setores objeto sf com geometria
#' @param dados tabela de atributos
#' @param id_setor_geom nome da coluna ID no objeto espacial
#' @param id_setor_tab nome da coluna ID na tabela
#' @return objeto sf
#' @export
gm_join_data <- function(setores, dados,
                         id_setor_geom = "CD_SETOR",
                         id_setor_tab  = "CD_SETOR") {

  dplyr::left_join(
    setores,
    dados,
    by = stats::setNames(id_setor_tab, id_setor_geom)
  )
}
