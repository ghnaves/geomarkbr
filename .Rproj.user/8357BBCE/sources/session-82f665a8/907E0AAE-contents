#' Ler atributos de setores censitários
#'
#' @param path caminho para csv local.
#' @return data.frame
#' @export
gm_read_attributes <- function(path) {
  dados <- readr::read_csv(path, show_col_types = FALSE)
  dados
}

#' Ler dados de setores do Censo via censobr
#'
#' @param dataset base do censo
#' @param year ano do censo
#' @param as_data_frame retornar data.frame
#' @export
gm_read_tracts_censo <- function(dataset = "Basico", year = 2022, as_data_frame = TRUE) {
  censobr::read_tracts(
    year = year,
    dataset = dataset,
    as_data_frame = as_data_frame,
    showProgress = FALSE
  )
}
