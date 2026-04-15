#' Ler atributos de setores censitários
#'
#' @param path caminho para csv local.
#' @return data.frame
#' @export
gm_read_attributes <- function(path) {

  if (!file.exists(path)) {
    stop("Arquivo não encontrado: ", path)
  }

  ext <- tools::file_ext(path)

  if (ext == "csv") {
    dados <- readr::read_csv(path, show_col_types = FALSE)
  } else if (ext == "rds") {
    dados <- readRDS(path)
  } else {
    stop("Formato não suportado. Use .csv ou .rds")
  }

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
