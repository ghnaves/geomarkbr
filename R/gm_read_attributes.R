#' Ler atributos de setores censitários
#'
#' Lê uma tabela de atributos associada aos setores censitários a partir de um
#' arquivo local. Suporta arquivos nos formatos \code{.csv} e \code{.rds}.
#'
#' @param path Caminho para o arquivo local contendo os dados.
#'
#' @return Um objeto do tipo \code{data.frame} ou \code{tibble}.
#'
#' @details
#' Arquivos no formato \code{.csv} são lidos com \code{readr::read_csv}, enquanto
#' arquivos \code{.rds} são lidos com \code{readRDS}. O uso de \code{.rds} é
#' recomendado quando se deseja preservar os tipos originais das variáveis.
#'
#' @examples
#' \dontrun{
#' dados_csv <- gm_read_attributes("dados/variaveis.csv")
#' dados_rds <- gm_read_attributes(gm_example_data("variaveis.rds"))
#' }
#'
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

#' Ler setores censitários do Censo
#'
#' Realiza o download dos dados de setores censitários diretamente do IBGE
#' utilizando o pacote \code{censobr}.
#'
#' @param dataset Nome da base do Censo a ser utilizada (por exemplo,
#'   \code{"Basico"}).
#' @param year Ano do Censo (por exemplo, \code{2022}).
#' @param as_data_frame Se \code{TRUE}, retorna um \code{data.frame}. Caso
#'   contrário, retorna um objeto \code{sf}.
#'
#' @return Um objeto \code{data.frame} ou \code{sf}, dependendo do argumento
#'   \code{as_data_frame}.
#'
#' @details
#' A função utiliza \code{censobr::read_tracts}, que realiza o download dos dados
#' diretamente dos servidores do IBGE. O tempo de execução pode variar conforme
#' o tamanho da base e a conexão com a internet.
#'
#' @examples
#' \dontrun{
#' setores <- gm_read_tracts_censo(year = 2022, dataset = "Basico")
#' }
#'
#' @export
gm_read_tracts_censo <- function(dataset = "Basico", year = 2022, as_data_frame = TRUE) {
  censobr::read_tracts(
    year = year,
    dataset = dataset,
    as_data_frame = as_data_frame,
    showProgress = FALSE
  )
}
