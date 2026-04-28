#' Obter caminho de arquivos de exemplo do pacote
#'
#' Retorna o caminho para os dados de exemplo incluídos no pacote, localizados
#' na pasta \code{inst/extdata}. Esses dados são utilizados nas aulas e exemplos
#' do pacote.
#'
#' @param file Nome do arquivo dentro de \code{extdata}. Se \code{NULL}, retorna
#'   o caminho da pasta \code{extdata}.
#'
#' @return Um vetor de caracteres com o caminho completo para o arquivo ou
#'   diretório solicitado.
#'
#' @examples
#' \dontrun{
#' # Listar arquivos disponíveis
#' list.files(gm_example_data())
#'
#' # Obter caminho de um arquivo específico
#' setores_path <- gm_example_data("setores.gpkg")
#' }
#'
#' @export
gm_example_data <- function(file = NULL) {
  path <- system.file("extdata", package = "geomarkbr")

  if (is.null(file)) return(path)

  file.path(path, file)
}
