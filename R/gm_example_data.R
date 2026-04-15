#' Retorna o caminho de arquivos de exemplo do pacote
#' @param file nome do arquivo em extdata
#' @export
gm_example_data <- function(file = NULL) {
  path <- system.file("extdata", package = "geomarkbr")
  if (is.null(file)) return(path)
  file.path(path, file)
}
