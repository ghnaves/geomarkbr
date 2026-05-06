#' Ler dados dos setores censitários
#'
#' Baixa os dados tabulares dos setores censitários a partir do pacote
#' \code{censobr}, permitindo selecionar diferentes conjuntos de variáveis
#' disponíveis para cada ano do censo.
#'
#' @param code_muni Código IBGE do município.
#' @param dataset Nome do conjunto de dados a ser baixado. Os valores válidos
#'   dependem do ano selecionado.
#' @param year Ano do censo. Valores suportados: \code{2000}, \code{2010},
#'   \code{2022}.
#' @param as_data_frame Se \code{TRUE}, retorna um \code{data.frame}. Caso
#'   contrário, retorna um objeto com estrutura original do \code{censobr}.
#' @param showProgress Se \code{TRUE}, exibe barra de progresso durante o download.
#' @param cache Se \code{TRUE}, utiliza cache local para evitar downloads repetidos.
#' @param verbose Se \code{TRUE}, exibe mensagens informativas durante a execução.
#'
#' @return \code{data.frame} contendo os dados dos setores censitários.
#'
#' @details
#' Os conjuntos de dados disponíveis variam conforme o ano do censo:
#'
#' \strong{2000}: Basico, Domicilio, Responsavel, Pessoa, Instrucao, Morador.
#'
#' \strong{2010}: Basico, Domicilio, DomicilioRenda, Responsavel,
#' ResponsavelRenda, Pessoa, PessoaRenda, Entorno.
#'
#' \strong{2022}: Basico, Domicilio, ResponsavelRenda, Pessoas,
#' Indigenas, Quilombolas, Entorno, Obitos, Preliminares.
#'
#' Para combinar os dados com a geometria dos setores, utilize a função
#' \code{\link{gm_read_sectors}} seguida de \code{gm_join_data()}.
#'
#' @examples
#' \dontrun{
#' # Baixar dados básicos do censo 2022
#' dados <- gm_read_tracts_data(
#'   code_muni = 3301009,
#'   year = 2022,
#'   dataset = "Basico"
#' )
#' }
#'
#' @export
gm_read_sectors_data <- function(code = "all",
                                 path = NULL,
                                 dataset = "Basico",
                                 year = c(2022, 2010, 2000),
                                 filter_var = NULL,
                                 state = NULL,
                                 as_data_frame = TRUE,
                                 showProgress = TRUE,
                                 cache = TRUE,
                                 verbose = TRUE) {

  year <- match.arg(as.character(year), choices = c("2022", "2010", "2000"))
  year <- as.integer(year)

  datasets_por_ano <- list(
    `2000` = c("Basico", "Domicilio", "Responsavel", "Pessoa", "Instrucao", "Morador"),
    `2010` = c("Basico", "Domicilio", "DomicilioRenda", "Responsavel",
               "ResponsavelRenda", "Pessoa", "PessoaRenda", "Entorno"),
    `2022` = c("Basico", "Domicilio", "ResponsavelRenda", "Pessoas",
               "Indigenas", "Quilombolas", "Entorno", "Obitos", "Preliminares")
  )

  datasets_validos <- datasets_por_ano[[as.character(year)]]

  if (!dataset %in% datasets_validos) {
    stop(
      "Dataset inválido para o ano ", year, ".\n",
      "Use um destes: ", paste(datasets_validos, collapse = ", ")
    )
  }

  filter_info <- gm_resolve_data_filter(
    code = code,
    filter_var = filter_var,
    state = state
  )

  if (!is.null(path)) {

    if (!file.exists(path)) {
      stop("Arquivo não encontrado: ", path)
    }

    dados <- readr::read_rds(path)

  } else {

    dados <- censobr::read_tracts(
      year = year,
      dataset = dataset,
      as_data_frame = as_data_frame,
      showProgress = showProgress,
      cache = cache,
      verbose = verbose
    )
  }

  if (!is.null(filter_info$state_filter_var)) {
    dados <- gm_filter_by_code(
      x = dados,
      filter_var = filter_info$state_filter_var,
      filter_code = filter_info$state_filter_code
    )
  }

  if (filter_info$needs_filter) {
    dados <- gm_filter_by_code(
      x = dados,
      filter_var = filter_info$filter_var,
      filter_code = filter_info$filter_code
    )
  }

  dados
}
