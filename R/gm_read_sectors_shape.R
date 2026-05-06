#' Ler setores censitários (geometria)
#'
#' Lê a malha de setores censitários a partir de um arquivo local ou realiza
#' o download diretamente via pacote \code{geobr}.
#'
#' @param path Caminho para um arquivo local contendo os setores, por exemplo
#'   \code{.gpkg} ou \code{.shp}. Se informado, a função lê o arquivo local.
#'   Caso \code{code} e \code{filter_var} também sejam informados, aplica um
#'   filtro sobre os dados lidos.
#'
#' @param code Código territorial utilizado para download ou filtragem. Pode
#'   assumir diferentes formatos:
#'   \itemize{
#'     \item Código de município com 7 dígitos, por exemplo \code{3305000};
#'     \item Código de estado com 2 dígitos, por exemplo \code{33};
#'     \item Sigla da unidade da federação, por exemplo \code{"RJ"};
#'     \item \code{"all"}, para retornar todos os setores disponíveis;
#'     \item Outros códigos territoriais numéricos presentes na base, como
#'     distrito, subdistrito ou bairro. Nesse caso, informe também
#'     \code{filter_var}.
#'   }
#'
#' @param filter_var Nome da variável utilizada para filtrar os dados. Exemplos:
#'   \code{"code_muni"}, \code{"code_district"}, \code{"code_subdistrict"} ou
#'   \code{"code_neighborhood"}. Quando \code{path} é informado, o filtro é
#'   aplicado sobre o arquivo local. Quando \code{path} não é informado e
#'   \code{code} não corresponde a município, estado ou \code{"all"}, a função
#'   baixa os setores da UF correspondente e filtra pela variável indicada.
#'
#' @param year Ano da malha censitária. Valores comuns incluem \code{2000},
#'   \code{2010} e \code{2022}. O padrão é \code{2022}. Este parâmetro é usado
#'   apenas quando o download é realizado via \code{geobr}.
#'
#' @param state Necessária caso esteja filtrando por \code{filter_var}
#'   e \code{code} do tipo caracter. Essa parâmtro será ignorado
#'   em todos os demais casos.
#'
#' @param simplified Se \code{TRUE}, retorna uma versão simplificada da malha
#'   ao utilizar o download via \code{geobr}. Este parâmetro é usado apenas
#'   quando o download é realizado via \code{geobr}.
#'
#' @return Objeto \code{sf} contendo a geometria dos setores censitários.
#'
#' @details
#' A função pode operar de duas formas. Se \code{path} for informado, os dados
#' são lidos de um arquivo local. Se \code{code} e \code{filter_var} também
#' forem informados, a função aplica um filtro sobre essa base local.
#'
#' Se \code{path} não for informado, os dados são baixados via
#' \code{geobr::read_census_tract()}. Códigos de município, códigos de estado,
#' siglas de UF e \code{"all"} são enviados diretamente ao \code{geobr}. Para
#' outros códigos territoriais numéricos, a função baixa os setores da UF
#' correspondente, identificada pelos dois primeiros dígitos do código, e aplica
#' o filtro indicado em \code{filter_var}.
#'
#' Esta função retorna apenas a geometria dos setores. Para obter os dados
#' tabulares associados aos setores, utilize \code{\link{gm_read_tracts_data}}.
#'
#' @examples
#' \dontrun{
#' # Ler a partir de arquivo local
#' setores <- gm_read_sectors_shape("dados/setores.gpkg")
#'
#' # Ler e filtrar município a partir de base local
#' setores <- gm_read_sectors_shape(
#'   path = "dados/setores.gpkg",
#'   code = 3305000,
#'   filter_var = "code_muni"
#' )
#'
#' # Baixar via geobr: município
#' setores <- gm_read_sectors_shape(code = 3305000)
#'
#' # Baixar via geobr: estado
#' setores <- gm_read_sectors_shape(code = "RJ")
#'
#' # Baixar via geobr: Brasil
#' setores <- gm_read_sectors_shape(code = "all")
#'
#' # Baixar UF e filtrar por outro território
#' setores <- gm_read_sectors_shape(
#'   code = 3301009002,
#'   filter_var = "code_neighborhood")
#'
#' setores <- gm_read_sectors_shape(
#' code = "Parque Avenida Pelinca",
#' filter_var = "name_neighborhood",
#' state = "RJ")
#' }
#'
#' @export
gm_read_sectors_shape <- function(path = NULL,
                            code = NULL,
                            filter_var = NULL,
                            state = NULL,
                            year = 2022,
                            simplified = FALSE) {

  code_info <- gm_resolve_census_code(
    code = code,
    filter_var = filter_var,
    state = state
  )

  if (!is.null(path)) {

    if (!file.exists(path)) {
      stop("Arquivo não encontrado: ", path)
    }

    setores <- sf::st_read(path, quiet = TRUE)

    # Se leu arquivo local e code foi informado, define filtro local
    if (!is.null(code) && as.character(code) != "all") {

      if (is.null(filter_var)) {
        filter_var <- dplyr::case_when(
          grepl("^[0-9]{2}$", as.character(code)) ~ "code_state",
          grepl("^[0-9]{7}$", as.character(code)) ~ "code_muni",
          grepl("^[A-Z]{2}$", as.character(code)) ~ "abbrev_state",
          TRUE ~ NA_character_
        )
      }

      if (is.na(filter_var)) {
        stop("Para esse tipo de filtro em base local, informe 'filter_var'.")
      }

      setores <- gm_filter_by_code(
        setores,
        filter_var = filter_var,
        filter_code = code
      )
    }

  } else if (!is.null(code)) {

    setores <- geobr::read_census_tract(
      code_tract = code_info$download_code,
      year = year,
      simplified = simplified
    )

    setores <- gm_filter_by_code(
      setores,
      filter_var = filter_var,
      filter_code = code_info$filter_code
    )

  } else {
    stop("Informe 'path' ou 'code'.")
  }

  setores
}
