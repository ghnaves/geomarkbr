geomarkbr
================

Pacote em desenvolvimento com funções básicas para análise espacial de
dados do Censo brasileiro, com foco em ensino de geomarketing.

## O que o pacote faz

- Ler setores censitários
- Integrar atributos do Censo
- Criar mapas temáticos simples
- Calcular indicadores demográficos
- Gerar grades regulares para análise espacial

------------------------------------------------------------------------

## Instalação

Instalação local a partir do arquivo .tar.gz:

    install.packages("geomarkbr_0.0.0.9000.tar.gz", repos = NULL, type = "source")

Durante desenvolvimento:

    devtools::load_all()

------------------------------------------------------------------------

## Dados de exemplo

O pacote inclui dados para uso em aula:

- setores.gpkg
- variaveis.csv
- variaveis.rds

Para acessar:

    library(geomarkbr)
    list.files(system.file("extdata", package = "geomarkbr"))

------------------------------------------------------------------------

## Exemplo básico

    library(geomarkbr)
    library(dplyr)

    setores_path <- gm_example_data("setores.gpkg")
    variaveis_path <- gm_example_data("variaveis.rds")

    setores <- gm_read_sectors(setores_path)
    dados   <- gm_read_attributes(variaveis_path)

    base <- gm_join_data(setores, dados)

    gm_plot_basic(base, pop_abs, "População por setor")

------------------------------------------------------------------------

## Indicadores demográficos

    cols_idade <- c(
      "pop_abs_00_04",
      "pop_abs_05_09",
      "pop_abs_10_14",
      "pop_abs_15_19",
      "pop_abs_20_24",
      "pop_abs_25_29",
      "pop_abs_30_39",
      "pop_abs_40_49",
      "pop_abs_50_59",
      "pop_abs_60_69",
      "pop_abs_70_mais"
    )

    base <- base |>
      dplyr::mutate(
        idade_media   = gm_media_idade(base, colunas = cols_idade),
        idade_mediana = gm_mediana_idade(base, colunas = cols_idade)
      )

------------------------------------------------------------------------

## Criação de grade regular

    library(sf)

    grid <- gm_make_grid(base, cellsize = 1000)

    plot(sf::st_geometry(grid), border = "grey50")
    plot(sf::st_geometry(base), add = TRUE)

------------------------------------------------------------------------

## Estrutura do pacote

    R/              -> funções
    inst/extdata/   -> dados de exemplo
    inst/scripts/   -> scripts de aula
    sandbox/        -> testes e experimentos

------------------------------------------------------------------------

## Uso em aula

O pacote foi estruturado para apoiar três etapas:

1.  leitura e integração de dados espaciais\
2.  construção de mapas temáticos\
3.  preparação para análise de localização (geomarketing)

Scripts disponíveis:

    system.file("scripts", package = "geomarkbr")

------------------------------------------------------------------------

## Observações

- Os dados de idade são agregados em faixas, portanto as medidas são
  aproximadas\
- Estratégias de tratamento de NA podem ser controladas nas funções\
- O pacote está em desenvolvimento

------------------------------------------------------------------------

## Autor

Gustavo Givisiez\
Universidade Federal Fluminense
