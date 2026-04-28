geomarkbr
================

Pacote em desenvolvimento com funções básicas para análise espacial de
dados para o contexto brasileiro, com foco em ensino de geomarketing.

## O que o pacote faz

- Ler setores censitários
- Integrar atributos do Censo
- Criar mapas temáticos simples
- Calcular indicadores demográficos
- Gerar grades regulares para análise espacial

------------------------------------------------------------------------

## Instalação

Instalação local a partir do arquivo .tar.gz:

    install.packages("geomarkbr_0.0.0.9001.tar.gz", repos = NULL, type = "source")

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

## Criação de grade regular

    base_utm <- sf::st_transform(base, 31984)
    
    gm_plot_basic(base_utm, pop_abs, "População por setor")
    
    gm_plot_overlay(
      base = base_utm,
      overlay = grid_utm,
      titulo = "Grade regular sobre setores censitários"
    )
    
    gm_plot_overlay(
      base = base,
      overlay = grid_utm,
      base_fill = "lightblue",
      base_col = 'blue',
      overlay_col = "green",
      overlay_fill = 'lightgreen',
      overlay_alpha = 0.3
    )

------------------------------------------------------------------------

## Baixar vias e ciclovias pelo osmdata

    vias <- sf::st_read(gm_example_data("vias_principais.gpkg"), quiet = TRUE)
    ciclovias <- gm_read_example_cycleways("cycleway")
    ciclofaixas <- gm_read_example_cycleways("cycleway_tag")

    gm_plot_overlay(base, vias, titulo = "Vias principais") +
      tmap::tm_shape(ciclovias) +
      tmap::tm_lines(col = "blue", lwd = 2) +
      tmap::tm_shape(ciclofaixas) +
      tmap::tm_lines(col = "green", lwd = 2)
      
    gm_plot_overlay(base, vias, titulo = "Vias principais")
    
    gm_plot_overlay(
      base = base,
      overlay = grid_utm,
      titulo = "Grade sobre setores",
      overlay_col = "black",
      overlay_alpha = 0.4
    )
    
    gm_plot_overlay(
      base = base,
      overlay = ciclovias,
      titulo = "Ciclovias",
      overlay_col = "blue",
      overlay_lwd = 2
    )
    
------------------------------------------------------------------------

## Mapas interativos


    tmap::tmap_mode("view")
    
    gm_plot_overlay(base, vias, titulo = "Vias principais", overlay_lwd = 1) +
      tmap::tm_shape(ciclovias) +
      tmap::tm_lines(col = "blue", lwd = 2) +
      tmap::tm_shape(ciclofaixas) +
      tmap::tm_lines(col = "green", lwd = 2)
      
    
    # Para salvar html (ótimo para mostrar para clientes)
    mapa_interativo = gm_plot_overlay(base, vias, titulo = "Vias principais", overlay_lwd = 1) +
      tmap::tm_shape(ciclovias) +
      tmap::tm_lines(col = "blue", lwd = 2) +
      tmap::tm_shape(ciclofaixas) +
      tmap::tm_lines(col = "green", lwd = 2)
      
    tmap::tmap_save(mapa_interativo, "mapa_interativo.html")
    
    # para voltar para o mapa estático
    tmap::tmap_mode("plot") 
    
------------------------------------------------------------------------

## Estrutura do pacote

    R/              -> funções
    inst/extdata/   -> dados de exemplo
    inst/scripts/   -> scripts de aula

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
