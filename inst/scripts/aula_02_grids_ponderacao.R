# install.packages("<CAMINHO>/geomarkbr_0.0.0.9000.tar.gz", repos = NULL, type = "source")

library(geomarkbr)
library(dplyr)

setores_path <- gm_example_data("setores.gpkg")
variaveis_csv <- gm_example_data("variaveis.csv")
variaveis_rds <- gm_example_data("variaveis.rds")

setores <- gm_read_sectors(setores_path)
dados_csv <- gm_read_attributes(variaveis_csv)
dados_rds <- gm_read_attributes(variaveis_rds)

# base <- gm_join_data(setores, dados_csv)
base <- gm_join_data(setores, dados_rds)

names(base)
plot(sf::st_geometry(base))
gm_plot_basic(base, pop_abs, "População por setor")

# Para SIRGAS2000 latlong use `4674`
# Para SIRGAS 2000 / UTM zone 24S use o default (31984)

grid_latlon <- gm_make_grid(base, type = "latlon", cellsize = '30s', crs_out = 4674)
grid_utm <- gm_make_grid(base, type = "metric", cellsize = 100, crs_out = 31984)

# O tamanho da célula de saída é 1000 metros na grid_utm e aproximadamente 925m na grid_latlon
# Nós vamos trabalhar como o UTM (medidas métricas fazem mais sentido). Por isso, temos de reprojetar a base

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


