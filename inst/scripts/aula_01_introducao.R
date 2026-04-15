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
