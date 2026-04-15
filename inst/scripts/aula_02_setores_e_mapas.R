library(geomarkbr)
library(dplyr)

setores <- gm_read_sectors(gm_example_data("setores.gpkg"))
dados <- gm_read_attributes(gm_example_data("variaveis.csv"))

base <- gm_join_data(setores, dados)

gm_plot_basic(base, pop_abs, "População por setor")
gm_plot_basic(base, perc_mulheres, "Percentual de mulheres")
