library(geomarkbr)
library(dplyr)
library(sf)

setores <- gm_read_sectors(gm_example_data("setores.gpkg"))
dados <- gm_read_attributes(gm_example_data("variaveis.csv"))

base <- gm_join_data(setores, dados)

grid <- gm_make_grid(base, cellsize = 1000)

plot(sf::st_geometry(grid), border = "grey50")
plot(sf::st_geometry(base), add = TRUE)
