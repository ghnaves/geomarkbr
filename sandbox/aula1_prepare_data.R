library(censobr)
library(dplyr)

### ---- Census tracts 2022 ----
datasets_censobr_2022 = c("Basico", "Domicilio", "ResponsavelRenda",
                          "Pessoas", "Indigenas", "Quilombolas",
                          "Entorno", "Obitos", "Preliminares")

data_dictionary(2022,'tracts')


variaveis <- read_tracts(2022,'Basico', as_data_frame = TRUE)
variaveis = tibble::as_tibble(variaveis)
setores <- sf::read_sf('~/Documents/Bases_Mapas/RJ_setores_CD2022.gpkg')
names(setores)=c(names(variaveis),'geom')
setores = setores |>
  dplyr::filter(code_muni == '3301009' & name_district == "Campos dos Goytacazes")
sf::write_sf(setores, 'inst/extdata/setores.gpkg')

### ---- Total de Moradores ----
pessoas <- read_tracts(2022,'Pessoas', as_data_frame = TRUE)

pessoas = pessoas |>
  dplyr::filter(code_muni == '3301009' & name_district == "Campos dos Goytacazes")

pessoas = tibble::as_tibble(pessoas)

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

mediana <- gm_mediana_idade(variaveis_raw, colunas = cols_idade, na_strategy = "zero")
media = gm_media_idade(variaveis_raw, colunas = cols_idade, na_strategy = "ignore")

variaveis_raw = pessoas |>
  dplyr::select(code_tract,
                pop_abs = demografia_V01006,
                pop_mulheres = demografia_V01008,
                pop_homens = demografia_V01007,
                homnes_00_04 = demografia_V01009,
                homens_05_09 = demografia_V01010,
                homens_10_14 = demografia_V01011,
                homens_15_19 = demografia_V01012,
                homens_20_24 = demografia_V01013,
                homens_25_29 = demografia_V01014,
                homens_30_39 = demografia_V01015,
                homens_40_49 = demografia_V01016,
                homens_50_59 = demografia_V01017,
                homens_60_69 = demografia_V01018,
                homens_70_mais = demografia_V01019,
                pop_abs_00_04 = demografia_V01031,
                pop_abs_05_09 = demografia_V01032,
                pop_abs_10_14 = demografia_V01033,
                pop_abs_15_19 = demografia_V01034,
                pop_abs_20_24 = demografia_V01035,
                pop_abs_25_29 = demografia_V01036,
                pop_abs_30_39 = demografia_V01037,
                pop_abs_40_49 = demografia_V01038,
                pop_abs_50_59 = demografia_V01039,
                pop_abs_60_69 = demografia_V01040,
                pop_abs_70_mais = demografia_V01041)

variaveis = variaveis_raw %>%
  dplyr::mutate(perc_mulheres = pop_mulheres/pop_abs,
         perc_homens = pop_homens/pop_abs,
         idade_mediana = gm_mediana_idade(variaveis_raw, na_strategy = "zero"),
         idade_media   = gm_media_idade(variaveis_raw, na_strategy = "zero")) %>%
  dplyr::select(code_tract, pop_abs,
                pop_mulheres, perc_mulheres,
                pop_homens, perc_homens,
                idade_mediana, idade_media)

readr::write_csv(variaveis, 'inst/extdata/variaveis.csv')
readr::write_rds(variaveis, "inst/extdata/variaveis.rds")




