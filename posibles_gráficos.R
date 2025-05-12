### Pruebas de gráficos --------------------------------------------------------

## Carga de paquetes
library(tidyverse)
library(readxl)
library(readr)

### Carga de bases RData

load("AVAD_serie.RData")#cantidad de AVAD por año, provincia, sexo y grupo de edad
load("prop_AVAD.RData") #proporción de AVP y AVD sobre AVAD por año, provincia, sexo y grupo de edad
load("tasas_eso_AVAD.RData") #tasas específicas según grupo de edad de AVAD, AVP y AVD por año, provincia y sexo
load("tasas_gral_AVAD.RData") #tasas brutas generales de AVAD, AVP y AVD por año, provincia y sexo
load("tasas_aj_AVAD.RData") #tasas ajustadas de AVAD, AVP y AVD por año, provincia y sexo
load("tasas_nac.RData") #recuento de AVAD, AVP y AVD y sus tasas específicas según grupo de edad, por año y sexo para el total país
load("tasas_aj_nac.RData") #tasas ajustadas de AVAD, AVP y AVD por año y sexo para el total país


## Nueva variable con grandes grupos de edad para graficar
tasas_esp_AVAD <- tasas_esp_AVAD %>% 
  mutate(GRUPEDAD_GR = case_when(
    GRUPEDAD == "04_15 a 19" | GRUPEDAD == "05_20 a 24" | GRUPEDAD == "06_25 a 29" | GRUPEDAD == "07_30 a 34" | GRUPEDAD == "08_35 a 39" ~ "01_Menor de 40",
    GRUPEDAD == "09_40 a 44" | GRUPEDAD == "10_45 a 49" | GRUPEDAD == "11_50 a 54" ~ "02_40 a 54",
    GRUPEDAD == "12_55 a 59" | GRUPEDAD == "13_60 a 64" | GRUPEDAD == "14_65 a 69" ~ "03_55 a 69",
    GRUPEDAD == "15_70 a 74" | GRUPEDAD == "16_75 a 79" | GRUPEDAD == "17_80 y más" ~ "04_70 y más",
    .default = GRUPEDAD
  ))

tasas_nac <- tasas_nac %>% 
  mutate(GRUPEDAD_GR = case_when(
    GRUPEDAD == "04_15 a 19" | GRUPEDAD == "05_20 a 24" | GRUPEDAD == "06_25 a 29" | GRUPEDAD == "07_30 a 34" | GRUPEDAD == "08_35 a 39" ~ "01_Menor de 40",
    GRUPEDAD == "09_40 a 44" | GRUPEDAD == "10_45 a 49" | GRUPEDAD == "11_50 a 54" ~ "02_40 a 54",
    GRUPEDAD == "12_55 a 59" | GRUPEDAD == "13_60 a 64" | GRUPEDAD == "14_65 a 69" ~ "03_55 a 69",
    GRUPEDAD == "15_70 a 74" | GRUPEDAD == "16_75 a 79" | GRUPEDAD == "17_80 y más" ~ "04_70 y más",
    .default = GRUPEDAD
  ))


### Gráficos de tendencia de tasas ----

## Tasas AVAD ----
g1 <- tasas_esp_AVAD %>% 
  filter(PROV_DESC == "Buenos Aires") %>% 
  ggplot(aes(x=AÑO, y=tasa_AVAD, color = GRUPEDAD, group = GRUPEDAD)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_grid(rows = vars(GRUPEDAD_GR), cols = vars(SEXO_DESC), scales = "free_y")
g1

g1_nac <- tasas_nac %>% 
  ggplot(aes(x=AÑO, y=tasa_AVAD_nac, color = GRUPEDAD, group = GRUPEDAD)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_grid(rows = vars(GRUPEDAD_GR), cols = vars(SEXO_DESC), scales = "free_y")
g1_nac


## Tasas AVP ----
g2 <- tasas_esp_AVAD %>% 
  filter(PROV_DESC == "Buenos Aires") %>% 
  ggplot(aes(x=AÑO, y=tasa_AVP, color = GRUPEDAD, group = GRUPEDAD)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_grid(rows = vars(GRUPEDAD_GR), cols = vars(SEXO_DESC), scales = "free_y")
g2

g2_nac <- tasas_nac %>% 
  ggplot(aes(x=AÑO, y=tasa_AVP_nac, color = GRUPEDAD, group = GRUPEDAD)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_grid(rows = vars(GRUPEDAD_GR), cols = vars(SEXO_DESC), scales = "free_y")
g2_nac


## Tasas AVD ----
g3 <- tasas_esp_AVAD %>% 
  filter(PROV_DESC == "Buenos Aires") %>% 
  ggplot(aes(x=AÑO, y=tasa_AVD, color = GRUPEDAD, group = GRUPEDAD)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_grid(rows = vars(GRUPEDAD_GR), cols = vars(SEXO_DESC), scales = "free_y")
g3

g3_nac <- tasas_nac %>% 
  ggplot(aes(x=AÑO, y=tasa_AVD_nac, color = GRUPEDAD, group = GRUPEDAD)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_grid(rows = vars(GRUPEDAD_GR), cols = vars(SEXO_DESC), scales = "free_y")
g3_nac


## Gráfico de aporte de cada componente al AVAD ----
g4 <- prop_AVAD %>% 
  filter(PROV_DESC == "Buenos Aires") %>% 
  ggplot(aes(x=AÑO, y=PROP, color = INDIC, group = INDIC)) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(GRUPEDAD), cols = vars(SEXO_DESC), scales = "free_y")
g4


## Pirámide ----
install.packages("apyramid")
apyramid::age_pyramid(data = demo_agg_long,
                      age_group = "age_cat5",# column name for age category
                      split_by = "gender",   # column name for gender
                      count = "counts")      # column name for case counts