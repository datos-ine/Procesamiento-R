#### Cálculo de AVADs para DM en Argentina - años 2005, 2009, 2013 y 2018 AVAD =
#### AVP (años de vida perdidos por muerte prematura) + AVD (años vividos con
#### discapacidad) AVP = cantidad de muertes por DM por grupo de edad *
#### esperanza de vida según grupo de edad
#### Autora: Micaela Gauto
#### Colaboradora: Tamara Ricardo
#### Última modificación
# Fri May  9 10:05:05 2025 ------------------------------


# Carga de paquetes -------------------------------------------------------
library(janitor)
library(tidyverse)
#library(readxl)
# library(readr)


# Carga/Limpieza de datos -------------------------------------------------
## Muertes por DM: Serie 2005-2022 por jurisdicción, sexo y grupo de edad (DEIS)
serie_def <- read_csv("Bases de datos/clean/serie_def_dm.csv") |> 
  # Variables caracter a factor
  mutate(across(.cols = where(is.character), 
                .fns = ~ factor(.x)))

## Base de esperanza de vida (tabla del WHO-GHE)
esp_vida <- read_csv2("Bases de datos/arg2019_espvida.csv") |> 
  # Estandarizar nombres de variables
  rename(grupo_edad = GRUPEDAD) |> 
  
  # Filtrar grupos etarios
  filter(!grepl("<1|1-4|5-9|10-14", grupo_edad)) |> 
  
  # Etiquetas grupos etarios
  mutate(grupo_edad = factor(grupo_edad,
                             labels = levels(serie_def$grupo_edad))) |> 
  
  # Descartas columnas innecesarias
  select(-INDICADOR, -`Ambos sexos`) |> 
  
  # Formato long
  pivot_longer(cols = c("Masculino", "Femenino"),
               names_to = "sexo",
               values_to = "esp_vida")

## ENFR 2005-2018
enfr_dm <- read_csv("Bases de datos/clean/ENFR_dm.csv")

## Secuelas DM
dw_edad <- read_csv("Bases de datos/clean/dw_edad.csv")

 
### Explorar datos
tabyl(esp_vida$grupo_edad)


# Cálculo AVP a partir de las defunciones ---------------------------------
AVP_serie <- serie_def |> 
  # Añade año encuesta
  mutate(anio_enfr = case_when(
    anio %in% c(2005:2006) ~ 2005,
    anio %in% c(2008:2010) ~ 2009,
    anio %in% c(2012:2014) ~ 2013,
    TRUE ~ 2018
  )) |> 
  
  # Calcula muertes
  group_by(prov_res_cat, sexo, grupo_edad, anio_enfr) |> 
  summarise(mean_defun = mean(defun_dm, na.rm = T)) |> 
  ungroup() |> 
  
  # Une con datos esperanza de vida
  inner_join(esp_vida) |> 
  
  # Calcular AVP
  mutate(AVP = mean_defun * esp_vida)
 

### A RESOLVER:
# - Promedio muertes en trienio alrededor de ENFR? Para ENFR 2005 no tengo datos de falle 2004.
# - Prevalencia de DM para cálculo de AVAD inicia en 18 años, para falle tengo
# grupos quinquenales


### Cálculo de AVD -------------------------------------------------------------
## AVD = suma[prev de secuela de DM * peso de la secuela]

# ## Cálculo de AVD ----
# 
# # Unión de tabla con prevalencia de DM y tabla con frec y pesos de secuelas
# prev_DM_pesos <- prev_DM %>% 
#   left_join(DW, by = join_by(GRUPEDAD == GRUPEDAD)) #base se multiplica x 4 porque hay 4 secuelas para cada grupo
# 
# # Primero calculo AVD aportado por cada secuela
# prev_DM_pesos <- prev_DM_pesos %>% 
#   mutate(AVD_indiv = #para obtener cantidad de años vividos multiplico cantidad de personas con DM, frecuencia de secuela y DW
#            SI * `Frecuencia (Wandurranga)` / 100 * `Disability Weight`) 
# 
# # Sumo AVD de secuelas para obtener AVD total por grupo de edad
# AVD_serie <- prev_DM_pesos %>% 
#   group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, prop_DM) %>% #agrupo por estas dos variables para que me aparezcan ambas en la tabla (de todos modos para cada grupo de edad hay 1 prop_DM)
#   summarise(AVD = sum(AVD_indiv)) #tiene que tener misma cantidad de filas que tabla con prev_DM
# 
# # Grupos teóricos = 2688
# # Obtengo base de AVD con 2688 filas, todos los grupos posibles con dato
# 
# 
# ## Prueba de compatibilidad de bases AVP / AVD previa al macheo ----
# 
# # Transformo variables a factor y reordeno columnas en cada base
# AVP_serie <- AVP_serie %>% 
#   mutate(PROVRES = as.factor(PROVRES),
#          PROV_DESC = as.factor(PROV_DESC),
#          AÑO = as.factor(AÑO),
#          SEXO = as.factor(SEXO),
#          SEXO_DESC = as.factor(case_when(
#            SEXO_DESC == "Masculino" ~ "Varones",
#            SEXO_DESC == "Femenino" ~ "Mujeres")),
#          GRUPEDAD = as.factor(GRUPEDAD)) %>% 
#   select(AÑO, PROVRES, PROV_DESC, SEXO, SEXO_DESC, GRUPEDAD, AVP)
# # AVP = 2159 filas
# 
# AVD_serie <- AVD_serie %>% 
#   mutate(AÑO = as.factor(AÑO),
#          PROV_DESC = as.factor(PROV_DESC),
#          SEXO_DESC = as.factor(SEXO_DESC),
#          GRUPEDAD = as.factor(GRUPEDAD))
# # AVD = 2688 filas
# 
# # Base de AVD tiene más filas que AVP: no todos los grupos cuentan con fallecidos para calcular AVP
# # Agrupo y macheo para ver qué categoría no tiene dato de AVP
# 
# prueba_AVP <- AVP_serie %>% 
#   group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD) %>% 
#   summarise(n_AVP=n())
# 
# prueba_AVD <- AVD_serie %>% 
#   group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD) %>% 
#   summarise(n_AVD=n())
# 
# prueba <- prueba_AVD %>% 
#   left_join(prueba_AVP, by = join_by(AÑO == AÑO,
#                                      PROV_DESC == PROV_DESC,
#                                      SEXO_DESC == SEXO_DESC,
#                                      GRUPEDAD == GRUPEDAD)) %>% 
#   filter(is.na(n_AVP)) 
# 
# # Hay 529 categorías que no tienen AVP, es decir, no cuentan con fallecidos por DM, no se puede calcular AVP
# 
# rm(list=setdiff(ls(), c("espvida", "serie_def", "AVP_serie", "DM_serie", "AVD_serie")))
# 
# 
# ### Cálculo de AVAD ------------------------------------------------------------
# ## AVAD = AVP + AVD
# 
# AVAD_serie <- AVD_serie %>% #uso de base la serie de AVD porque es la más completa
#   left_join(AVP_serie, 
#             by = join_by(AÑO == AÑO, 
#                          PROV_DESC == PROV_DESC, 
#                          SEXO_DESC == SEXO_DESC,
#                          GRUPEDAD == GRUPEDAD)) %>%
#     mutate(
#       AVP = case_when(
#         is.na(AVP) ~ 0, #asigno valor 0 en filas que no tienen dato de AVP
#         .default = AVP),
#       AVAD = (AVD + AVP)) %>% 
#   select(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, prop_DM, AVD, AVP, AVAD) #reordeno columnas
# 
# 
# ### Aporte de AVP y de AVD sobre AVAD totales por grupo de edad, sexo y año ----
# 
# prop_AVAD <- AVAD_serie %>% 
#   group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD) %>% 
#   summarise(AVP_acum = sum(AVP),
#             AVD_acum = sum(AVD),
#             AVAD_acum = sum(AVAD)) %>% 
#   mutate(
#     prop_AVP = round((AVP_acum / AVAD_acum * 100), 2),
#     prop_AVD = round((AVD_acum / AVAD_acum * 100), 2)) %>% 
#   pivot_longer(cols = c(prop_AVP, prop_AVD), names_to = "INDIC", values_to = "PROP") %>% 
#   select(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, INDIC, PROP)
# 
# 
# ### Tasas de AVAD --------------------------------------------------------------
# 
# ## Tasas brutas y específicas por grupo de edad, sexo y provincia ----
# 
# # No tengo proyecciones 2009, censo 1991/2001 tiene proyecciones cada 5 años, se usa 2010.
# 
# # Carga de población por año, sexo y jurisdicción
# pobl_05 <- read_excel("Bases de datos/Proyecciones INDEC/proyec_2005.xlsx") #2005 (de proyecciones 2001-2015)
# pobl_10_18 <- read_excel("Bases de datos/Proyecciones INDEC/proyec_2010_2018.xlsx") #2010, 2013, 2018 (de proyecciones 2010-2040)
# 
# pobl_05 <- pobl_05 %>% 
#   mutate(AÑO = 2005) %>% 
#   rename("Ambos sexos" = "AMBOS SEXOS",
#          "Varones" = "VARONES", "Mujeres" = "MUJERES") %>% 
#   select(GRUPEDAD, `Ambos sexos`, Varones, Mujeres, PROV_DESC, AÑO)
# 
# serie_pobl <- rbind(pobl_05, pobl_10_18)
# 
# # Armonización de variable "GRUPEDAD"
# serie_pobl <- serie_pobl %>% 
#   mutate(GRUPEDAD = 
#            as.factor(
#              case_when(
#                GRUPEDAD == "0-4" ~ "01_1 a 4", #equiparo 0-4 de poblaciones con 1-4
#                GRUPEDAD == "5-9" ~ "02_5 a 9",
#                GRUPEDAD == "10-14" ~ "03_10 a 14",
#                GRUPEDAD == "15-19" ~ "04_15 a 19",
#                GRUPEDAD == "20-24" ~ "05_20 a 24",
#                GRUPEDAD == "25-29" ~ "06_25 a 29",
#                GRUPEDAD == "30-34" ~ "07_30 a 34",
#                GRUPEDAD == "35-39" ~ "08_35 a 39",
#                GRUPEDAD == "40-44" ~ "09_40 a 44",
#                GRUPEDAD == "45-49" ~ "10_45 a 49",
#                GRUPEDAD == "50-54" ~ "11_50 a 54",
#                GRUPEDAD == "55-59" ~ "12_55 a 59",
#                GRUPEDAD == "60-64" ~ "13_60 a 64",
#                GRUPEDAD == "65-69" ~ "14_65 a 69",
#                GRUPEDAD == "70-74" ~ "15_70 a 74",
#                GRUPEDAD == "75-79" ~ "16_75 a 79",
#                GRUPEDAD == "80-84" | GRUPEDAD == "85-89" | GRUPEDAD == "90-94" | 
#                  GRUPEDAD == "95-99" | GRUPEDAD == "100 y más" ~ "17_80 y más",
#                GRUPEDAD == "80ymas" ~ "17_80 y más",
#                .default = GRUPEDAD))
#              )
# 
# # Armado de base de población para unir con series de AVAD
# pobl_join <- serie_pobl %>%
#   select(-`Ambos sexos`) %>% #excluyo recuento total, asumo que si sumo sexos obtengo total (que no hay otra categoría)
#   filter(grepl("04_|05_|06_|07_|08_|09_|10_|11_|12_|13_|14_|15_|16_|17_", GRUPEDAD)) %>% #excluyo filas de grupos de edad no necesarios
#   pivot_longer(cols = c(Mujeres, Varones),
#                names_to = "SEXO_DESC", values_to = "RECUENTO_pobl") %>% 
#   group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD) %>% 
#   summarise(RECUENTO_pobl = sum(RECUENTO_pobl)) %>% 
#   mutate(AÑO = as.factor(AÑO))
# 
# 
# ## Cálculo de tasas brutas de AVAD (generales y específicas)
# tasas_esp_AVAD <- AVAD_serie %>% 
#   select(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, AVP, AVD, AVAD) %>% 
#   mutate(AÑO_join = as.factor(case_when(
#     AÑO == "2009" ~ "2010", #para prueba de cálculo de tasas, ver si se puede conseguir pobl de 2009
#     .default = AÑO
#   ))) %>% 
#   left_join(pobl_join, by = join_by(AÑO_join == AÑO, PROV_DESC == PROV_DESC, SEXO_DESC == SEXO_DESC,
#                                       GRUPEDAD == GRUPEDAD)) %>% 
#   select(-AÑO_join)
# 
# tasas_esp_AVAD <- tasas_esp_AVAD %>% 
#   mutate(tasa_AVP = round((AVP / RECUENTO_pobl * 100000),2),
#          tasa_AVD = round((AVD / RECUENTO_pobl * 100000),2),
#          tasa_AVAD = round((AVAD / RECUENTO_pobl * 100000),2))
# 
# tasas_gral_AVAD <- tasas_esp_AVAD %>% 
#   group_by(AÑO, PROV_DESC, SEXO_DESC) %>% 
#   summarise(pobl_tot = sum(RECUENTO_pobl),
#             AVP_sum = sum(AVP),
#             AVD_sum = sum(AVD),
#             AVAD_sum = sum(AVAD),
#             AVP_gral = AVP_sum/pobl_tot*100000,
#             AVD_gral = AVD_sum/pobl_tot*100000,
#             AVAD_gral = AVAD_sum/pobl_tot*100000) %>% 
#   select(AÑO, PROV_DESC, SEXO_DESC, AVP_gral, AVD_gral, AVAD_gral)
# 
# tasas_nac <- tasas_esp_AVAD %>% 
#   group_by(AÑO, SEXO_DESC, GRUPEDAD) %>% 
#   summarise(pobl_nac = sum(RECUENTO_pobl),
#             AVP_nac = sum(AVP),
#             AVD_nac = sum(AVD),
#             AVAD_nac = sum(AVAD),
#             tasa_AVP_nac = AVP_nac/pobl_nac*100000,
#             tasa_AVD_nac = AVD_nac/pobl_nac*100000,
#             tasa_AVAD_nac = AVAD_nac/pobl_nac*100000) %>% 
#   select(AÑO, SEXO_DESC, GRUPEDAD, AVP_nac, AVD_nac, AVAD_nac, tasa_AVP_nac, tasa_AVD_nac, tasa_AVAD_nac)
#   
#   
# rm(list=setdiff(ls(), c("AVP_serie", "DM_serie", "AVD_serie", "AVAD_serie", "prop_AVAD",
#                         "tasas_esp_AVAD", "tasas_gral_AVAD", "tasas_nac", "pobl_join")))
# 
# 
# ## Tasas ajustadas de AVAD ----
# 
# # Recorte de población de referencia: población total Argentina 2010, por sexo (último dato censal en el período de interés)
# pobl_ref <- pobl_join %>% 
#   filter(AÑO == 2010)
# 
# # Unión a base con tasas específicas para cálculo de tasas ajustadas (método directo)
# base_aj <- tasas_esp_AVAD %>% 
#   select(AÑO, GRUPEDAD, PROV_DESC, SEXO_DESC, tasa_AVP, tasa_AVD, tasa_AVAD) %>% 
#   left_join(pobl_ref, by = join_by(GRUPEDAD == GRUPEDAD, PROV_DESC == PROV_DESC,
#                                    SEXO_DESC == SEXO_DESC)) %>% 
#   select(-AÑO.y) %>% 
#   rename(AÑO = AÑO.x)
# 
# # Cálculo de tasas ajustadas
# tasas_aj_AVAD <- base_aj %>% 
#   mutate(AVP_ref = tasa_AVP*RECUENTO_pobl/100000,
#          AVD_ref = tasa_AVD*RECUENTO_pobl/100000,
#          AVAD_ref = tasa_AVAD*RECUENTO_pobl/100000) %>% 
#   group_by(AÑO, PROV_DESC, SEXO_DESC) %>% 
#   summarise(pobl_ref = sum(RECUENTO_pobl),
#             AVP_sum = sum(AVP_ref),
#             AVD_sum = sum(AVD_ref),
#             AVAD_sum = sum(AVAD_ref),
#             AVP_aj = AVP_sum/pobl_ref*100000,
#             AVD_aj = AVD_sum/pobl_ref*100000,
#             AVAD_aj = AVAD_sum/pobl_ref*100000) %>% 
#   select(AÑO, PROV_DESC, SEXO_DESC, AVP_aj, AVD_aj, AVAD_aj)
# 
# # Tasas ajustadas nacionales
# tasas_aj_nac <- tasas_nac %>% 
#   select(AÑO, SEXO_DESC, GRUPEDAD, tasa_AVP_nac, tasa_AVD_nac, tasa_AVAD_nac) %>% 
#   left_join((pobl_ref %>% 
#                group_by(AÑO, SEXO_DESC, GRUPEDAD) %>% 
#                summarise(pobl_nac = sum(RECUENTO_pobl))), 
#             by = join_by(SEXO_DESC == SEXO_DESC, GRUPEDAD == GRUPEDAD)) %>% 
#   select(-AÑO.y) %>% 
#   rename(AÑO = AÑO.x)
# 
# tasas_aj_nac <- tasas_aj_nac %>% 
#   mutate(AVP_ref = tasa_AVP_nac*pobl_nac/100000,
#          AVD_ref = tasa_AVD_nac*pobl_nac/100000,
#          AVAD_ref = tasa_AVAD_nac*pobl_nac/100000) %>% 
#   group_by(AÑO, SEXO_DESC) %>% 
#   summarise(pobl_ref = sum(pobl_nac),
#             AVP_sum = sum(AVP_ref),
#             AVD_sum = sum(AVD_ref),
#             AVAD_sum = sum(AVAD_ref),
#             AVP_aj = AVP_sum/pobl_ref*100000,
#             AVD_aj = AVD_sum/pobl_ref*100000,
#             AVAD_aj = AVAD_sum/pobl_ref*100000) %>% 
#   select(AÑO, SEXO_DESC, AVP_sum, AVD_sum, AVAD_sum, AVP_aj, AVD_aj, AVAD_aj)
# 
# 
# ## Guardo archivos como RData ----
# save(AVAD_serie, #cantidad de AVAD por año, provincia, sexo y grupo de edad
#      file = "AVAD_serie.RData")
# 
# save(prop_AVAD, #proporción de AVP y AVD sobre AVAD por año, provincia, sexo y grupo de edad
#      file = "prop_AVAD.RData")
# 
# save(tasas_esp_AVAD, #tasas específicas según grupo de edad de AVAD, AVP y AVD por año, provincia y sexo
#      file = "tasas_eso_AVAD.RData")
# 
# save(tasas_gral_AVAD, #tasas brutas generales de AVAD, AVP y AVD por año, provincia y sexo
#      file = "tasas_gral_AVAD.RData")
# 
# save(tasas_aj_AVAD, #tasas ajustadas de AVAD, AVP y AVD por año, provincia y sexo
#      file = "tasas_aj_AVAD.RData")
# 
# save(tasas_nac, #recuento de AVAD, AVP y AVD y sus tasas específicas según grupo de edad, por año y sexo para el total país
#      file = "tasas_nac.RData") 
# 
# save(tasas_aj_nac, #tasas ajustadas de AVAD, AVP y AVD por año y sexo para el total país
#      file = "tasas_aj_nac.RData")

