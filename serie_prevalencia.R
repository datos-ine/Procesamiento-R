### Cálculo de prevalencia de DM a partir de los datos publicados de las ENFR,
### considerando la prevalencia de DM o glucemia elevada por autorreporte.
### Autora: Micaela Gauto
### Colaboradora: Tamara Ricardo
### Fecha modificación:
# Fri May  9 13:47:20 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(epikit)
library(srvyr)
library(tidyverse)


# Cargar/limpiar datos ----------------------------------------------------
## ENFR 2005
datos_05 <- read_delim("Bases de datos/ENFR_bases/ENFR 2005 - Base usuario.txt") |> 
  # Seleccionar columnas relevantes
  select(prov_res = PROV,
         sexo = CHCH04,
         edad = CHCH05,
         dm_auto = CIDI01,
         ponderacion = PONDERACION) |> 
  
  # Crear columna para año encuesta
  mutate(anio = 2005) |> 
  
  # Crear grupo etario
  mutate(grupo_edad = age_categories(
    edad,
    lower = 15,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), .after = edad)


## ENFR 2009
datos_09 <- read_delim("Bases de datos/ENFR_bases/ENFR 2009 - Base usuario.txt") |> 
  # Seleccionar columnas relevantes
  select(prov_res = PRVNC,
         sexo = BHCH04,
         edad = BHCH05,
         dm_auto = BIDI01,
         ponderacion = PONDERACION) |> 
  
  # Crear columna para año encuesta
  mutate(anio = 2009) |> 
  
  # Crear grupo etario
  mutate(grupo_edad = age_categories(
    edad,
    lower = 15,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), .after = edad)


## ENFR 2013
datos_13 <- read_delim("Bases de datos/ENFR_bases/ENFR 2013 - Base usuario.txt") |> 
  
  # Seleccionar columnas relevantes
  select(prov_res = COD_PROVINCIA,
         sexo = BHCH04,
         edad = BHCH05,
         dm_auto = BIDI01,
         ponderacion = PONDERACION) |> 
  
  # Crear columna para año encuesta
  mutate(anio = 2013) |> 
  
  # Crear grupo etario
  mutate(grupo_edad = age_categories(
    edad,
    lower = 15,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), .after = edad)


## ENFR 2018
datos_18 <- read_delim("Bases de datos/ENFR_bases/ENFR 2018 - Base usuario.txt") |> 
  # Seleccionar columnas relevantes
  select(prov_res = cod_provincia,
         sexo = bhch03,
         edad = bhch04,
         dm_auto = bidi01,
         ponderacion = wf1p) |> 
  
  # Crear columna para año encuesta
  mutate(anio = 2018) |> 
  
  # Crear grupo etario
  mutate(grupo_edad = age_categories(
    edad,
    lower = 15,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), .after = edad)
  

# Prevalencia DM ----------------------------------------------------------
## ENFR 2005
prev_05 <- datos_05 |> 
  as_survey(weights = ponderacion) |> 
  group_by(anio, prov_res, grupo_edad, sexo, dm_auto) |> 
  summarise(survey_prop(na.rm = TRUE))

## ENFR 2009

## ENFR 2013

## ENFR 2018


# Unir datos --------------------------------------------------------------
datos_join <- datos_raw |> 
  
  # Modifica etiquetas provincia
  mutate(prov_res_cat = factor(prov_res,
                            labels = c("CABA", 
                                       "Buenos Aires", 
                                       "Catamarca",
                                       "Córdoba",
                                       "Corrientes",
                                       "Chaco",
                                       "Chubut",
                                       "Entre Ríos",
                                       "Formosa",
                                       "Jujuy",
                                       "La Pampa",
                                       "La Rioja",
                                       "Mendoza",
                                       "Misiones",
                                       "Neuquén",
                                       "Río Negro",
                                       "Salta",
                                       "San Juan",
                                       "San Luis",
                                       "Santa Cruz",
                                       "Santa Fe",
                                       "Santiago del Estero",
                                       "Tucumán",
                                       "Tierra del Fuego")),
         .after = prov_res) |> 
  
  # Modifica etiquetas sexo
  mutate(sexo = if_else(sexo == 1, "Masculino", "Femenino")) |> 
  
  # Modifica etiquetas DM autorreporte
  mutate(dm_auto = case_when(
    dm_auto == 1 ~ "Si",
    dm_auto == 2 ~ "No",
    dm_auto %in% c(9, 99) ~ "NS/NC",
    TRUE ~ NA
  )) |> 
  
  # Descartar columnas innecesarias
  select(anio, prov_res, prov_res_cat, grupo_edad, sexo, dm_auto, ponderacion) |> 
  
  # Ordenar datos
  arrange(anio, prov_res, grupo_edad, sexo)

  
# ## Prevalencia de DM por año, jurisdicción y grupo de edad ----
# 
# # Primero agrupo por año, provincia, sexo, grupo de edad y condición de DM,
# # luego calculo prevalencia de DM en cada grupo
# 
# prev_DM <- DM_serie %>% 
#   group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, DM_auto) %>% 
#   summarise(recuento = sum(PONDERACION)) %>% 
#   pivot_wider(names_from = DM_auto, values_from = recuento, values_fill = 0) %>% 
#   mutate(prop_DM = round(SI/(SI+NO+`NS/NC`)*100, digits = 2),
#                    total_DM = SI+NO+`NS/NC`) %>%
#   select(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, SI, total_DM, prop_DM)
# 
# # Grupos teóricos = 2688; 
# # Obtengo base de prevalencia de DM con 2688 filas, todos los grupos posibles con dato
# 

# Guardar datos limpios ---------------------------------------------------
write_csv(datos, file = "Bases de datos/clean/ENFR_dm.csv")

