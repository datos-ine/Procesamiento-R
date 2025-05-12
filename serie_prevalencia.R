### Cálculo de prevalencia de DM a partir de los datos publicados de las ENFR,
### considerando la prevalencia de DM o glucemia elevada por autorreporte.
### Autora: Micaela Gauto
### Colaboradora: Tamara Ricardo
### Fecha modificación:
# Mon May 12 11:49:22 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(epikit)
library(srvyr)
library(tidyverse)


# Cargar datos crudos -----------------------------------------------------
## ENFR 2005
datos_05 <- read_delim("Bases de datos/ENFR_bases/ENFR 2005 - Base usuario.txt") |> 
  # Seleccionar columnas relevantes
  select(id = IDENTIFI,
         prov_res = PROV,
         sexo = CHCH04,
         edad = CHCH05,
         dm_auto = CIDI01,
         ponderacion = PONDERACION)

## ENFR 2009
datos_09 <- read_delim("Bases de datos/ENFR_bases/ENFR 2009 - Base usuario.txt") |> 
  # Seleccionar columnas relevantes
  select(id = IDENTIFI, 
         prov_res = PRVNC,
         sexo = BHCH04,
         edad = BHCH05,
         dm_auto = BIDI01,
         ponderacion = PONDERACION)


## ENFR 2013
datos_13 <- read_delim("Bases de datos/ENFR_bases/ENFR 2013 - Base usuario.txt") |> 
  # Seleccionar columnas relevantes
  select(id = ID,
         prov_res = COD_PROVINCIA,
         sexo = BHCH04,
         edad = BHCH05,
         dm_auto = BIDI01,
         ponderacion = PONDERACION)

## ENFR 2018
datos_18 <- read_delim("Bases de datos/ENFR_bases/ENFR 2018 - Base usuario.txt") |> 
  # Seleccionar columnas relevantes
  select(id,
         prov_res = cod_provincia,
         sexo = bhch03,
         edad = bhch04,
         dm_auto = bidi01,
         wf1p) 

## Réplicas ENFR 2018
replicas_18 <- read_delim("Bases de datos/ENFR_bases/ENFR2018_base_replicas_filtrada.csv") |> 
  # Seleccionar columnas relevantes
  select(id, starts_with("wf1p"))


# Procesamiento de datos -------------------------------------------------- #
# Para la creación de los grupos etarios revisar y justificar conceptualmente, ya
# que por las características de la enfermedad no hay prácticamente casos en los
# grupos más jóvenes y el algoritmo no converge, pero tampoco es apropiado
# calcular a partir de la prevalencia muestral. 

## Cálculo de prevalencia ENFR2005-----
prev_05 <- datos_05 |> 
  # Crear grupo etario (20-80 años cada 5 años)
  filter(edad >= 20) |> 
  mutate(grupo_edad = age_categories(
    edad,
    lower = 20,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), .after = edad) |> 
  
  # Crear objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Calcular prevalencia
  group_by(prov_res, sexo, grupo_edad, dm_auto) |> 
  
  summarise(prev_dm_auto = survey_prop(vartype = "cv", na.rm = TRUE)) |> 
  
  ungroup() |> 
  
  filter(dm_auto == 1)


## Cálculo de prevalencia ENFR2009-----
prev_09 <- datos_09 |> 
  # Crear grupo etario (20-80 años cada 5 años)
  filter(edad >= 20) |> 
  mutate(grupo_edad = age_categories(
    edad,
    lower = 20,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), .after = edad) |> 
  
  # Crear objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Calcular prevalencia
  group_by(prov_res, sexo, grupo_edad, dm_auto) |> 
  
  summarise(prev_dm_auto = survey_prop(vartype = "cv", na.rm = TRUE)) |>
  
  ungroup() |> 
  
  filter(dm_auto == 1)


## Cálculo de prevalencia ENFR2013 (no converge)-----
prev_13 <- datos_13 |> 
  # Crear grupo etario (20-80 años cada 5 años)
  filter(edad >= 20) |> 
  mutate(grupo_edad = age_categories(
    edad,
    lower = 20,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), .after = edad) |> 
  
  # Crear objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Calcular prevalencia
  group_by(prov_res, sexo, grupo_edad, dm_auto) |> 
  
  summarise(prev_dm_auto = survey_prop(vartype = "cv", na.rm = TRUE)) |>
  
  ungroup() |> 
  
  filter(dm_auto == 1) |> 
  
  # Crear columna para año encuesta
  mutate(anio = 2013)


## Cálculo de prevalencia ENFR2018 (da error)-----
prev_18 <- datos_18 |> 
  # Crear grupo etario (20-80 años cada 5 años)
  filter(edad >= 20) |> 
  mutate(grupo_edad = age_categories(
    edad,
    lower = 20,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), .after = edad) |> 
  
  # Unir con base réplicas
  inner_join(replicas_18) |> 
  
  # Crear objeto de diseño
  as_survey_rep(weights = wf1p, 
                repweights = starts_with("wf1p"),
                type = "bootstrap", 
                mse = T) |> 
  
  # Calcular prevalencia
  group_by(prov_res, sexo, grupo_edad, dm_auto) |> 
  
  summarise(prev_dm_auto = survey_prop(vartype = "cv", na.rm = TRUE)) |>
  
  ungroup() |> 
  
  filter(dm_auto == 1) |> 
  
  # Crear columna para año encuesta
  mutate(anio = 2018)


# Unir bases de prevalencia -----------------------------------------------
prev_join <- prev_05 |> 
  bind_rows(prev_09, .id = "anio") |> 
  
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
  mutate(sexo = if_else(sexo == 1, "Masculino", "Femenino"))

