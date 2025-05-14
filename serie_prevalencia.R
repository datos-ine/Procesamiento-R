### Cálculo de prevalencia de DM a partir de los datos publicados de las ENFR,
### considerando la prevalencia de DM o glucemia elevada por autorreporte.
### Autora: Micaela Gauto
### Colaboradora: Tamara Ricardo
### Fecha modificación:
# Wed May 14 11:39:21 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(epikit)
library(srvyr)
library(tidyverse)


# Cargar/limpiar datos ----------------------------------------------------
# Para la creación de los grupos etarios revisar y justificar conceptualmente, ya
# que por las características de la enfermedad no hay prácticamente casos en los
# grupos más jóvenes y el algoritmo no converge, pero tampoco es apropiado
# calcular a partir de la prevalencia muestral. 
# Etiquetas provincias
provincia <- read_csv("Bases de datos/cod_pcias_arg.csv")

## ENFR 2005
datos_05 <- read_delim("Bases de datos/ENFR_bases/ENFR 2005 - Base usuario.txt",
                       col_select = c(
                                      prov_res = PROV, 
                                      sexo = CHCH04, 
                                      edad = CHCH05, 
                                      rango_edad = RANGEDAD, 
                                      dm_auto = CIDI01, 
                                      ponderacion = PONDERACION)) |> 
  
  # Añadir etiquetas provincia
  left_join(provincia) |> 
  
  # Añadir etiquetas grupo edad
  mutate(grupo_edad_enfr = factor(rango_edad,
                                  labels = c("18 a 24",
                                             "25 a 34",
                                             "35 a 49",
                                             "50 a 64",
                                             "65+")), 
         .after = rango_edad)|> 
  
  # Crear grupos edad quinquenales
  mutate(grupo_edad_quin = age_categories(
    edad,
    lower = 15,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), 
    .after = grupo_edad_enfr) |> 
  
  # Cambiar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Varón",
                                  "Mujer"))) |> 
  
  # Cambiar etiquetas diabetes por autorreporte
  mutate(dm_auto = factor(dm_auto,
                          labels = c("Si",
                                     "No",
                                     "NS/NC"))) |> 
  
  # Variables caracter a factor
  mutate(across(.cols = where(is.character),
                .fns = ~ factor(.x)))

  
## ENFR 2009
datos_09 <- read_delim("Bases de datos/ENFR_bases/ENFR 2009 - Base usuario.txt",
                       col_select = c(id = IDENTIFI, 
                                      prov_res = PRVNC,
                                      sexo = BHCH04,
                                      edad = BHCH05,
                                      rango_edad = RANGEDAD,
                                      dm_auto = BIDI01,
                                      ponderacion = PONDERACION)) |> 
  
  # Añadir etiquetas provincia
  left_join(provincia) |> 
  
  # Añadir etiquetas grupo edad
  mutate(grupo_edad_enfr = factor(rango_edad,
                                  labels = c("18 a 24",
                                             "25 a 34",
                                             "35 a 49",
                                             "50 a 64",
                                             "65+")), 
         .after = rango_edad)|> 
  
  # Crear grupos edad quinquenales
  mutate(grupo_edad_quin = age_categories(
    edad,
    lower = 15,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), 
    .after = grupo_edad_enfr) |> 
  
  # Cambiar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Varón",
                                  "Mujer"))) |> 
  
  # Cambiar etiquetas diabetes por autorreporte
  mutate(dm_auto = factor(dm_auto,
                          labels = c("Si",
                                     "No",
                                     "NS/NC"))) |> 
  
  # Variables caracter a factor
  mutate(across(.cols = where(is.character),
                .fns = ~ factor(.x)))


## ENFR 2013
datos_13 <- read_delim("Bases de datos/ENFR_bases/ENFR 2013 - Base usuario.txt",
                       col_select = c(id = ID,
                                      prov_res = COD_PROVINCIA,
                                      sexo = BHCH04,
                                      edad = BHCH05,
                                      rango_edad = RANGO_EDAD,
                                      dm_auto = BIDI01,
                                      ponderacion = PONDERACION)) |> 
  
  # Añadir etiquetas provincia
  left_join(provincia) |> 
  
  # Añadir etiquetas grupo edad
  mutate(grupo_edad_enfr = factor(rango_edad,
                                  labels = c("18 a 24",
                                             "25 a 34",
                                             "35 a 49",
                                             "50 a 64",
                                             "65+")), 
         .after = rango_edad)|> 
  
  # Crear grupos edad quinquenales
  mutate(grupo_edad_quin = age_categories(
    edad,
    lower = 15,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), 
    .after = grupo_edad_enfr) |> 
  
  # Cambiar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Varón",
                                  "Mujer"))) |> 
  
  # Cambiar etiquetas diabetes por autorreporte
  mutate(dm_auto = factor(dm_auto,
                          labels = c("Si",
                                     "No",
                                     "NS/NC"))) |> 
  
  # Variables caracter a factor
  mutate(across(.cols = where(is.character),
                .fns = ~ factor(.x)))


## ENFR 2018
datos_18 <- read_delim("Bases de datos/ENFR_bases/ENFR 2018 - Base usuario.txt",
                       col_select = c(id,
                                      prov_res = cod_provincia,
                                      sexo = bhch03,
                                      edad = bhch04,
                                      rango_edad,
                                      dm_auto = bidi01,
                                      wf1p)) |> 
  
  # Añadir etiquetas provincia
  left_join(provincia) |> 
  
  # Añadir etiquetas grupo edad
  mutate(grupo_edad_enfr = factor(rango_edad,
                                  labels = c("18 a 24",
                                             "25 a 34",
                                             "35 a 49",
                                             "50 a 64",
                                             "65+")), 
         .after = rango_edad)|> 
  
  # Crear grupos edad quinquenales
  mutate(grupo_edad_quin = age_categories(
    edad,
    lower = 15,
    upper = 80,
    by = 5,
    separator = " a ",
    above.char = " y más"), 
    .after = grupo_edad_enfr) |> 
  
  # Cambiar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Varón",
                                  "Mujer"))) |> 
  
  # Cambiar etiquetas diabetes por autorreporte
  mutate(dm_auto = factor(dm_auto,
                          labels = c("Si",
                                     "No",
                                     "NS/NC"))) |> 
  
  # Variables caracter a factor
  mutate(across(.cols = where(is.character),
                .fns = ~ factor(.x))) |>  
  
  # Añadir base de réplicas
  inner_join(
    read_delim("Bases de datos/ENFR_bases/ENFR2018_base_replicas_filtrada.csv") |> 
      # Seleccionar columnas relevantes
      select(id, starts_with("wf1p"))  
  )


# Crear objetos de diseño -------------------------------------------------
# ENFR 2005
prev05 <- datos_05 |> 
  as_survey_design(weights = ponderacion)

# ENFR 2009
prev09 <- datos_09 |> 
  as_survey_design(weights = ponderacion)

# ENFR 2013
prev13 <- datos_13 |> 
  as_survey_design(weights = ponderacion)

# ENFR 2018
prev18 <- datos_18 |> 
  as_survey_rep(weights = wf1p, 
                repweights = starts_with("wf1p"),
                type = "bootstrap", 
                mse = T) 



# Explorar combinaciones faltantes ----------------------------------------
## Crear combinaciones posibles prov_nombre, grupo_edad_enfr y sexo
comb_enfr <- expand_grid(
  prov_nombre = levels(datos_05$prov_nombre),
  grupo_edad_enfr = levels(datos_05$grupo_edad_enfr),
  sexo = levels(datos_05$sexo),
  dm_auto = levels(datos_05$dm_auto)
  )|> 
  filter(dm_auto == "Si")

## Crear combinaciones posibles prov_nombre, grupo_edad_quin y sexo
comb_quin <- expand_grid(
  prov_nombre = levels(datos_05$prov_nombre),
  grupo_edad_quin = levels(datos_05$grupo_edad_quin),
  sexo = levels(datos_05$sexo),
  dm_auto = levels(datos_05$dm_auto)
  ) |> 
  filter(dm_auto == "Si")

## Combinaciones faltantes ENFR 2005
# grupos edad ENFR
comb_enfr |> 
  anti_join(datos_05)

# Grupos edad quinquenales
comb_quin |> 
  anti_join(datos_05)

## Combinaciones faltantes ENFR 2009
# grupos edad ENFR
comb_enfr |> 
  anti_join(datos_09)

# Grupos edad quinquenales
comb_quin |> 
  anti_join(datos_09)

## Combinaciones faltantes ENFR 2013
# grupos edad ENFR
comb_enfr |> 
  anti_join(datos_13)

# Grupos edad quinquenales
comb_quin |> 
  anti_join(datos_13)

## Combinaciones faltantes ENFR 2018
# grupos edad ENFR
comb_enfr |> 
  anti_join(datos_18)

# Grupos edad quinquenales
comb_quin |> 
  anti_join(datos_18)


# Calcular prevalencias ---------------------------------------------------
### ENFR 2005-----
## Grupo de edad ENFR (Error de convergencia)
prev05_enfr <- prev05 |> 
  # Calcular prevalencia
  group_by(prov_nombre, grupo_edad_enfr, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(na.rm = TRUE),
            prev_dm = survey_prop(na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")

## Grupo edad quinquenal (Error de convergencia)
prev05_quin <- prev05 |> 
  # Calcular prevalencia
  group_by(prov_nombre, grupo_edad_quin, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(na.rm = TRUE),
            prev_dm = survey_prop(na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")


### ENFR 2009-----
## Grupo de edad ENFR (Funciona bien)
prev09_enfr <- prev09 |> 
  # Calcular prevalencia
  group_by(prov_nombre, grupo_edad_enfr, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(na.rm = TRUE),
            prev_dm = survey_prop(na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")

## Grupo edad quinquenal (Funciona bien)
prev09_quin <- prev09 |> 
  # Calcular prevalencia
  group_by(prov_nombre, grupo_edad_quin, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(na.rm = TRUE),
            prev_dm = survey_prop(na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")


### ENFR 2013-----
## Grupo de edad ENFR (Funciona bien)
prev13_enfr <- prev13 |> 
  # Calcular prevalencia
  group_by(prov_nombre, grupo_edad_enfr, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(na.rm = TRUE),
            prev_dm = survey_prop(na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")

## Grupo edad quinquenal (Error de convergencia)
prev13_quin <- prev13 |> 
  # Calcular prevalencia
  group_by(prov_nombre, grupo_edad_quin, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(na.rm = TRUE),
            prev_dm = survey_prop(na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")


### ENFR 2018-----
## Grupo de edad ENFR (Funciona bien)
prev18_enfr <- prev18 |> 
  # Calcular prevalencia
  group_by(prov_nombre, grupo_edad_enfr, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(na.rm = TRUE),
            prev_dm = survey_prop(na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")

## Grupo edad quinquenal (Error de glm.fit)
prev18_quin <- prev18 |> 
  # Calcular prevalencia
  group_by(prov_nombre, grupo_edad_quin, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(na.rm = TRUE),
            prev_dm = survey_prop(na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")


# Unir bases prevalencia --------------------------------------------------
## Grupos edad quinquenales (la mayoría de las prevalencias falla, revisar)
# prev_join_quin <- prev05_quin |> # revisar x error de convergencia
#   bind_rows(prev09_quin,
#             prev13_quin, 
#             prev18_quin, 
#             .id = "anio_enfr") |> 
#   
#   # Reemplaza etiquetas año
#   mutate(anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018")))


## Grupos edad ENFR (faltan 20 combinaciones de provincia, edad y sexo)
prev_join_enfr <- prev05_enfr |> # revisar x error de convergencia
  bind_rows(prev09_enfr,
            prev13_enfr, 
            prev18_enfr, 
            .id = "anio_enfr") |> 
  
  # Reemplaza etiquetas año
  mutate(anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
         ) |> 
  
  # Descartar columnas innecesarias
  select(-dm_auto)

# CABA: falta prev. hombres y mujeres 18-24 años para 2013
# Catamarca: falta prev. hombres 18-24 años para 2009
# Chubut: falta prev. hombres 18-24 años para 2018
# Córdoba: falta prev. hombres 18-24 años para 2005
# Entre Ríos: falta prev. mujeres 18-24 años para 2005
# Formosa: falta prev. hombres 25-34 años para 2009 y 18-24 años para 2018
# Jujuy: falta prev. hombres y mujeres 18-24 años para 2005 y hombres 18-24 años para 2018
# Misiones: falta prev. hombres 18-24 años para 2013
# Salta: falta prev. hombres 18-24 años para 2009, 2013 y 2018
# Santiago del Estero: falta prev. hombres 18-24 años para 2018
# Tierra del Fuego: falta prev. hombres 18-24 años para 2013
# Tucumán: falta prev. hombres 18-24 años para 2009 y 2013 y 25-34 años para 2009
# Provincias restantes: datos completos para todas las combinaciones
 

# Guardar datos limpios ---------------------------------------------------
# write_csv(prev_join_quin, "Bases de datos/clean/prev_dm_quin.csv")

write_csv(prev_join_enfr, "Bases de datos/clean/prev_dm_enfr.csv")

# Limpiar environment
rm(list = ls())
