### Cálculo de prevalencia de DM a partir de los datos publicados de las ENFR,
### considerando la prevalencia de DM o glucemia elevada por autorreporte y grupos
### de edad cada 10 años.
### Autora: Micaela Gauto
### Colaboradora: Tamara Ricardo
### Fecha modificación:
# Mon May 19 09:32:28 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(epikit)
library(srvyr)
library(tidyverse)


# Cargar datos ------------------------------------------------------------
## Etiquetas provincias INDEC
id_prov <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_edad <- read_csv("Bases de datos/grupos_edad.csv") |> 
  mutate_all(.funs = ~ factor(.x))


## ENFR 2005
datos05_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2005 - Base usuario.txt",
                       col_select = c(
                         prov_id = PROV, 
                         sexo = CHCH04, 
                         edad = CHCH05, 
                         dm_auto = CIDI01, 
                         ponderacion = PONDERACION)) 


## ENFR 2009
datos09_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2009 - Base usuario.txt",
                       col_select = c(id = IDENTIFI, 
                                      prov_id = PRVNC,
                                      sexo = BHCH04,
                                      edad = BHCH05,
                                      dm_auto = BIDI01,
                                      ponderacion = PONDERACION))


## ENFR 2013
datos13_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2013 - Base usuario.txt",
                       col_select = c(id = ID,
                                      prov_id = COD_PROVINCIA,
                                      sexo = BHCH04,
                                      edad = BHCH05,
                                      dm_auto = BIDI01,
                                      ponderacion = PONDERACION))


## ENFR 2018
datos18_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2018 - Base usuario.txt",
                       col_select = c(id,
                                      prov_id = cod_provincia,
                                      sexo = bhch03,
                                      edad = bhch04,
                                      dm_auto = bidi01,
                                      wf1p)) |> 
  
  # Añadir base de réplicas
  left_join(read_delim("Bases de datos/ENFR_bases/ENFR2018_base_rep_filter.csv"))


# Función para limpiar datos ----------------------------------------------
clean_enfr <- function(x){
  x |> 
    # Filtrar menores de 20 años
    filter(edad >= 20) |> 
    
    # Añadir etiquetas provincia
    left_join(id_prov) |> 
    
    # Crear grupos de edad cada 10 años
    mutate(grupo_edad_10 = age_categories(edad,
                                          lower = 20,
                                          upper = 80,
                                          by = 10,
                                          separator = " a "),
           .after = edad) |> 
    
    # Cambiar etiquetas sexo
    mutate(sexo = factor(sexo,
                         labels = c("Varón",
                                    "Mujer"))) |> 
    
    # Cambiar etiquetas diabetes por autorreporte
    mutate(dm_auto = factor(dm_auto,
                            labels = c("Si",
                                       "No",
                                       "NS/NC")))
}


# Limpiar datos -----------------------------------------------------------
## ENFR 2005
datos05 <- datos05_raw |> 
  # Aplicar función de limpieza
  clean_enfr()


## ENFR 2009
datos09 <- datos09_raw |> 
  # Aplicar función de limpieza
  clean_enfr()


## ENFR 2013
datos13 <- datos13_raw |> 
  # Aplicar función de limpieza
  clean_enfr()


## ENFR 2018
datos18 <- datos18_raw |> 
  # Aplicar función de limpieza
  clean_enfr()


# Explorar combinaciones faltantes ----------------------------------------
## Crear objeto para combinaciones posibles
comb_10 <- id_prov |> 
  cross_join(grupos_edad |>  
               select(grupo_edad_10)) |> 
  
  expand_grid(sexo = c("Varón", "Mujer"),
              dm_auto = "Si") |> 
  
  distinct()
  
## Faltantes ENFR 2005
comb_10 |> 
  anti_join(datos05)

## Faltantes ENFR 2009
comb_10 |> 
  anti_join(datos09)

## Faltantes ENFR 2013
comb_10 |> 
  anti_join(datos13)

## Faltantes ENFR 2018
comb_10 |> 
  anti_join(datos18)


# Calcular prevalencias ---------------------------------------------------
## ENFR 2005
prev05 <- datos05 |> 
  # Crear objeto diseño
  as_survey_design(weights = ponderacion) |> 

  # Calcular prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(vartype = "cv", na.rm = TRUE),
            prev_dm = survey_prop(vartype = "cv", na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")


## ENFR 2009
prev09 <- datos09 |> 
  # Crear objeto diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Calcular prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(vartype = "cv", na.rm = TRUE),
            prev_dm = survey_prop(vartype = "cv", na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")


## ENFR 2013 (Error de convergencia)
prev13 <- datos13 |> 
  # Crear objeto diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Calcular prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(vartype = "cv", na.rm = TRUE),
            prev_dm = survey_prop(vartype = "cv", na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")


## ENFR 2018
prev18 <- datos18 |> 
  # Crear objeto diseño
  as_survey_rep(weights = wf1p, 
                repweights = starts_with("wf1p"),
                type = "bootstrap"
                ) |> 
  
  # Calcular prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo, dm_auto) |> 
  summarise(freq_dm = survey_total(na.rm = TRUE),
            prev_dm = survey_prop(na.rm = TRUE),
            .groups = "drop") |> 
  
  # Seleccionar DM = Si
  filter(dm_auto == "Si")


# Unir bases prevalencia --------------------------------------------------
prev_join <- prev05 |> 
  bind_rows(prev09,
            prev13,
            prev18,
            .id = "anio_enfr") |> 
  
  # Reemplaza etiquetas año
  mutate(anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
  )