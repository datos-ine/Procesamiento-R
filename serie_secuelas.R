### Cálculo de la frecuencia de secuelas de DM por grupo de edad y sexo 
### Se diferencian secuelas por sexo --> FALTA ACTUALIZAR INFO, QUEDA POR AHORA IGUAL
### POR SEXO. Asumo misma proporción de secuela en cada grupo de edad, 
### jurisdicción y año. 
## Autora: Micaela Gauto 
## Colaboradora: Tamara Ricardo 
## Fecha modificación: 
# Wed May 14 11:39:28 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(rio)
library(janitor)
library(tidyverse)


# Cargar datos crudos -----------------------------------------------------
## Secuelas DM
DW_raw <- import("Bases de datos/Complicaciones y DW.xlsx",
                 sheet = "DW_DM")

## Prevalencia DM Argentina por grupos edad ENFR
prev_dm <- read_csv("Bases de datos/clean/prev_dm_enfr.csv")


# Limpieza de datos -------------------------------------------------------
DW_clean <- DW_raw |> 
  
  # Estandarizar nombres de columnas
  clean_names() |> 
  
  # Eliminar datos ausentes
  drop_na() |> 
  
  # Variables caracter a factor
  mutate(across(.cols = where(is.character),
                .fns = ~ factor(.x)))


# Calcular AVD ------------------------------------------------------------
# Asumiendo mismas frecuencias de secuelas por provincia, sexo y grupo edad ENFR
prev_dm_dw <- prev_dm |>
  # Completar filas sin prevalencia DM
  complete(anio_enfr, prov_nombre, sexo, grupo_edad_enfr,
           fill = list(prev = 0)) |> 
  
 # Añadir pesos discapacidad
  cross_join(DW_clean |> 
               select(-sequela)) |> 
  
  # Calcular AVD por secuela
  mutate(AVD_indiv = (freq_dm * frecuencia_wandurranga) / (100 * disability_weight)) |> 
  
  # Calcular AVD por secuela agrupados
  group_by(anio_enfr, prov_nombre, grupo_edad_enfr, sexo, freq_dm, prev_dm) |> 
  summarise(AVD_total = sum(AVD_indiv, na.rm = T),
            .groups = "drop")


# Guardar datos limpios ---------------------------------------------------
write_csv(prev_dm_dw, file = "Bases de datos/clean/prev_dm_avd.csv") # Revisar

# Limpiar environment
rm(list = ls())
