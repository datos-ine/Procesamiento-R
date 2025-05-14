### Cálculo de la frecuencia de secuelas de DM por grupo de edad y sexo 
### Se diferencian secuelas por sexo --> FALTA ACTUALIZAR INFO, QUEDA POR AHORA IGUAL
### POR SEXO. Asumo misma proporción de secuela en cada grupo de edad, 
### jurisdicción y año. 
## Autora: Micaela Gauto 
## Colaboradora: Tamara Ricardo 
## Fecha modificación: 
# Wed May 14 10:41:47 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(rio)
library(janitor)
library(tidyverse)

# Cargar datos crudos -----------------------------------------------------
DW_raw <- import("Bases de datos/Complicaciones y DW.xlsx",
                 sheet = "DW_DM")


# Limpieza de datos -------------------------------------------------------
DW_clean <- DW_raw |> 
  
  # Estandarizar nombres de columnas
  clean_names() |> 
  
  # Eliminar datos ausentes
  drop_na() |> 
  
  # Variables caracter a factor
  mutate(across(.cols = where(is.character),
                .fns = ~ factor(.x)))


# Secuelas por grupo etario -----------------------------------------------
# Grupo etario quinquenal
dw_edad_quin <- tibble(
  grupo_edad_quin = c("15 a 19",
                 "20 a 24",
                 "25 a 29",
                 "30 a 34",
                 "35 a 39",
                 "40 a 44",
                 "45 a 49",
                 "50 a 54",
                 "55 a 59",
                 "60 a 64",
                 "65 a 69",
                 "70 a 74",
                 "75 a 79",
                 "80 y más") |> 
    rep(4)) |> 
  
  # Ordenar por grupo edad
  arrange(grupo_edad_quin) |> 
  
  # Agregar secuelas
  mutate(secuela = rep(levels(DW_clean$secuela), 14)) |> 
  
  # Agregar pesos y frecuencias
  left_join(DW_clean)


# Grupo etario ENFR
dw_edad_enfr <- tibble(
  grupo_edad_enfr = c("18 a 24",
                      "25 a 34",
                      "35 a 49",
                      "50 a 64",
                      "65+") |> 
    rep(4)) |> 
  
  # Ordenar por grupo edad
  arrange(grupo_edad_enfr) |> 
  
  # Agregar secuelas
  mutate(secuela = rep(levels(DW_clean$secuela), 5)) |> 
  
  # Agregar pesos y frecuencias
  left_join(DW_clean)


# Guardar datos limpios ---------------------------------------------------
write_csv(dw_edad_quin, file = "Bases de datos/clean/dw_edad_quin.csv")

write_csv(dw_edad_enfr, file = "Bases de datos/clean/dw_edad_enfr.csv")

