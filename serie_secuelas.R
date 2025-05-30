### Cálculo de la frecuencia de secuelas de DM por grupo de edad y sexo 
### Se diferencian secuelas por sexo --> FALTA ACTUALIZAR INFO, QUEDA POR AHORA IGUAL
### POR SEXO. Asumo misma proporción de secuela en cada grupo de edad, 
### jurisdicción y año. 
## Autora: Micaela Gauto 
## Colaboradora: Tamara Ricardo 
## Fecha modificación: 
# Thu May 29 14:53:34 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor, 
  tidyverse
)


# Cargar datos crudos -----------------------------------------------------
dw_raw <- import("Bases de datos/GBD/IHME_GBD_2019_DISABILITY_WEIGHTS_Y2020M010D15.XLSX",
             skip = 1)


# Limpiar datos -----------------------------------------------------------
dw_dm <- dw_raw |> 
  # Estandarizar nombres columnas
  clean_names() |> 
  
  # Filtrar campos DM2
  filter(str_detect(sequela, "diabetes mellitus type 2| type 2 diab")) |> 
  
  # Separar columnas DW
  separate(disability_weight, into = c("dw", "ci"), sep = "\\(") |> 
  
  separate(ci, into = c("lower", "upper"), sep = "-") |> 
  
  # Limpiar formato columnas
  mutate(upper = str_remove(upper, "\\)")) |> 
  
  # Cambiar a numérico
  mutate(across(.cols = c(dw, lower, upper),
                .fns = ~ parse_number(.x))) |> 
  
  # Añadir frecuencias Wandurranga
  mutate(frec_wandurranga = case_when(
    str_detect(sequela, "Uncomplicated") ~ 37.14, # Sin complicaciones
    str_detect(sequela, "Severe vision") ~ 12.46, # Retinopatía severa
    str_detect(sequela, "type 2, without") ~ 30.91, # Neuropatía s/amputación
    TRUE ~ NA
  ))


# Guardar datos -----------------------------------------------------------
write_csv(dw_dm, file = "Bases de datos/clean/comp_dm_temp.csv")

