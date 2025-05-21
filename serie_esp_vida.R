### Limpieza de la tabla de vida para Argentina 2019 (GHO-WHO) por grupos de edad
### quinquenales y cada 10 años.
### Autora: Micaela Gauto 
### Colaboradora: Tamara Ricardo 
### Fecha de modificación:
# Wed May 21 10:50:14 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(tidyverse)


# Cargar datos crudos -----------------------------------------------------
## Etiquetas grupos de edad
grupos_edad <- read_csv("Bases de datos/grupos_edad.csv") |> 
  mutate_all(.funs = ~ factor(.x))

## Base esperanza de vida WHO-GHE
esp_vida_raw <- read_csv2("Bases de datos/WHO_GHO/argentina_tabla de vida_GHO.csv",
                          skip = 1)


# Limpiar datos -----------------------------------------------------------
esp_vida <- esp_vida_raw |> 
  # Estandarizar nombres de columna
  clean_names() |> 
  
  # Seleccionar columnas 2019
  select(indicator,
         grupo_edad = age_group,
         "Varón" = male_4,
         "Mujer" = female_5) |> 
  
  # Extraer primeras dos letras del estimador
  mutate(indicator = str_sub(indicator, start = 1, end = 2)) |>
  
  # Crear columna para sexo
  pivot_longer(cols = c("Varón", "Mujer"), 
               names_to = "sexo") |>
  
  # Crear columnas para indicadores
  pivot_wider(names_from = indicator,
              values_from = value) |> 
  
  # Filtrar menores de 20 años y mayores de 85 años
  filter(!str_detect(grupo_edad, "<1|1-4|5-9|10-14|15-19|85")) |> 
  
  # Cambiar las etiquetar grupo edad quinquenal
  mutate(grupo_edad = fct_relabel(grupo_edad, ~ levels(grupos_edad$grupo_edad))
  ) |> 
  
  # Añadir grupo edad cada 10 años
  left_join(grupos_edad) 


# Guardar datos limpios ---------------------------------------------------
write_delim(esp_vida, "Bases de datos/clean/arg_esp_vida_2019_clean.csv")
