### Limpieza y procesamiento de las proyecciones poblacionales de INDEC para los
### años 2010-2021 según provincia, sexo y grupo edad quinquenal
### Autora: Tamara Ricardo
### Fecha modificación:
# Wed May 21 18:18:51 2025 ------------------------------



# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(tidyverse)
library(readxl)


# Cargar datos ------------------------------------------------------------
## Etiquetas provincias INDEC
id_prov <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_edad <- read_csv("Bases de datos/grupos_edad.csv") |> 
  mutate_all(.funs = ~ factor(.x))


## Ruta del archivo excel
proy_indec <- "Bases de datos/Proyecciones INDEC/c2_proyecciones_prov_2010_2040.xls"

## Cargar/unir hojas
proy_raw <- excel_sheets(proy_indec)[-c(1:2)] |>  # Listar hojas por provincia
  # Crear columna para la provincia
  set_names() |> 
  
  # Leer filas para 2010-2015 y unir por provincia
  map(~ read_excel(proy_indec, sheet = .x, range = "A3:X28")) |> 
  list_rbind(names_to = "prov") |> 
  
  # Leer filas para 2016-2021 y unir por provincia
  bind_cols(
    excel_sheets(proy_indec)[-c(1:2)] |>  # Listar hojas por provincia
      # Crear columna para la provincia
      set_names() |> 
      
      # Leer filas para 2010-2015 y unir por provincia
      map(~ read_excel(proy_indec, sheet = .x, range = "A31:X56")) |> 
      list_rbind(names_to = "prov")
  )
  
  
# Limpiar datos -----------------------------------------------------------
proy_clean <- proy_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  
  # Seleccionar columnas relevantes
  select(prov_id = prov_1,
         grupo_edad = edad_2,
         V_2010 = x4,
         M_2010 = x5,
         V_2013 = x16,
         M_2013 = x17,
         V_2018 = x37,
         M_2018 = x38)  |> 
  
  # Filtrar filas con totales o datos ausentes
  filter(!grupo_edad %in% c(NA, "Total")) |> 
  
  # Limpiar id de provincia
  mutate(prov_id = str_sub(prov_id, 1, 2) |> 
           as.numeric()) |> 
  
  # Añadir identificador categórico provincia
  left_join(id_prov) |> 
  
  # Formato long
  pivot_longer(cols = c(V_2010:M_2018), 
               values_to = "proyeccion") |> 
  
  # Crear columnas para año y sexo
  separate(name, into = c("sexo", "anio")) |> 
  
  # Modificar etiquetas sexo
  mutate(sexo = if_else(sexo == "V", "Varón", "Mujer")) |> 
  
  # Filtrar menores de 20 años
  filter(!grupo_edad %in% c("0-4", "5-9", "10-14", "15-19")) |> 
  
  # Agrupar mayores de 80 años
  mutate(grupo_edad = fct_collapse(grupo_edad,
                                 "80+" = c("80-84", "85-89","90-94",
                                           "95-99", "100 y más"))) |> 
  
  # Cambiar etiquetas grupos edad
  mutate(grupo_edad = fct_relabel(grupo_edad, ~ levels(grupos_edad$grupo_edad))
         ) |> 
  
  # Convertir proyección a numérico
  mutate(proyeccion = as.numeric(proyeccion)) |> 
  
  # Calcular población para mayores de 80 años
  group_by(anio, prov_id, prov_nombre, sexo, grupo_edad) |> 
  summarise(proyeccion = sum(proyeccion, na.rm = TRUE),
            .groups = "drop")
  

# Guardar datos limpios ---------------------------------------------------
write_csv(proy_clean, file = "Bases de datos/clean/arg_proy_2010_2018.csv")



