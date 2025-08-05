### Proyecciones poblacionales 2005: Extracción de datos a partir de archivo PDF
### Autora: Tamara Ricardo
### Fecha modificación:
# Tue Jul  8 15:49:43 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  tabulapdf, # Extraer datos de PDF
  janitor,
  tidyverse
)


# Cargar ids provincias y grupos etarios ----------------------------------
## Etiquetas provincias INDEC
id_provincias <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_etarios <- read_csv("Bases de datos/grupos_etarios.csv") |> 
  mutate_all(~ factor(.x)) |> 
  filter(!str_detect(grupo_edad_5, "20-24|25-29|30-34"))


# Extraer tablas PDF ------------------------------------------------------
proy_01_05_raw <- extract_areas(
  "Bases de datos/Proyecciones INDEC/INDEC_proyec 2001-2015.pdf",
  pages = c(22:24, 27:28, 25:26, 29:43, 45, 44))


# Limpiar datos -----------------------------------------------------------
proy_05 <- proy_01_05_raw |>
  # Asignar identificador numérico a cada provincia
  set_names(unique(id_provincias$prov_id)) |>
  
  # Unir tablas de provincias
  list_rbind(names_to = "prov_id") |>
  
  # Estandarizar nombres de columnas
  clean_names() |>
  
  # Seleccionar columnas relevantes
  select(prov_id,
         grupo_edad = x1,
         Varón_2001 = x2001,
         Mujer_2001 = x4,
         Varón_2005 = x2005,
         Mujer_2005 = x7) |>
  
  # Filtrar filas con valores ausentes
  drop_na() |>
  
  # Filtrar <20 años y totales
  filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19")) |>
  
  # Identificador de provincia a numérico
  mutate(prov_id = parse_number(prov_id)) |>
  
  # Pasar a formato long para obtener proyecciones
  pivot_longer(cols = c(Varón_2001:Mujer_2005)) |>
  
  # Crear columnas para sexo y año
  separate(name, into = c("sexo", "anio"), sep = "_") |>
  
  # Transformar escala proyección poblacional
  mutate(value = parse_number(value, locale = locale(decimal_mark = ",")))


# ## Guardar (opcional)
# write_csv(proy_05, "Bases de datos/Proyecciones INDEC/proy_2005.csv")
