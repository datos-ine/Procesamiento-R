### Proyecciones poblacionales 2005: Extracción de datos a partir de archivo PDF
### Autora: Tamara Ricardo
### Fecha creación: 08-07-2025
# Última modificación: 21-01-2026 08:55

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  tabulapdf, # Extraer datos de PDF
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Etiquetas provincias ----
prov <- import("bases_de_datos/cod_prov_arg.rds")


## Proyecciones 2001-2005 ----
proy_01_05_raw <- extract_areas(
  "bases_de_datos/INDEC_proyec 2001-2015.pdf",
  pages = c(22:24, 27:28, 25:26, 29:43, 45, 44)
)


# Limpiar datos -----------------------------------------------------------
proy_01_05 <- proy_01_05_raw |>
  # Asignar identificador numérico de provincia
  set_names(unique(prov$codprov_censo)) |>

  # Unir tablas de provincias
  list_rbind(names_to = "codprov_censo") |>

  # Estandarizar nombres de columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    codprov_censo,
    grupo_edad = x1,
    Varón_2001 = x2001,
    Mujer_2001 = x4,
    Varón_2005 = x2005,
    Mujer_2005 = x7
  ) |>

  # Filtrar grupos etarios no revelantes
  filter(
    between(grupo_edad, "30-34", "45-49") |
      between(grupo_edad, "50-54", "80 y más")
  ) |>

  # Añadir identificador categórico de provincia
  left_join(prov) |>

  # Base long
  pivot_longer(cols = c(Varón_2001:Mujer_2005), values_to = "pob_proy") |>

  # Separar sexo y año
  separate_wider_delim(name, delim = "_", names = c("sexo", "anio")) |>

  # Población a numérico
  mutate(pob_proy = parse_number(pob_proy, locale = locale(decimal_mark = ",")))


# Exportar datos limpios -------------------------------------------------
export(proy_01_05, file = "bases_de_datos/arg_proy_2001_2005.rds")


# Limpiar environment y desactivar paquetes ------------------------------
rm(list = ls())

pacman::p_unload("all")
