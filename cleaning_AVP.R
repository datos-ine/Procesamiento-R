### Limpieza y procesamiento de las bases de datos de mortalidad publicadas por
### la Dirección de Estadísticas e Información de Salud (DEIS), considerando
### como causa básica de muerte los códigos E10 a E14 de la Décima Revisión de
### la Clasificación Estadística Internacional de Enfermedades y Problemas
### Relacionados con la Salud (CIE-10).
### Limpieza y procesamiento de las tablas de vida publicadas para Argentina en
### el año 2019 por la GHO-WHO, considerando grupos de edad quinquenales y cada
### 10 años para población de 30 años y más según sexo.
### Autoras: Micaela Gauto y Tamara Ricardo
# Última modificación: 20-01-2026 12:54

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  geoAr,
  tidyverse
)


# Cargar datos crudos -----------------------------------------------------
## Etiquetas provincias ----
prov <- import("clean/cod_prov_arg.rds")


## Esperanza vida ----
ex_raw <- read_csv2("raw/argentina_tabla de vida_GHO.csv", skip = 1)


## Defunciones 2004 ----
def04_raw <- import("raw/DEIS/DE_2004.csv")


## Defunciones 2005-2019 ----
def05_19_raw <- list.files(
  path = "raw/DEIS/",
  pattern = "^defweb.",
  full.names = TRUE
)


# Limpiar datos defunciones ----------------------------------------------
## Defunciones 2004 ----
def04 <- def04_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>
  rename(
    prov_nombre = jurisdiccion,
    grupo_edad = grupo_de_edad,
    cie10_causa = causa_de_muerte_cie_10
  ) |>

  # Filtrar datos ausentes provincia defunción
  filter(!prov_nombre %in% c("Lugar no especificado", "Otro país")) |>

  # Filtrar datos ausentes sexo
  filter(between(sexo, "Mujer", "Varón")) |>

  # Filtrar menores de edad y datos ausentes
  filter(between(grupo_edad, "13.30 a 34", "24.85 y más")) |>

  # Cambiar etiqueta CABA
  mutate(
    prov_nombre = if_else(
      str_detect(prov_nombre, "Ciudad"),
      "CABA",
      prov_nombre
    )
  ) |>

  # Añadir identificador numérico provincias
  left_join(prov)


## Defunciones 2005-2019 ----
def05_19 <- def05_19_raw |>
  # Crear columna para el año
  set_names(nm = paste0("20", str_sub(def05_19_raw, 16, 17))) |>

  # Leer archivos csv
  map(read_csv, locale = locale(encoding = "WINDOWS-1252")) |>

  # Unir datasets
  list_rbind(names_to = "anio") |>

  # Estandarizar nombres de columnas
  clean_names() |>
  rename(
    codprov_censo = provres,
    grupo_edad = grupedad,
    cie10_causa = causa,
    total = cuenta
  ) |>

  # Filtrar datos ausentes provincia
  filter(between(codprov_censo, "02", "94")) |>

  # Filtrar datos ausentes sexo
  filter(between(sexo, 1, 2)) |>

  # Filtrar menores de edad y datos ausentes
  filter(between(grupo_edad, "07_30 a 34", "17_80 y más")) |>

  # Cambiar niveles sexo
  mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) |>

  # Añadir identificador categórico provincias
  left_join(prov)


## Unir datasets defunciones ----
defun <- bind_rows(def04, def05_19) |>

  # Filtrar muertes por DM2 (E11 y E14)
  filter(cie10_causa %in% c("E11", "E14")) |>

  # Cambiar etiquetas grupo etario
  mutate(grupo_edad = str_sub(grupo_edad, 4)) |>

  # Crear grupo edad decenal
  mutate(
    grupo_edad10 = case_when(
      between(grupo_edad, "30 a 34", "35 a 39") ~ "30 a 39",
      between(grupo_edad, "40 a 44", "45 a 49") ~ "40 a 49",
      between(grupo_edad, "50 a 54", "55 a 59") ~ "50 a 59",
      between(grupo_edad, "60 a 64", "65 a 69") ~ "60 a 69",
      between(grupo_edad, "70 a 74", "75 a 79") ~ "70 a 79",
      .default = "80+"
    )
  ) |>

  # Completar datos faltantes año defunción
  mutate(anio = replace_na(anio, "2004")) |>

  # Añadir año ENFR
  mutate(
    anio_enfr = case_when(
      between(anio, "2004", "2006") ~ "2005",
      between(anio, "2008", "2010") ~ "2009",
      between(anio, "2012", "2014") ~ "2013",
      between(anio, "2017", "2019") ~ "2018"
    )
  ) |>

  # Crear región geográfica DEIS
  mutate(
    region_deis = case_when(
      codprov_censo %in% c("02", "06", "14", "30", "82") ~ "Centro",
      codprov_censo %in% c("18", "22", "34", "54") ~ "NEA",
      codprov_censo %in% c("38", "66", "90") ~ "NOA1",
      codprov_censo %in% c("10", "86") ~ "NOA2",
      codprov_censo %in% c("46", "50", "70", "74") ~ "Cuyo",
      codprov_censo %in% c("42", "58", "62") ~ "Patagonia Norte",
      .default = "Patagonia Sur"
    )
  ) |>

  # Añadir filas faltantes
  complete(
    nesting(anio, anio_enfr),
    nesting(codprov_censo, prov_nombre, region_deis),
    nesting(grupo_edad, grupo_edad10),
    sexo,
    fill = list(total = 0)
  ) |>

  # Agrupar datos por grupos decenales
  count(
    anio,
    anio_enfr,
    codprov_censo,
    prov_nombre,
    region_deis,
    grupo_edad10,
    sexo,
    wt = total
  )


# Limpiar datos esperanza de vida ----------------------------------------
ex_ge10 <- ex_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>
  select(
    indicator,
    age_group,
    "Varón" = male_4,
    "Mujer" = female_5
  ) |>

  # Filtrar grupos edad no relevantes
  filter(
    between(age_group, "30-34 years", "45-49 years") |
      between(age_group, "50-54 years", "85+ years")
  ) |>

  # Cambiar niveles indicador
  mutate(indicator = str_extract(indicator, '^[^ ]+')) |>

  # Base long
  pivot_longer(cols = c(Varón, Mujer), names_to = "sexo") |>

  # Base wide
  pivot_wider(names_from = indicator, values_from = value) |>

  # Crear grupo edad decenal
  mutate(
    grupo_edad10 = case_when(
      between(age_group, "30 a 34", "35 a 39") ~ "30 a 39",
      between(age_group, "40 a 44", "45 a 49") ~ "40 a 49",
      between(age_group, "50 a 54", "55 a 59") ~ "50 a 59",
      between(age_group, "60 a 64", "65 a 69") ~ "60 a 69",
      between(age_group, "70 a 74", "75 a 79") ~ "70 a 79",
      .default = "80+"
    )
  ) |>

  # Recalcular indicadores por grupo decenal
  group_by(sexo, grupo_edad10) |>
  summarise(
    lx = first(lx),
    nLx = sum(nLx, na.rm = TRUE),
    ndx = sum(ndx, na.rm = TRUE),
    nMx = sum(nMx * nLx, na.rm = TRUE) / sum(nLx, na.rm = TRUE),
    nqx = sum(nqx * nLx, na.rm = TRUE) / sum(nLx, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Calcular Tx y ex
  group_by(sexo) |>
  mutate(
    Tx = rev(cumsum(rev(nLx))),
    ex = Tx / lx
  ) |>
  ungroup()


# Explorar datos ----------------------------------------------------------
tabyl(def04$prov_nombre)

tabyl(def04$sexo)

tabyl(def04$grupo_edad)

tabyl(def05_19$prov_nombre)

tabyl(def05_19$sexo)

tabyl(def05_19$grupo_edad)

tabyl(defun$prov_nombre)

tabyl(defun$sexo)

tabyl(defun$grupo_edad10)

tabyl(ex_ge10$grupo_edad10)


# Calcular AVP ------------------------------------------------------------
## Por provincia, sexo y grupo edad decenal ----
AVP_ge10 <- defun |>
  # Agrupar datos
  group_by(
    anio_enfr,
    codprov_censo,
    prov_nombre,
    region_deis,
    grupo_edad10,
    sexo
  ) |>

  # Calcular defunciones por trienio ENFR
  summarise(
    defun_n = sum(n, na.rm = TRUE),
    defun_mean = mean(n, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Añadir datos esperanza de vida
  left_join(
    ex_ge10 |>
      select(sexo:lx, Tx, ex)
  ) |>

  # Calcular AVP por grupo decenal
  mutate(AVP = defun_mean * ex) |>

  # Variables caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))


## Por región, sexo y grupo edad decenal ----
AVP_ge10_reg <- defun |>
  # Agrupar datos
  group_by(
    anio_enfr,
    region_deis,
    grupo_edad10,
    sexo
  ) |>

  # Calcular defunciones por trienio ENFR
  summarise(
    defun_n = sum(n, na.rm = TRUE),
    defun_mean = mean(n, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Añadir datos esperanza de vida
  left_join(ex_ge10) |>

  # Calcular AVP por grupo decenal
  mutate(AVP = defun_mean * ex)


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = names(AVP_ge10),

  descripcion = c(
    "Año de realización de la Encuesta Nacional de Factores de Riesgo (ENFR)",
    "Identificador numérico de provincia según clasificación INDEC",
    "Identificador categórico de provincia",
    "Región geográfica según clasificación DEIS (2021)",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Número de defunciones para el trienio correspondiente",
    "Promedio de defunciones para el trienio correspondiente",
    "Cantidad de personas vivas a la edad x",
    "Años-persona vividos por encima de la edad x",
    "Esperanza de vida a la edad x",
    "Años de vida perdidos por muerte prematura por diabetes mellitus"
  ),

  tipo_var = map_chr(AVP_ge10, ~ paste(class(.x), collapse = ", ")),

  niveles = map_chr(
    AVP_ge10,
    ~ if (is.factor(.x)) {
      paste(levels(.x), collapse = ", ")
    } else {
      "O-Inf"
    }
  )
)


# Guardar datos limpios ---------------------------------------------------
## Defunciones por provincia
export(AVP_ge10, file = "clean/arg_defun_avp_30_prov.rds")

## Defunciones por región
export(AVP_ge10_reg, file = "clean/arg_defun_avp_30_reg.rds")

## Diccionario de datos
export(data_dict, file = "clean/dic_arg_defun_avp.xlsx")

# Limpiar environment y desactivar paquetes ------------------------------
rm(list = ls())

pacman::p_unload("all")
