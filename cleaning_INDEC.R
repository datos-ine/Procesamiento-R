### Limpieza y procesamiento de las proyecciones poblacionales de INDEC para los
### años 2010-2021 según provincia, sexo y grupo edad quinquenal.
### Se suma el cálculo por grupo de edad decenal y por región.
### Autoras: Tamara Ricardo y Micaela Gauto
### Fecha modificación:

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  geoAr,
  tabulapdf, # Extraer datos de PDF
  tidyverse,
  readxl
)


# Cargar datos -----------------------------------------------------------
## Etiquetas provincias ----
prov <- show_arg_codes() |>
  # Filtrar totales
  filter(between(codprov, "01", "24")) |>

  # Cambiar etiqueta CABA
  mutate(prov_nombre = if_else(codprov_censo == "02", id, name_iso))


## Proyecciones 2001-2005 ----
proy_01_05_raw <- extract_areas(
  "raw/INDEC_proyec 2001-2015.pdf",
  pages = c(22:24, 27:28, 25:26, 29:43, 45, 44)
)


## Proyecciones 2009-2018 ----
proy_10_18_raw <- {
  datos <- "raw/c2_proyecciones_prov_2010_2040.xls"
  hojas <- excel_sheets(datos)[-c(1:2)]

  leer_filas <- function(rango) {
    hojas |>
      set_names() |>
      map(~ read_excel(datos, sheet = .x, range = rango)) |>
      list_rbind(names_to = "prov")
  }

  bind_cols(
    leer_filas("A3:X28"), # 2010–2015
    leer_filas("A31:X56") # 2016–2021
  )
}


# Limpieza de datos ------------------------------------------------------
## Proyecciones 2001-2005 ----
proy_01_05 <- proy_01_05_raw |>
  # Asignar identificador numérico a cada provincia
  set_names(unique(prov$codprov_censo)) |>

  # Unir tablas de provincias
  list_rbind(names_to = "codprov_censo") |>

  # Estandarizar nombres de columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    codprov_censo,
    grupo_edad5 = x1,
    Varón_2001 = x2001,
    Mujer_2001 = x4,
    Varón_2005 = x2005,
    Mujer_2005 = x7
  ) |>

  # Filtrar grupos etarios no revelantes
  filter(
    between(grupo_edad5, "30-34", "45-49") |
      between(grupo_edad5, "50-54", "80 y más")
  ) |>

  # Añadir identificador categórico de provincia
  left_join(prov) |>

  # Base long
  pivot_longer(cols = c(Varón_2001:Mujer_2005), values_to = "pob_proy") |>

  # Separar sexo y año
  separate_wider_delim(name, delim = "_", names = c("sexo", "anio")) |>

  # Población a numérico
  mutate(pob_proy = parse_number(pob_proy, locale = locale(decimal_mark = ",")))


## Proyecciones 2010-2018 ----
proy_10_18 <- proy_10_18_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>

  # Seleccionar columnas relevantes
  select(
    codprov_censo = prov_1,
    grupo_edad5 = edad_2,
    Varón_2010 = x4,
    Mujer_2010 = x5,
    Varón_2013 = x16,
    Mujer_2013 = x17,
    Varón_2018 = x37,
    Mujer_2018 = x38
  ) |>

  # Filtrar grupos etarios no revelantes
  filter(
    between(grupo_edad5, "30-34", "45-49") |
      between(grupo_edad5, "50-54", "95-99") |
      grupo_edad5 == "100 y más"
  ) |>

  # Modificar identificador numérico de provincia
  mutate(codprov_censo = str_sub(codprov_censo, 1, 2)) |>

  # Añadir identificador categórico de provincia
  left_join(prov) |>

  # Base long
  pivot_longer(cols = c(Varón_2010:Mujer_2018), values_to = "pob") |>

  # Separar sexo y año
  separate_wider_delim(name, delim = "_", names = c("sexo", "anio")) |>

  # Población a numérico
  mutate(pob = parse_number(pob, locale = locale(decimal_mark = ","))) |>

  # Agrupar datos
  count(
    anio,
    codprov_censo,
    prov_nombre,
    sexo,
    grupo_edad5,
    wt = pob,
    name = "pob_proy"
  )


# Proyecciones por año, provincia, sexo y grupo etario -------------------
# Unir datasets proyecciones ----
proy_pob <- bind_rows(proy_01_05, proy_10_18) |>

  # Crear grupo edad decenal
  mutate(
    grupo_edad10 = case_when(
      between(grupo_edad5, "30-34", "35-39") ~ "30 a 39",
      between(grupo_edad5, "40-44", "45-49") ~ "40 a 49",
      between(grupo_edad5, "50-54", "55-59") ~ "50 a 59",
      between(grupo_edad5, "60-64", "65-69") ~ "60 a 69",
      between(grupo_edad5, "70-74", "75-79") ~ "70 a 79",
      .default = "80+"
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

  # Reagrupar datos
  count(
    anio,
    codprov_censo,
    prov_nombre,
    region_deis,
    sexo,
    grupo_edad10,
    wt = pob_proy,
    name = "pob_proy"
  )


## Proyección para 2009 por interpolación lineal ----
proy_09 <- proy_pob |>
  # Filtrar proyecciones 2010
  filter(anio %in% c("2001", "2010")) |>

  # Formato wide
  pivot_wider(
    names_from = anio,
    values_from = pob_proy,
    names_prefix = "pob_"
  ) |>

  # Interpolación lineal
  mutate(
    anio = "2009",
    tasa_anual = log(pob_2010 / pob_2001) / 9,
    proy_pob = round((pob_2001 * tasa_anual * 8) + pob_2001)
  ) |>

  # Seleccionar columnas
  select(
    anio,
    codprov_censo,
    prov_nombre,
    region_deis,
    sexo,
    grupo_edad10,
    proy_pob
  )


## Crear población estándar 2010 ----
pob_est_2010 <- proy_pob |>

  # Filtrar datos 2010
  filter(anio == 2010) |>

  # Descartar columnas innecesarias
  select(-anio) |>

  # Recalcular proyección
  count(
    sexo,
    grupo_edad10,
    wt = pob_proy,
    name = "pob_est_2010"
  )


## Unir datos ----
proy_pob_prov <- bind_rows(proy_pob, proy_09) |>
  # Añadir población estándar 2010
  left_join(pob_est_2010) |>

  # Ordenar filas
  arrange(anio, codprov_censo, sexo, grupo_edad10)


# Proyecciones por año, región, sexo y grupo etario ----------------------
## Dataset proyecciones x región----
proy_pob_reg <- proy_pob |>
  # Agrupar por región
  count(anio, region_deis, sexo, grupo_edad10, wt = pob_proy, name = "pob_proy")


## Proyección para 2009 por interpolación lineal x región----
proy_09_reg <- proy_pob_reg |>
  # Filtrar proyecciones 2010
  filter(anio %in% c("2001", "2010")) |>

  # Formato wide
  pivot_wider(
    names_from = anio,
    values_from = pob_proy,
    names_prefix = "pob_"
  ) |>

  # Interpolación lineal
  mutate(
    anio = "2009",
    tasa_anual = log(pob_2010 / pob_2001) / 9,
    proy_pob = round((pob_2001 * tasa_anual * 8) + pob_2001)
  ) |>

  # Seleccionar columnas
  select(
    anio,
    region_deis,
    sexo,
    grupo_edad10,
    proy_pob
  )


## Unir datos ----
proy_pob_reg <- bind_rows(proy_pob_reg, proy_09_reg) |>
  # Añadir población estándar 2010
  left_join(pob_est_2010) |>

  # Ordenar filas
  arrange(anio, region_deis, sexo, grupo_edad10)


# Guardar datos limpios ---------------------------------------------------
## Proyecciones por año, provincia, sexo y grupo etario decenal
write_csv(
  proy_pob_prov,
  file = "clean/arg_proy_2005_2018_ge10.csv"
)


## Proyecciones por año, región, sexo y grupo etario decenal
write_csv(
  proy_pob_reg,
  file = "clean/arg_proy_2005_2018_ge10_reg.csv"
)


# # Diccionario de datos ----------------------------------------------------
# data_dict <- tibble(
#   variable = names(proy_pob_prov),

#   descripcion = c(
#     "Año de realización ENFR",
#     "Año para la proyección poblacional (para 2009 se interpoló linealmente a partir de 2005 y 2010)",
#     "Identificador numérico de provincia",
#     "Identificador categórico de provincia",
#     # "Grupo de edad quinquenal",
#     "Grupo de edad decenal",
#     "Sexo biológico",
#     "Proyección poblacional",
#     "Población estándar Censo 2010"
#   ),

#   tipo_var = c(rep("factor", 6), rep("numeric", 2)),

#   valores = list(
#     c(2005, 2009, 2013, 2018),
#     c(2005, 2010, 2013, 2018),
#     levels(id_provincias$prov_id |> factor()),
#     levels(id_provincias$prov_nombre),
#     # levels(grupos_etarios$grupo_edad5),
#     levels(grupos_etarios$grupo_edad10),
#     c("Varón", "Mujer"),
#     "0-Inf",
#     "0-Inf"
#   ) |>
#     as.character() |>
#     str_remove_all('^c\\(|\\)$|"')
# )

# data_dict_reg <- tibble(
#   variable = c(
#     "anio_enfr",
#     "anio",
#     "region",
#     "grupo_edad10",
#     "sexo",
#     "proy_pob",
#     "pob_est_2010"
#   ),

#   descripcion = c(
#     "Año de realización ENFR",
#     "Año para la proyección poblacional (para 2009 se interpoló linealmente a partir de 2005 y 2010)",
#     "Identificador categórico de región",
#     "Grupo de edad decenal",
#     "Sexo biológico",
#     "Proyección poblacional",
#     "Población estándar Censo 2010"
#   ),

#   tipo_var = c(rep("factor", 5), rep("numeric", 2)),

#   valores = list(
#     c(2005, 2009, 2013, 2018),
#     c(2005, 2010, 2013, 2018),
#     levels(proy_join_10_reg$region |> factor()),
#     levels(grupos_etarios$grupo_edad10),
#     c("Varón", "Mujer"),
#     "0-Inf",
#     "0-Inf"
#   ) |>
#     as.character() |>
#     str_remove_all('^c\\(|\\)$|"')
# )


# ## Guardar diccionario de datos
# export(data_dict, file = "Bases de datos/clean/dic_arg_proy_2005_2018.xlsx")

# export(
#   data_dict_reg,
#   file = "Bases de datos/clean/dic_arg_proy_2005_2018_reg.xlsx"
# )


# ## Limpiar environment y desactivar paquetes
# rm(list = ls())

# pacman::p_unload("all")
