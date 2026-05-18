### Limpieza de los dataset:
## - Proyecciones poblacionales por sexo y grupo etario quinquenal, 2001-2018 (INDEC)
## - Población estándar por sexo y grupo etario, Argentina, Censo 2010 (INDEC)
### Autora: Tamara Ricardo
# Última modificación: 18-05-2026 08:13

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  tabulapdf,
  rio,
  janitor,
  tidyverse,
  readxl
)


# Cargar datos -----------------------------------------------------------
## Proyecciones poblacionales 2001 y 2005 --------------------------------
proy_01_05_raw <- extract_areas(
  file = "bases_datos/INDEC_proyec 2001-2015.pdf",
  pages = 21
)


## Proyecciones poblacionales 2010, 2013 y 2018 --------------------------
proy_10_18_raw <- proy_10_18_raw <- c("A3:D28", "N3:P28", "J31:L56") |>
  map(
    ~ {
      read_excel(
        "bases_datos/c2_proyecciones_prov_2010_2040.xls",
        sheet = 2,
        range = .x
      ) |>
        remove_empty("cols")
    }
  ) |>
  list_cbind() |>
  clean_names()


## Población estándar Censo 2010 -----------------------------------------
pob_est_2010_raw <- import(
  "bases_datos/c2_proyecciones_prov_2010_2040.xls",
  sheet = 2,
  range = "A3:D28"
)


# Limpiar datos ----------------------------------------------------------
## Proyecciones poblacionales (total país) -----
proy_01_18 <- proy_01_05_raw |>
  # Unir tablas
  list_rbind() |>

  # Seleccionar columnas
  select(
    grupo_edad = 1,
    Total_2001 = 2,
    Varón_2001 = 3,
    Mujer_2001 = 4,
    Total_2005 = 5,
    Varón_2005 = 6,
    Mujer_2005 = 7
  ) |>

  # Descartar filas vacías
  drop_na() |>

  # Pasar a formato long
  pivot_longer(cols = Total_2001:Mujer_2005) |>

  # Unir con proyecciones 2010, 2013 y 2018 -----
  bind_rows(
    proy_10_18_raw |>
      # Seleccionar columnas
      select(
        grupo_edad = 1,
        Total_2010 = 2,
        Varón_2010 = 3,
        Mujer_2010 = 4,
        Total_2013 = 5,
        Varón_2013 = 6,
        Mujer_2013 = 7,
        Total_2018 = 8,
        Varón_2018 = 9,
        Mujer_2018 = 10
      ) |>
      drop_na() |>

      # Pasar a formato long
      pivot_longer(cols = Total_2010:Mujer_2018)
  ) |>

  # Crear grupo etario decenal
  mutate(
    grupo_edad_10 = case_when(
      between(grupo_edad, "30-34", "35-39") ~ "30 a 39",
      between(grupo_edad, "40-44", "45-49") ~ "40 a 49",
      between(grupo_edad, "50-54", "55-59") ~ "50 a 59",
      between(grupo_edad, "60-64", "65-69") ~ "60 a 69",
      between(grupo_edad, "70-74", "75-79") ~ "70 a 79",
      between(grupo_edad, "80 y más", "95-99") |
        grupo_edad == "100 y más" ~ "80+",
      .default = NA
    )
  ) |>

  # Separar sexo y año
  separate_wider_delim(name, delim = "_", names = c("sexo", "anio_enfr")) |>

  # Población a numérico
  mutate(value = parse_number(value, locale = locale(decimal_mark = ","))) |>

  # Descartar NAs
  drop_na() |>

  # Recalcular proyecciones
  count(anio_enfr, sexo, grupo_edad_10, wt = value, name = "proy_pob") |>

  # Estimar población 2009 por interpolación lineal
  (\(x) {
    bind_rows(
      x,
      # Seleccionar datos de 2001 y 2010
      x |>
        filter(anio_enfr %in% c("2001", "2010")) |>
        pivot_wider(
          names_from = anio_enfr,
          values_from = proy_pob,
          names_prefix = "pob_"
        ) |>

        # Interpolar datos para 2009
        mutate(
          anio_enfr = "2009",
          tasa_anual = log(pob_2010 / pob_2001) / 9,
          proy_pob = round(pob_2001 * (1 + tasa_anual * 8)),
          pob_2001 = NULL,
          pob_2010 = NULL,
          tasa_anual = NULL
        )
    )
  })() |>

  # Quitar datos 2001 y 2010
  filter_out(anio_enfr %in% c("2001", "2010"))


## Población estándar 2010 -----
pob_est_2010 <- pob_est_2010_raw |>
  # Estandarizar nombres de columnas
  rename(
    grupo_edad = 1,
    Total = 2,
    "Varón" = 3,
    "Mujer" = 4
  ) |>

  # Crear grupo etario decenal
  mutate(
    grupo_edad_10 = case_when(
      between(grupo_edad, "30-34", "35-39") ~ "30 a 39",
      between(grupo_edad, "40-44", "45-49") ~ "40 a 49",
      between(grupo_edad, "50-54", "55-59") ~ "50 a 59",
      between(grupo_edad, "60-64", "65-69") ~ "60 a 69",
      between(grupo_edad, "70-74", "75-79") ~ "70 a 79",
      between(grupo_edad, "80 y más", "95-99") |
        grupo_edad == "100 y más" ~ "80+",
      .default = NA
    )
  ) |>

  # Descartar filas con NAs
  drop_na() |>

  # Pasar a formato long
  pivot_longer(cols = c("Total":"Mujer"), names_to = "sexo") |>

  # Población a numérico
  mutate(value = parse_number(value, locale = locale(decimal_mark = ","))) |>

  # Agrupar por grupo etario decenal
  count(grupo_edad_10, sexo, wt = value, name = "pob_est")


# Guardar datos limpios --------------------------------------------------
## Proyecciones poblacionales (total país)
export(proy_01_18, file = "datos_limpios/arg_proy_pob.rds")

## Población estándar 2010
export(pob_est_2010, file = "datos_limpios/pob_est_2010.rds")


# Limpiar environment ----------------------------------------------------
rm(list = ls())

pacman::p_unload("all")
