### Limpieza y procesamiento de las proyecciones poblacionales de INDEC para los
### años 2010-2021 según provincia, sexo y grupo edad quinquenal.
### Se suma el cálculo por grupo de edad decenal y por región.
### Autoras: Tamara Ricardo y Micaela Gauto
# Última modificación: 21-01-2026 08:53

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse,
  readxl
)


# Cargar datos -----------------------------------------------------------
## Etiquetas provincias ----
prov <- import("bases_de_datos/cod_prov_arg.rds")


## Proyecciones 2001-2005 ----
proy_01_05 <- import("bases_de_datos/arg_proy_2001_2005.rds")


## Proyecciones 2009-2018 ----
proy_10_18_raw <- {
  leer_filas <- function(rango) {
    excel_sheets("bases_de_datos/c2_proyecciones_prov_2010_2040.xls")[
      -c(1:2)
    ] |>
      set_names() |>
      map(
        ~ read_excel(
          "bases_de_datos/c2_proyecciones_prov_2010_2040.xls",
          sheet = .x,
          range = rango
        )
      ) |>
      list_rbind(names_to = "prov")
  }

  bind_cols(
    leer_filas(rango = "A3:X28"), # 2010–2015
    leer_filas(rango = "A31:X56") # 2016–2021
  )
}


# Limpieza de datos ------------------------------------------------------
## Proyecciones 2010-2018
proy_10_18 <- proy_10_18_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>

  # Seleccionar columnas relevantes
  select(
    codprov_censo = prov_1,
    grupo_edad = edad_2,
    Varón_2010 = x4,
    Mujer_2010 = x5,
    Varón_2013 = x16,
    Mujer_2013 = x17,
    Varón_2018 = x37,
    Mujer_2018 = x38
  ) |>

  # Filtrar grupos etarios no revelantes
  filter(
    between(grupo_edad, "30-34", "45-49") |
      between(grupo_edad, "50-54", "95-99") |
      grupo_edad == "100 y más"
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
    grupo_edad,
    wt = pob,
    name = "pob_proy"
  )


# Calcular proyecciones por provincia, sexo y grupo etario ---------------
proy_pob_prov <- bind_rows(proy_01_05, proy_10_18) |>
  # Crear grupo etario decenal
  mutate(
    grupo_edad10 = case_when(
      between(grupo_edad, "30-34", "35-39") ~ "30 a 39",
      between(grupo_edad, "40-44", "45-49") ~ "40 a 49",
      between(grupo_edad, "50-54", "55-59") ~ "50 a 59",
      between(grupo_edad, "60-64", "65-69") ~ "60 a 69",
      between(grupo_edad, "70-74", "75-79") ~ "70 a 79",
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
  ) |>

  # Añadir población estimada para 2009 (interpolación lineal)
  (\(x) {
    bind_rows(
      x,
      x |>
        filter(anio %in% c("2001", "2010")) |>
        pivot_wider(
          names_from = anio,
          values_from = pob_proy,
          names_prefix = "pob_"
        ) |>

        mutate(
          anio = "2009",
          tasa_anual = log(pob_2010 / pob_2001) / 9,
          pob_proy = round(pob_2001 * (1 + tasa_anual * 8))
        )
    )
  })() |>

  # Añadir población estándar 2010
  (\(x) {
    left_join(
      x,
      x |>
        filter(anio == "2010") |>
        count(sexo, grupo_edad10, wt = pob_proy, name = "pob_est_2010")
    )
  })() |>

  # Descartar columnas innecesarias
  select(anio:pob_proy, pob_est_2010) |>

  # Ordenar filas
  arrange(anio, codprov_censo, sexo, grupo_edad10) |>

  # Variables caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))


# Calcular proyecciones por región, sexo y grupo etario ------------------
proy_pob_reg <- proy_pob_prov |>
  # Agrupar por región
  group_by(anio, region_deis, sexo, grupo_edad10) |>
  summarise(
    pob_proy = sum(pob_proy, na.rm = TRUE),
    pob_est_2010 = sum(pob_est_2010, na.rm = TRUE),
    .groups = "drop"
  )


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = names(proy_pob_prov),
  descripcion = c(
    "Año de realización ENFR",
    "Año para la proyección poblacional (para 2009 se interpoló linealmente a partir de 2005 y 2010)",
    "Identificador numérico de provincia",
    "Identificador categórico de provincia",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Proyección poblacional",
    "Población estándar Censo 2010"
  ),
  tipo_var = map_chr(proy_pob_prov, ~ paste(class(.x), collapse = ", ")),

  niveles = map_chr(
    proy_pob_prov,
    ~ if (is.factor(.x)) {
      paste(levels(.x), collapse = ", ")
    } else {
      "0-Inf"
    }
  )
)

# Guardar datos limpios ---------------------------------------------------
## Proyecciones por año, provincia, sexo y grupo etario decenal
export(proy_pob_prov, file = "datos_limpios/arg_proy_2005_2018_ge10.rds")


## Proyecciones por año, región, sexo y grupo etario decenal
export(proy_pob_reg, file = "datos_limpios/arg_proy_2005_2018_ge10_reg.rds")

## Diccionario de datos
export(
  data_dict,
  file = "datos_limpios/dic_arg_proy_2005_2018.xlsx",
  format_headers = FALSE
)


# Limpiar environment y desactivar paquetes ------------------------------
rm(list = ls())

pacman::p_unload("all")
