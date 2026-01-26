### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
###  en Argentina, período 2005-2018
### Cálculo de AVP, AVD, AVAD y tasas crudas (95% CI) para DM2 en Argentina
### para los periodos correspondientes a las cuatro Encuestas Nacionales de
### Factores de Riesgo (2005, 2009, 2013 y 2018).
### Autoras:
## - Micaela Gauto
## - Tamara Ricardo

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
## Prevalencia DM2 por grupos decenales de edad y provincia ----
prev_dm2_prov <- import("datos_limpios/arg_dm2_ge10_prov.csv")

## Prevalencia DM2 por grupos decenales de edad y región ----
prev_dm2_reg <- import("datos_limpios/arg_dm2_ge10_reg.csv")


## AVP por grupos decenales de edad y provincia ----
AVP_dm2_prov <- import("datos_limpios/arg_avp_ge10_prov.rds")

## AVP por grupos decenales de edad y región ----
AVP_dm2_reg <- import("datos_limpios/arg_avp_ge10_reg.rds")


## Secuelas DM2 (datos temporales 2014) ----
comp_dm2 <- import("datos_limpios/fr_comp_DW_ge10.csv")


# Calcular AVAD -----------------------------------------------------------
## Por provincia, sexo y grupos decenales de edad ----
AVAD_dm2_prov <- prev_dm2_prov |>
  # Añadir defunciones y AVP
  left_join(AVP_dm2_prov) |>

  # Añadir proyecciones poblacionales y población estándar
  left_join(proy_ge10_prov) |>

  # Añadir frecuencias complicaciones y DW
  left_join(
    comp_dm2 |>
      # Calcular promedio ponderado de discapacidad (fwd)
      group_by(sexo, grupo_edad10) |>
      summarise(
        fwd = sum(comp_frec * dw, na.rm = TRUE),
        .groups = "drop"
      )
  ) |>

  # Calcular AVD  y AVAD
  mutate(
    AVD = dm2_total * fwd,
    AVAD = AVP + AVD
  ) |>

  # Calcular tasas AVP, AVD y AVAD x 100.000 hab
  mutate(across(
    .cols = c(AVP, AVD, AVAD),
    .fns = list(
      tasa = ~ round(.x / pob_proy * 100000, 2)
    )
  )) |>

  # Variables caracter a factor
  mutate(across(
    .cols = where(is.character) | c(anio_enfr, codprov_censo),
    .fn = ~ factor(.x)
  )) |>

  # Reorganizar columnas
  select(
    anio_enfr,
    contains("prov"),
    region_deis,
    contains("pob"),
    sexo:dm2_prev_cv_cat,
    defun_n:ex,
    fwd,
    starts_with("AV")
  )


## Por región, sexo y grupos decenales de edad ----
AVAD_dm2_reg <- prev_dm2_reg |>
  # Añadir defunciones y AVP
  left_join(AVP_dm2_reg) |>

  # Añadir proyecciones poblacionales y población estándar
  left_join(proy_ge10_reg) |>

  # Añadir frecuencias complicaciones y DW
  left_join(
    comp_dm2 |>
      # Calcular promedio ponderado de discapacidad (fwd)
      group_by(sexo, grupo_edad10) |>
      summarise(
        fwd = sum(comp_frec * dw, na.rm = TRUE),
        .groups = "drop"
      )
  ) |>

  # Calcular AVD  y AVAD
  mutate(
    AVD = dm2_total * fwd,
    AVAD = AVP + AVD
  ) |>

  # Calcular tasas AVP, AVD y AVAD x 100.000 hab
  mutate(across(
    .cols = c(AVP, AVD, AVAD),
    .fns = list(
      tasa = ~ round(.x / pob_proy * 100000, 2)
    )
  )) |>

  # Variables caracter a factor
  mutate(across(.cols = where(is.character), .fn = ~ factor(.x))) |>

  # Reorganizar columnas
  select(
    anio_enfr,
    region_deis,
    contains("pob"),
    sexo:dm2_prev_cv_cat,
    defun_n:ex,
    fwd,
    starts_with("AV")
  )


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = names(AVAD_dm2_prov),

  descripcion = c(
    "Año de realización de la Encuesta Nacional de Factores de Riesgo (ENFR)",
    "Identificador numérico de provincia según clasificación INDEC",
    "Identificador categórico de provincia",
    "Región geográfica según clasificación DEIS (2021)",
    "Proyección poblacional para el año correspondiente",
    "Población estándar para Argentina según Censo Nacional 2010",
    "Sexo biológico",
    "Grupo de edad decenal",
    "Total estimado de personas con diabetes mellitus por provincia, edad y sexo",
    "Error estándar del total estimado de personas con diabetes mellitus por provincia, edad y sexo",
    "Total estimado de personas con diabetes mellitus tipo 2 por provincia, edad y sexo",
    "Error estándar del total estimado de personas con diabetes mellitus tipo 2 por provincia, edad y sexo",
    "Prevalencia de diabetes mellitus tipo 2 por autorreporte",
    "Error estándar del total de la prevalencia de personas con DM2",
    "Coeficiente de variación de la prevalencia de personas con DM2",
    "Categorización del coeficiente de variación de la prevalencia de personas con DM2",
    "Número de defunciones para el trienio correspondiente",
    "Promedio de defunciones para el trienio correspondiente",
    "Cantidad de personas vivas a la edad x",
    "Años-persona vividos por encima de la edad x",
    "Esperanza de vida a la edad x",
    "Promedio ponderado de discapacidad por DM2",
    "Años de vida perdidos por muerte prematura por DM2",
    "Años vividos con discapacidad por DM2",
    "Años de vida ajustados por discapacidad por DM2",
    "Tasa de AVP por 100.000 habitantes",
    "Tasa de AVD por 100.000 habitantes",
    "Tasa de AVAD por 100.000 habitantes"
  ),

  tipo_var = map_chr(AVAD_dm2_prov, ~ paste(class(.x), collapse = ", ")),

  niveles = map_chr(
    AVAD_dm2_prov,
    ~ if (is.factor(.x)) {
      paste(levels(.x), collapse = ", ")
    } else {
      "0-Inf"
    }
  )
)


# Guardar datos limpios --------------------------------------------------
## AVAD por provincia
export(AVAD_dm2_prov, file = "datos_limpios/AVAD_dm2_ge10_prov.xlsx")

## AVAD por región
export(AVAD_dm2_reg, file = "datos_limpios/AVAD_dm2_ge10_reg.xlsx")

## Diccionario de datos
export(
  data_dict,
  file = "datos_limpios/diccionario_datos_AVAD_dm2_ge10_.xlsx"
)


# Limpiar environment y desactivar paquetes ------------------------------
rm(list = ls())

pacman::p_unload("all")
