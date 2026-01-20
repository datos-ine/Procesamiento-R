### Cálculo de prevalencia de DM a partir de los datos publicados de las ENFR,
### considerando la prevalencia de DM o glucemia elevada por autorreporte y
### grupos decenales de edad.
### Se suma el cálculo de prevalencias por región y corrección del 90% sobre el
### total para obtener la prevalencia de DM2.
### Autoras: Tamara Ricardo y Micaela Gauto
### Fecha creación: # 2025-10-22 13:12:27
# Última modificación: 20-01-2026 13:21

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  srvyr,
  epikit,
  janitor,
  tidyverse,
  geoAr
)


# Cargar datos crudos -----------------------------------------------------
## ENFR 2005 ----
enfr05_raw <- read_delim("raw/ENFR 2005 - Base usuario.txt")


## ENFR 2009 ----
enfr09_raw <- read_delim("raw/ENFR 2009 - Base usuario.txt")


## ENFR 2013 ----
enfr13_raw <- import("raw/ENFR 2013 - Base usuario.txt")


## ENFR 2018 ----
enfr18_raw <- read_delim("raw/ENFR 2018 - Base usuario.txt")

# Réplicas ENFR 2018
enfr18_rep <- read_delim("raw/ENFR2018_base_rep_filter.csv")


# Función para limpiar datos ----------------------------------------------
clean_enfr <- function(x) {
  x |>
    # Filtrar menores de 30 años
    filter(edad >= 30) |>

    # Cambiar formato id de provincia
    mutate(
      codprov_censo = if_else(
        codprov_censo %in% c(2, 6),
        paste0("0", codprov_censo),
        as.character(codprov_censo)
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

    # Crear grupo de edad decenal
    mutate(
      grupo_edad10 = age_categories(
        edad,
        lower = 30,
        upper = 80,
        by = 10,
        separator = " a "
      )
    ) |>

    # Cambiar etiquetas sexo
    mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) |>

    # Convertir dm_auto a binomial
    mutate(
      dm_auto = if_else(dm_auto == 1, 1, 0),

      # Calcular frecuencias DM2
      dm2_auto = dm_auto * 0.9
    )
}


# Limpiar datos ----------------------------------------------------------
## ENFR 2005 ----
enfr05 <- enfr05_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    id = identifi,
    codprov_censo = prov,
    sexo = chch04,
    edad = chch05,
    dm_auto = cidi01,
    ponderacion
  ) |>

  # Aplicar función de limpieza
  clean_enfr()


## ENFR 2009 ----
enfr09 <- enfr09_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    id = identifi,
    codprov_censo = prvnc,
    sexo = bhch04,
    edad = bhch05,
    dm_auto = bidi01,
    ponderacion
  ) |>

  # Aplicar función de limpieza
  clean_enfr()


## ENFR 2013 ----
enfr13 <- enfr13_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    id,
    codprov_censo = cod_provincia,
    sexo = bhch04,
    edad = bhch05,
    dm_auto = bidi01,
    ponderacion
  ) |>

  # Aplicar función de limpieza
  clean_enfr()


## ENFR 2018 ----
enfr18 <- enfr18_raw |>
  # Estandarizar nombres columnas
  clean_names() |>

  # Seleccionar columnas
  select(
    id,
    codprov_censo = cod_provincia,
    sexo = bhch03,
    edad = bhch04,
    dm_auto = bidi01,
    wf1p
  ) |>

  # Aplicar función de limpieza
  clean_enfr() |>

  # Añadir réplicas
  left_join(enfr18_rep)


# Explorar datos ---------------------------------------------------------
# Frecuencias x sexo
tabyl(enfr05$sexo)

tabyl(enfr09$sexo)

tabyl(enfr13$sexo)

tabyl(enfr18$sexo)

# Frecuencias x grupo etario decenal
tabyl(enfr05$grupo_edad10)

tabyl(enfr09$grupo_edad10)

tabyl(enfr13$grupo_edad10)

tabyl(enfr18$grupo_edad10)

# Frecuencias x presencia DM
tabyl(enfr05$dm_auto)

tabyl(enfr09$dm_auto)

tabyl(enfr13$dm_auto)

tabyl(enfr18$dm_auto)


# Frecuencias x presencia DM2
tabyl(enfr05$dm2_auto)

tabyl(enfr09$dm2_auto)

tabyl(enfr13$dm2_auto)

tabyl(enfr18$dm2_auto)


# Prevalencias por provincia, sexo y grupo edad decenal ------------------
## ENFR 2005 ----
enfr05_ge10_prov <- enfr05 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(codprov_censo, region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2009 ----
enfr09_ge10_prov <- enfr09 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(codprov_censo, region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2013 ----
enfr13_ge10_prov <- enfr13 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(codprov_censo, region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2018 (warning) ----
enfr18_ge10_prov <- enfr18 |>
  # Crear objeto diseño
  as_survey_rep(
    weights = wf1p,
    repweights = starts_with("wf1p"),
    type = "bootstrap"
  ) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(codprov_censo, region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## Unir datasets ----
enfr_ge10_prov <- bind_rows(
  enfr05_ge10_prov,
  enfr09_ge10_prov,
  enfr13_ge10_prov,
  enfr18_ge10_prov,
  .id = "anio_enfr"
) |>

  # Añadir etiquetas año ENFR
  mutate(
    anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
  ) |>

  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2))) |>

  # Variables categóricas a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  # Categorizar coeficiente de variación
  mutate(
    dm2_prev_cv_cat = cut(
      dm2_prev_cv,
      breaks = c(-Inf, .1, .2, .3, Inf),
      labels = c("Baja", "Moderada", "Alta", "Muy alta")
    )
  )


# Prevalencias por región DEIS, sexo y grupo edad decenal ----------------
## ENFR 2005 ----
enfr05_ge10_reg <- enfr05 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2009 ----
enfr09_ge10_reg <- enfr09 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2013 ----
enfr13_ge10_reg <- enfr13 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2018 (warning) ----
enfr18_ge10_reg <- enfr18 |>
  # Crear objeto diseño
  as_survey_rep(
    weights = wf1p,
    repweights = starts_with("wf1p"),
    type = "bootstrap"
  ) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## Unir datasets ----
enfr_ge10_reg <- bind_rows(
  enfr05_ge10_reg,
  enfr09_ge10_reg,
  enfr13_ge10_reg,
  enfr18_ge10_reg,
  .id = "anio_enfr"
) |>

  # Añadir etiquetas año ENFR
  mutate(
    anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
  ) |>

  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2))) |>

  # Variables categóricas a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  # Categorizar coeficiente de variación
  mutate(
    dm2_prev_cv_cat = cut(
      dm2_prev_cv,
      breaks = c(-Inf, .1, .2, .3, Inf),
      labels = c("Baja", "Moderada", "Alta", "Muy alta")
    )
  )


  # Diccionario de datos ----------------------------------------------------
  data_dict <- tibble(
    names(enfr_ge10_prov),

    descripcion = c(
      "Año de realización ENFR",
      "Identificador numérico de provincia según clasificación INDEC",
      "Región geográfica según clasificación DEIS (2021)",
      "Sexo biológico",
      "Grupo de edad decenal",
      "Total estimado de personas con diabetes mellitus por provincia, edad y sexo",
      "Error estándar del total estimado de personas con diabetes mellitus por provincia, edad y sexo",
      "Total estimado de personas con diabetes mellitus tipo 2 por provincia, edad y sexo",
      "Error estándar del total estimado de personas con diabetes mellitus tipo 2 por provincia, edad y sexo",
      "Prevalencia de diabetes mellitus tipo 2 por autorreporte",
      "Error estándar del total de la prevalencia de personas con DM2",
      "Coeficiente de variación de la prevalencia de personas con DM2",
      "Categorización del coeficiente de variación de la prevalencia de personas con DM"
    ),

    tipo_var = map_chr(enfr_ge10_prov, ~ paste(class(.x), collapse = ", ")),

    niveles = map_chr(
      enfr_ge10_prov,
      ~ if (is.factor(.x)) {
        paste(levels(.x), collapse = ", ")
      } else {
        "O-Inf"
      }
    )
  )


# Guardar datos limpios ---------------------------------------------------
## Grupos etarios decenales (30+ años) y provincia
export(enfr_ge10_prov, file = "clean/arg_prev_dm2_ge10_prov.rds")

## Grupos etarios decenales (30+ años) y región
export(enfr_ge10_reg, file = "clean/arg_prev_dm2_ge10_reg.rds")

## Diccionario de datos
export(data_dict, file = "clean/dic_arg_prev_dm2.xlsx")


# Limpiar environment y desactivar paquetes ------------------------------
rm(list = ls())

pacman::p_unload("all")
