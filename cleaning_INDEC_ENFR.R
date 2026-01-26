### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
###  en Argentina, período 2005-2018
### Limpieza y procesamiento de los datasets:
## - Resultados de las Encuestas Nacionales de Factores de Riesgo (ENFR) 2005-2018.
## - Proyecciones poblacionales de INDEC para los años 2001-2021 según sexo, grupo
## de edad decenal, provincia y/o región geográfica.
### Cálculo de la población estándar de Argentina para el Censo Nacional 2010
### Cálculo de la población por sexo, grupo decenal de edad, provincia y/o región
### geográfica para el año 2009 mediante el método de interpolación lineal.
### Cálculo de la prevalencia de DM o glucemia elevada por autorreporte según sexo,
### grupo de edad decenal, provincia y/o región geográfica.
### Corrección de las frecuencias de DM por autorreporte para obtener frecuencias y
### prevalencia DMW (~90% de los casos reportados)
### Autoras:
## - Micaela Gauto
## - Tamara Ricardo
# Última modificación: 26-01-2026 09:02

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  srvyr,
  epikit,
  geoAr,
  tabulapdf,
  janitor,
  tidyverse,
  readxl
)


# Cargar datos -----------------------------------------------------------
# Códigos de provincias ----
prov <- show_arg_codes() |>
  # Filtrar totales país
  filter(between(codprov_censo, "02", "94")) |>

  # Cambiar etiqueta CABA
  mutate(prov_nombre = if_else(codprov_censo == "02", id, name_iso)) |>

  # Crear región geográfica DEIS
  mutate(
    region_deis = case_when(
      codprov_censo %in% c("02", "06", "14", "30", "82") ~ "Centro",
      codprov_censo %in% c("18", "22", "34", "54") ~ "NEA",
      codprov_censo %in% c("38", "66") ~ "NOA1",
      codprov_censo %in% c("10", "86", "90") ~ "NOA2",
      codprov_censo %in% c("46", "50", "70", "74") ~ "Cuyo",
      .default = "Patagonia"
    )
  )


# Proyecciones poblacionales INDEC ----
## Proyecciones 2001-2005
proy_01_05_raw <- extract_areas(
  file = "bases_datos/INDEC_proyec 2001-2015.pdf",
  pages = c(22:24, 27:28, 25:26, 29:43, 45, 44)
)

## Proyecciones 2010, 2013 y 2018
proy_10_18_raw <- {
  leer_filas <- function(rango) {
    excel_sheets("bases_datos/c2_proyecciones_prov_2010_2040.xls")[
      -c(1:2)
    ] |>
      set_names() |>
      map(
        ~ read_excel(
          "bases_datos/c2_proyecciones_prov_2010_2040.xls",
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


# Resultados ENFR ----
## ENFR 2005
enfr05 <- read_delim("bases_datos/ENFR/ENFR 2005 - Base usuario.txt")

# ENFR 2009
enfr09 <- read_delim("bases_datos/ENFR/ENFR 2009 - Base usuario.txt")

# ENFR 2013
enfr13 <- import("bases_datos/ENFR/ENFR 2013 - Base usuario.txt")

# ENFR 2018
enfr18 <- read_delim("bases_datos/ENFR/ENFR 2018 - Base usuario.txt") |>
  # Añadir base de réplicas
  left_join(read_delim("bases_datos/ENFR/ENFR2018_base_rep_filter.csv"))


# Limpiar datasets proyecciones poblacionales ----------------------------
proy_pob_prov <- bind_rows(
  ## Limpiar proyecciones 2001 y 2005 ##
  proy_01_05_raw |>
    # Asignar identificador numérico de provincia
    set_names(unique(prov$codprov_censo)) |>

    # Unir tablas de provincias
    list_rbind(names_to = "codprov_censo") |>

    # Estandarizar nombres de columnas
    clean_names() |>

    # Seleccionar columnas relevantes
    select(
      codprov_censo,
      grupo_edad = x1,
      Varón_2001 = x2001,
      Mujer_2001 = x4,
      Varón_2005 = x2005,
      Mujer_2005 = x7
    ) |>

    # Filtrar menores de 30 años
    filter(
      between(grupo_edad, "30-34", "45-49") |
        between(grupo_edad, "50-54", "80 y más")
    ) |>

    # Pasar a formato long
    pivot_longer(cols = c(Varón_2001:Mujer_2005)) |>

    # Separar sexo y año
    separate_wider_delim(
      name,
      delim = "_",
      names = c("sexo", "anio_enfr")
    ) |>

    # Proyección poblacional a numérico
    mutate(
      value = parse_number(
        value,
        locale = locale(decimal_mark = ",")
      )
    ),

  ## Limpiar proyecciones 2010, 2013 y 2018 ##
  proy_10_18_raw |>
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

    # Filtrar menores de 30 años
    filter(
      between(grupo_edad, "30-34", "45-49") |
        between(grupo_edad, "50-54", "95-99") |
        grupo_edad == "100 y más"
    ) |>

    # Modificar identificador numérico de provincia
    mutate(codprov_censo = str_sub(codprov_censo, 1, 2)) |>

    # Pasar a formato long
    pivot_longer(cols = c(Varón_2010:Mujer_2018)) |>

    # Separar sexo y año
    separate_wider_delim(
      name,
      delim = "_",
      names = c("sexo", "anio_enfr")
    ) |>

    # Población a numérico
    mutate(
      value = parse_number(value, locale = locale(decimal_mark = ","))
    ) |>

    # Agrupar datos
    count(
      anio_enfr,
      codprov_censo,
      sexo,
      grupo_edad,
      wt = value,
      name = "value"
    )
) |>

  # Añadir nombres de provincias y regiones DEIS
  left_join(prov) |>

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

  # Reagrupar datos
  count(
    anio_enfr,
    codprov_censo,
    prov_nombre,
    region_deis,
    sexo,
    grupo_edad10,
    wt = value,
    name = "prov_proy_pob"
  ) |>

  # Añadir población estimada para 2009 (interpolación lineal)
  (\(x) {
    bind_rows(
      x,
      x |>
        filter(anio_enfr %in% c("2001", "2010")) |>
        pivot_wider(
          names_from = anio_enfr,
          values_from = prov_proy_pob,
          names_prefix = "pob_"
        ) |>

        mutate(
          anio_enfr = "2009",
          tasa_anual = log(pob_2010 / pob_2001) / 9,
          prov_proy_pob = round(pob_2001 * (1 + tasa_anual * 8))
        )
    )
  })()


# Limpiar datos ENFR -----------------------------------------------------
## Función auxiliar para limpieza de datos ----
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

    # Añadir nombre de provincia y regiones DEIS
    left_join(prov |> select(-id)) |>

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

# Limpiar datos ENFR -----------------------------------------------------
# ENFR 2005 ----
enfr05 <- enfr05 |>
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


# ENFR 2009 ----
enfr09 <- enfr09 |>
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


# ENFR 2013 ----
enfr13 <- enfr13 |>
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


# ENFR 2018 ----
enfr18 <- enfr18 |>
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
  clean_enfr()


# Estimar prevalencias DM y DM2 por provincia ----------------------------
## ENFR 2005 ----
enfr05_prov <- enfr05 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(codprov_censo, prov_nombre, region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2009 ----
enfr09_prov <- enfr09 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(codprov_censo, prov_nombre, region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2013 ----
enfr13_prov <- enfr13 |>
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(codprov_censo, prov_nombre, region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


## ENFR 2018 (warning) ----
enfr18_prov <- enfr18 |>
  # Crear objeto diseño
  as_survey_rep(
    weights = wf1p,
    repweights = starts_with("wf1p"),
    type = "bootstrap"
  ) |>

  # Estimar cantidad de personas con DM y prevalencia
  group_by(codprov_censo, prov_nombre, region_deis, sexo, grupo_edad10) |>
  summarise(
    dm_total = survey_total(dm_auto),
    dm2_total = survey_total(dm2_auto),
    dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv")),
    .groups = "drop"
  )


# Estimar prevalencias DM y DM2 por región DEIS --------------------------
## ENFR 2005 ----
enfr05_reg <- enfr05 |>
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
enfr09_reg <- enfr09 |>
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
enfr13_reg <- enfr13 |>
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
enfr18_reg <- enfr18 |>
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


# Combinar datasets DM y proyecciones poblacionales ----------------------
## Por provincia, sexo y grupo etario decenal ----
enfr_prov <- bind_rows(
  # Unir estimaciones prevalencia
  enfr05_prov,
  enfr09_prov,
  enfr13_prov,
  enfr18_prov,
  .id = "anio_enfr"
) |>

  # Añadir etiquetas año ENFR
  mutate(
    anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
  ) |>

  left_join(proy_pob_prov) |>

  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2))) |>

  # Variables categóricas a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  # Reordenar columnas
  select(anio_enfr, contains("prov"), region_deis:dm2_prev_cv)


## Por región DEIS, sexo y grupo etario decenal ----
enfr_reg <- bind_rows(
  # Unir estimaciones prevalencia
  enfr05_reg,
  enfr09_reg,
  enfr13_reg,
  enfr18_reg,
  .id = "anio_enfr"
) |>

  # Añadir etiquetas año ENFR
  mutate(
    anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
  ) |>

  # Añadir proyecciones poblacionales por región
  left_join(
    proy_pob_prov |>
      count(
        anio_enfr,
        region_deis,
        sexo,
        grupo_edad10,
        wt = prov_proy_pob,
        name = "reg_proy_pob"
      )
  ) |>

  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2))) |>

  # Variables categóricas a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  # Reordenar columnas
  select(anio_enfr, contains("reg"), sexo:dm2_prev_cv)


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = names(enfr_prov),

  descripcion = c(
    "Año de realización de la ENFR",
    "Identificador numérico de provincia según clasificación INDEC",
    "Identificador categórico de la provincia",
    "Proyección poblacional para el año de la ENFR",
    "Región geográfica según clasificación DEIS (2021)",
    "Sexo biológico",
    "Grupo de edad decenal",
    "Total estimado de personas con DM por autorreporte por provincia, grupo etario y sexo",
    "Error estándar del total estimado de personas con DM por provincia, grupo etario y sexo",
    "Total estimado de personas con DM2 por provincia, grupo etario y sexo",
    "Error estándar del total estimado de personas con DM2 por provincia, grupo etario y sexo",
    "Prevalencia de DM2 por provincia, grupo etario y sexo",
    "Error estándar del total de la prevalencia de personas con DM2",
    "Coeficiente de variación de la prevalencia de personas con DM2"
  ),

  tipo_var = map_chr(enfr_prov, ~ paste(class(.x), collapse = ", ")),

  niveles = map_chr(
    enfr_prov,
    ~ if (is.factor(.x)) {
      paste(levels(.x), collapse = ", ")
    } else {
      "0-Inf"
    }
  )
)


# Guardar datos limpios ---------------------------------------------------
## Grupos etarios decenales (30+ años) y provincia
export(enfr_prov, file = "datos_limpios/arg_dm2_ge10_prov.rds")

## Grupos etarios decenales (30+ años) y región
export(enfr_reg, file = "datos_limpios/arg_dm2_ge10_reg.rds")

## Diccionario de datos
export(data_dict, file = "datos_limpios/dic_arg_dm2_ge10.xlsx")


# Limpiar environment y desactivar paquetes ------------------------------
rm(list = ls())

pacman::p_unload("all")
