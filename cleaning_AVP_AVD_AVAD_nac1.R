### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
###  en Argentina, período 2005-2018
### Limpieza y procesamiento de los datasets:
## - INDEC - Censo Nacional 2001 y 2010: Proyecciones poblacionales por
##  sexo y grupo etario quinquenal para los años 2001, 2005, 2010, 2013 y 2018.
## - INDEC - Encuesta Nacional de Factores de Riesgo (ENFR): Autorreporte de diabetes
## mellitus (DM) por sexo y edad para los años 2005, 2009, 2013 y 2018.
## - MSAL - DEIS: Defunciones ocurridas y registradas en Argentina por sexo
## y grupo etario quinquenal para el periodo 2004-2019. Se consideró DM2 como causa de
## muerte para los códigos E11 y E14 de la CIE-10.
## - WHO - GHO: Tablas de vida para Argentina por sexo y grupo etario quinquenal para
## el año 2019.
### Cálculo de AVP, AVD y AVAD e intervalos de incertidumbre mediante cadenas de
### Monte-Carlo con 10.000 réplicas. Se usaron las siguientes simulaciones:
## - Defunciones: distribución normal truncada en cero, con media igual al
## valor estimado y SD aproximada por sqrt(mu/3).
## - Prevalencia DM2: se simularon con una normal truncada en [0,1], con media
## igual a la estimación puntual y desviación estándar igual a su error estándar.
## - Frecuencia de complicaciones asociadas a DM2: Frecuencia de complicaciones microvasculares
## y macrovasculares por sexo y grupo etario según registros de la Red Qualidiab.
## Se incluyen aquellas complicaciones consensuadas con la Red.
## - Pesos de discapacidad: se consideraron los publicados por el Global Burden Disease.
### Cálculo de tasas estandarizadas AVP, AVD y AVAD e intervalos de incertidumbre
### mediante cadenas de Monte-Carlo con 10.000 réplicas.
### Autoras:
## - Micaela Gauto
## - Tamara Ricardo
### Fecha de creación: 27-01-2026

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Grupos etarios
  epikit,
  # Diseño muestral y prevalencia
  srvyr,
  # Simulaciones de Monte-Carlo
  truncnorm,
  # Manejo de datos
  rio,
  janitor,
  tidyverse,
  readxl
)


# Cargar funciones auxiliares --------------------------------------------
source("fun_auxiliares.R")

# Cargar datos AVP -------------------------------------------------------
## Tabla de vida Argentina (2019) -----
ex_ge10 <- read_csv2(
  "bases_datos/argentina_tabla de vida_GHO.csv",
  skip = 1
)

## Defunciones 2004 -----
def04 <- import("bases_datos/DEIS/DE_2004.csv")

## Defunciones 2005-2019 -----
def05_19 <- list.files(
  path = "bases_datos/DEIS/",
  pattern = "^defweb.",
  full.names = TRUE
) |>

  # Referenciar lista de archivos
  (\(x) {
    set_names(x, nm = paste0("20", str_sub(x, 24, 25)))
  })() |>

  # Leer archivos csv
  map(read_csv, locale = locale(encoding = "WINDOWS-1252")) |>

  # Unir datasets
  list_rbind(names_to = "anio")


# Cargar datos AVD -------------------------------------------------------
## Complicaciones DM2 por sexo, grupo etario y año -----
comp_dm2_raw <- import("datos_limpios/fr_comp_DW_ge10.csv")

## ENFR 2005 -----
enfr05 <- read_delim(
  "bases_datos/ENFR/ENFR 2005 - Base usuario.txt",
  col_select = c(
    id = IDENTIFI,
    sexo = CHCH04,
    edad = CHCH05,
    dm_auto = CIDI01,
    dm_g = CIDI02,
    wt = PONDERACION
  )
)

## ENFR 2009 -----
enfr09 <- read_delim(
  "bases_datos/ENFR/ENFR 2009 - Base usuario.txt",
  col_select = c(
    id = IDENTIFI,
    sexo = BHCH04,
    edad = BHCH05,
    dm_auto = BIDI01,
    dm_g = BIDI02,
    wt = PONDERACION
  )
)

## ENFR 2013 -----
enfr13 <- read_delim(
  "bases_datos/ENFR/ENFR 2013 - Base usuario.txt",
  col_select = c(
    ID,
    sexo = BHCH04,
    edad = BHCH05,
    dm_auto = BIDI01,
    dm_g = BIDI02,
    wt = PONDERACION
  )
)

## ENFR 2018 -----
enfr18 <- read_delim(
  "bases_datos/ENFR/ENFR 2018 - Base usuario.txt",
  col_select = c(
    id,
    sexo = bhch03,
    edad = bhch04,
    dm_auto = bidi01,
    dm_g = bidi02,
    wf1p
  )
) |>
  # Añadir base de réplicas
  left_join(read_delim(
    "bases_datos/ENFR/ENFR2018_base_rep_filter.csv"
  ))


# Cargar datos INDEC -----------------------------------------------------
## Proyecciones poblacionales 2001-2018 ----
proy_pob <- import("datos_limpios/arg_proy_pob.rds")

## Población estándar Censo 2010
pob_est_2010 <- import("datos_limpios/pob_est_2010.rds")


# Limpiar datos AVP ------------------------------------------------------
## Esperanza de vida -----------------------------------------------------
ex_ge10 <- ex_ge10 |>
  # Seleccionar columnas
  select(
    ind = 1,
    grupo_edad = 2,
    "Varón" = 4,
    "Mujer" = 5
  ) |>

  # Filtrar menores de 30 años y totales
  filter(
    between(grupo_edad, "30-34 years", "45-49 years") |
      between(grupo_edad, "50-54 years", "85+ years")
  ) |>

  # Cambiar etiquetas indicadores
  mutate(ind = str_extract(ind, '^[^ ]+')) |>

  # Pasar a formato long
  pivot_longer(cols = c(Varón, Mujer), names_to = "sexo") |>

  # Volver a formato wide
  pivot_wider(names_from = ind, values_from = value) |>

  # Crear grupo etario decenal
  mutate(
    grupo_edad_10 = case_when(
      between(grupo_edad, "30-34 years", "35-39 years") ~ "30 a 39",
      between(grupo_edad, "40-44 years", "45-49 years") ~ "40 a 49",
      between(grupo_edad, "50-54 years", "55-59 years") ~ "50 a 59",
      between(grupo_edad, "60-64 years", "65-69 years") ~ "60 a 69",
      between(grupo_edad, "70-74 years", "75-79 years") ~ "70 a 79",
      .default = "80+"
    )
  ) |>

  # Recalcular indicadores por grupo decenal
  group_by(sexo, grupo_edad_10) |>
  summarise(
    lx = first(lx),
    nLx = sum(nLx, na.rm = TRUE),
    ndx = sum(ndx, na.rm = TRUE),
    nMx = sum(nMx * nLx, na.rm = TRUE) / sum(nLx, na.rm = TRUE),
    nqx = sum(nqx * nLx, na.rm = TRUE) / sum(nLx, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Calcular Tx y ex
  mutate(
    Tx = rev(cumsum(rev(nLx))),
    ex = Tx / lx,
    .by = sexo
  )


## Defunciones 2004-2019 -------------------------------------------------
defun_dm2 <- def04 |>
  # Estandarizar nombres de columnas
  clean_names() |>
  rename(
    prov_nombre = jurisdiccion,
    grupo_edad = grupo_de_edad,
    cie10_causa = causa_de_muerte_cie_10
  ) |>

  ### Añadir defunciones 2005-2019 ###
  bind_rows(
    def05_19 |>
      # Estandarizar nombres de columnas
      clean_names() |>
      rename(
        codprov_censo = provres,
        grupo_edad = grupedad,
        cie10_causa = causa,
        total = cuenta
      ) |>

      # Sexo a caracter
      mutate(sexo = as.character(sexo))
  ) |>

  # Filtrar muertes por DM2 (E11 y E14)
  filter(cie10_causa %in% c("E11", "E14")) |>

  # Filtrar datos de otro país
  filter_out(prov_nombre == "Otro país" | codprov_censo == "98") |>

  # Filtrar valores ausentes sexo
  filter_out(sexo %in% c("Desconocido", "Indeterminado", "9")) |>

  # Filtrar datos ausentes edad
  filter_out(str_detect(grupo_edad, "esp")) |>

  # Completar datos faltantes año
  mutate(anio = replace_na(anio, "2004")) |>

  # Cambiar etiquetas sexo
  mutate(sexo = fct_recode(sexo, Varón = "1", Mujer = "2")) |>

  # Modificar etiquetas grupo edad
  mutate(grupo_edad = str_sub(grupo_edad, 4)) |>

  # Crear grupo etario decenal
  mutate(
    grupo_edad_10 = case_when(
      between(grupo_edad, "30 a 34", "35 a 39") ~ "30 a 39",
      between(grupo_edad, "40 a 44", "45 a 49") ~ "40 a 49",
      between(grupo_edad, "50 a 54", "55 a 59") ~ "50 a 59",
      between(grupo_edad, "60 a 64", "65 a 69") ~ "60 a 69",
      between(grupo_edad, "70 a 74", "75 a 79") ~ "70 a 79",
      between(grupo_edad, "80 a 84", "85 y más") ~ "80+",
      .default = NA
    )
  ) |>

  # Filtrar menores de 30 años
  filter_out(is.na(grupo_edad_10)) |>

  # Crear columna para año ENFR
  mutate(
    anio_enfr = case_when(
      between(anio, "2004", "2006") ~ "2005",
      between(anio, "2008", "2010") ~ "2009",
      between(anio, "2012", "2014") ~ "2013",
      between(anio, "2017", "2019") ~ "2018"
    )
  ) |>

  # Defunciones por sexo y grupo edad decenal
  count(
    anio_enfr,
    grupo_edad_10,
    sexo,
    wt = total,
    name = "defun_n",
    .drop = FALSE
  ) |>

  # Calcular media y SE
  mutate(
    defun_mean = defun_n / 3,
    defun_se = sqrt(defun_mean / 3),
    .by = c(anio_enfr, grupo_edad_10, sexo)
  )


# Limpiar datos AVD ------------------------------------------------------
## Complicaciones DM2 ----------------------------------------------------
comp_dm2 <- comp_dm2_raw |>
  # Filtrar menores de 30 años
  filter(between(grupo_edad_10, "30 a 39", "80+")) |>

  # Reemplazar NAs tipo complicación
  mutate(comp_tipo = na_if(comp_tipo, "")) |>

  # Crear variable año ENFR
  mutate(anio_enfr = as.character(anio)) |>

  # Crear variable complicacion:tipo
  mutate(
    comp_qd_tipo = if_else(
      !is.na(comp_tipo),
      paste(comp_tipo, comp_qualidiab, sep = "_"),
      comp_qualidiab
    )
  ) |>

  # Calcular promedio ponderado de discapacidad (fwd)
  group_by(anio_enfr, sexo, grupo_edad_10, comp_qd_tipo) |>
  summarise(
    fwd = sum(comp_frec * dw, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Niveles complicaciones a formato tidy
  mutate(
    comp_qd_tipo = make_clean_names(comp_qd_tipo, allow_dupes = TRUE) |>
      str_remove_all("vascular|atia|iferativa|funcion|_miembros_inferiores")
  ) |>

  # Pasar a formato wide
  pivot_wider(
    names_from = comp_qd_tipo,
    values_from = fwd,
    names_glue = "{comp_qd_tipo}_{.value}"
  ) |>

  # Crear columnas de totales micro y macro
  mutate(
    total_micro_fwd = rowSums(pick(starts_with("micro")), na.rm = TRUE),
    total_macro_fwd = rowSums(pick(starts_with("macro")), na.rm = TRUE),
    total_comp_fwd = total_micro_fwd + total_macro_fwd,
  )


# Prevalencia DM2 --------------------------------------------------------
prev_dm2 <- list(
  "2005" = enfr05,
  "2009" = enfr09,
  "2013" = enfr13,
  "2018" = enfr18
) |>
  map(\(x) {
    x |>
      # Aplicar función de limpieza
      clean_enfr() |>
      # Calcular total personas con DM y prevalencia
      group_by(sexo, grupo_edad_10) |>
      summarise(
        dm_total = survey_total(dm_auto),
        dm2_total = survey_total(dm2_auto),
        dm2_prev = survey_mean(
          dm2_auto,
          vartype = c("ci", "cv"),
          na.rm = TRUE
        ),
        .groups = "drop"
      )
  }) |>
  bind_rows(.id = "anio_enfr")


# Combinar datos AVP y AVD -----------------------------------------------
datos_dm2 <- prev_dm2 |>
  # Añadir datos defunciones
  left_join(defun_dm2) |>

  # Añadir esperanza de vida
  left_join(ex_ge10) |>

  # Añadir proyecciones poblacionales
  left_join(proy_pob) |>

  # Añadir pesos discapacidad complicaciones
  left_join(comp_dm2)


## Guardar datos limpios
export(datos_dm2, file = "datos_limpios/arg_datos_dm2.rds")

# Simular AVP, AVD y AVAD totales ----------------------------------------
set.seed(123)
sim_avad <- datos_dm2 |>
  # Seleccionar columnas
  select(
    anio_enfr,
    sexo,
    grupo_edad_10,
    dm2_total,
    dm2_total_se,
    defun_mean,
    defun_se,
    ex,
    proy_pob,
    total_comp_fwd
  ) |>

  # Monte Carlo fila por fila
  mutate(
    sims = pmap(
      list(
        dm2_total,
        dm2_total_se,
        total_comp_fwd,
        defun_mean,
        defun_se,
        ex,
        proy_pob
      ),
      sim_AVAD
    )
  ) |>

  # Expandir resultados
  unnest_wider(sims) |>

  # Añadir población estándar 2010
  left_join(pob_est_2010)


# Simular AVP, AVD y AVAD microvascular ----------------------------------
set.seed(123)

sim_avad_micro <- datos_dm2 |>
  # Seleccionar columnas
  select(
    anio_enfr,
    sexo,
    grupo_edad_10,
    dm2_total,
    dm2_total_se,
    defun_mean,
    defun_se,
    ex,
    proy_pob,
    total_micro_fwd
  ) |>

  # Monte Carlo fila por fila
  mutate(
    sims = pmap(
      list(
        dm2_total,
        dm2_total_se,
        total_micro_fwd,
        defun_mean,
        defun_se,
        ex,
        proy_pob
      ),
      sim_AVAD
    )
  ) |>

  # Expandir resultados
  unnest_wider(sims) |>

  # Añadir población estándar 2010
  left_join(pob_est_2010)


# Simular AVP, AVD y AVAD macrovascular ----------------------------------
set.seed(123)

sim_avad_macro <- datos_dm2 |>
  # Seleccionar columnas
  select(
    anio_enfr,
    sexo,
    grupo_edad_10,
    dm2_total,
    dm2_total_se,
    defun_mean,
    defun_se,
    ex,
    proy_pob,
    total_macro_fwd
  ) |>

  # Monte Carlo fila por fila
  mutate(
    sims = pmap(
      list(
        dm2_total,
        dm2_total_se,
        total_macro_fwd,
        defun_mean,
        defun_se,
        ex,
        proy_pob
      ),
      sim_AVAD
    )
  ) |>

  # Expandir resultados
  unnest_wider(sims) |>

  # Añadir población estándar 2010
  left_join(pob_est_2010)
