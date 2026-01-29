### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
###  en Argentina, período 2005-2018
### Limpieza y procesamiento de los datasets:
## - INDEC - Censo Nacional 2001 y 2010: Proyecciones poblacionales por provincia,
##  sexo y grupo etario quinquenal para los años 2001, 2005, 2010, 2013 y 2018.
## - INDEC - Encuesta Nacional de Factores de Riesgo (ENFR): Autorreporte de diabetes
## mellitus (DM) por provincia, sexo y edad para los años 2005, 2009, 2013 y 2018.
## - MSAL - DEIS: Defunciones ocurridas y registradas en Argentina por provincia, sexo
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
## - Pesos de discapacidad: se consideraron fijos (Qualidiab 2014).
### Autoras:
## - Micaela Gauto
## - Tamara Ricardo
### Fecha de creación: 27-01-2026

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Grupos etarios
  epikit,
  # Etiquetas geográficas
  geoAr,
  # Extraer datos de PDF
  tabulapdf,
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


# Cargar datos -----------------------------------------------------------
## Proyecciones poblacionales 2001 y 2005 ----
proy_01_05_raw <- extract_tables(
  file = "bases_datos/INDEC_proyec 2001-2015.pdf",
  pages = c(22:24, 27:28, 25:26, 29:43, 45, 44),
  area = locate_areas(
    file = "bases_datos/INDEC_proyec 2001-2015.pdf",
    pages = 22
  ),
  guess = FALSE
)


## Proyecciones poblacionales 2010, 2013 y 2018 ----
proy_10_18_raw <- bind_cols(
  ## 2010-2015 ##
  excel_sheets("bases_datos/c2_proyecciones_prov_2010_2040.xls")[3:26] |>
    set_names() |>
    map(
      ~ read_excel(
        "bases_datos/c2_proyecciones_prov_2010_2040.xls",
        sheet = .x,
        range = "A3:X28"
      )
    ) |>
    list_rbind(names_to = "prov"),

  ## 2018-2021 ##
  excel_sheets("bases_datos/c2_proyecciones_prov_2010_2040.xls")[3:26] |>
    set_names() |>
    map(
      ~ read_excel(
        "bases_datos/c2_proyecciones_prov_2010_2040.xls",
        sheet = .x,
        range = "A31:X56"
      )
    ) |>
    list_rbind(names_to = "prov")
)


## ENFR 2005 ----
enfr05 <- read_delim(
  "bases_datos/ENFR/ENFR 2005 - Base usuario.txt",
  col_select = c(
    id = IDENTIFI,
    codprov_censo = PROV,
    sexo = CHCH04,
    edad = CHCH05,
    dm_auto = CIDI01,
    wt = PONDERACION
  )
)


## ENFR 2009 ----
enfr09 <- read_delim(
  "bases_datos/ENFR/ENFR 2009 - Base usuario.txt",
  col_select = c(
    id = IDENTIFI,
    codprov_censo = PRVNC,
    sexo = BHCH04,
    edad = BHCH05,
    dm_auto = BIDI01,
    wt = PONDERACION
  )
)


## ENFR 2013 ----
enfr13 <- read_delim(
  "bases_datos/ENFR/ENFR 2013 - Base usuario.txt",
  col_select = c(
    ID,
    codprov_censo = COD_PROVINCIA,
    sexo = BHCH04,
    edad = BHCH05,
    dm_auto = BIDI01,
    wt = PONDERACION
  )
)


## ENFR 2018 ----
enfr18 <- read_delim(
  "bases_datos/ENFR/ENFR 2018 - Base usuario.txt",
  col_select = c(
    id,
    codprov_censo = cod_provincia,
    sexo = bhch03,
    edad = bhch04,
    dm_auto = bidi01,
    wf1p
  )
) |>
  # Añadir base de réplicas
  left_join(read_delim(
    "bases_datos/ENFR/ENFR2018_base_rep_filter.csv"
  ))


## Defunciones 2004 ----
def04_raw <- import("bases_datos/DEIS/DE_2004.csv")


## Defunciones 2005-2019 ----
def05_19_raw <- list.files(
  path = "bases_datos/DEIS/",
  pattern = "^defweb.",
  full.names = TRUE
)


## Población estándar Censo 2010 ----
pob_est_2010 <- import(
  "bases_datos/c2_proyecciones_prov_2010_2040.xls",
  sheet = 2,
  range = "A3:D28"
)


## Tabla de vida Argentina (2019) ----
ex_ge10 <- read_csv2("bases_datos/argentina_tabla de vida_GHO.csv", skip = 1)


## Complicaciones DM2 por sexo y grupo etario (Qualidiab 2014) ----
comp_dm2 <- import("datos_limpios/fr_comp_DW_ge10.csv")


# Crear etiquetas provincias ---------------------------------------------
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
  ) |>

  # Seleccionar columnas
  select(codprov_censo, prov_nombre, region_deis)


# Funciones auxiliares ---------------------------------------------------
## Limpieza de proyecciones poblacionales ----
clean_indec <- function(x) {
  x |>
    # Filtrar menores de 30 años y totales
    filter(
      !grupo_edad %in%
        c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "Edad",
          "Total",
          NA
        )
    ) |>

    # Crear grupo etario decenal
    mutate(
      grupo_edad_10 = case_when(
        between(grupo_edad, "30-34", "35-39") ~ "30 a 39",
        between(grupo_edad, "40-44", "45-49") ~ "40 a 49",
        between(grupo_edad, "50-54", "55-59") ~ "50 a 59",
        between(grupo_edad, "60-64", "65-69") ~ "60 a 69",
        between(grupo_edad, "70-74", "75-79") ~ "70 a 79",
        .default = "80+"
      )
    ) |>

    # Pasar a formato long
    pivot_longer(cols = starts_with(c("Va", "Mu"))) |>

    # Separar sexo y año ENFR
    separate_wider_delim(
      cols = name,
      names = c("sexo", "anio_enfr"),
      delim = "_",
      too_few = "align_start"
    ) |>

    # Proyección poblacional a formato numérico
    mutate(value = parse_number(value, locale = locale(decimal_mark = ",")))
}


## Limpieza de datos ENFR ----
clean_enfr <- function(x) {
  x_clean <- x |>
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

    # Añadir etiquetas provincias y región DEIS
    left_join(prov) |>

    # Crear grupo de edad decenal
    mutate(
      grupo_edad_10 = age_categories(
        edad,
        lower = 30,
        upper = 80,
        by = 10,
        separator = " a "
      )
    ) |>

    # Cambiar etiquetas sexo
    mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) |>

    # Convertir DM a binomial y calcular frecuencia DM2
    mutate(
      dm_auto = if_else(dm_auto == 1, 1, 0),
      dm2_auto = dm_auto * 0.9
    )

  ## Construir diseño muestral ##
  if ("wt" %in% names(x_clean)) {
    x_clean |>
      as_survey_design(weights = wt)
  } else {
    x_clean |>
      as_survey_rep(
        weights = wf1p,
        repweights = starts_with("wf1p"),
        type = "bootstrap"
      )
  }
}


## Simulaciones de Monte-Carlo para AVP, AVD y AVAD ----
sim_IU_DALYs <- function(
  defun_mean,
  defun_se,
  dm2_total,
  dm2_total_se,
  ex,
  fwd,
  # pob,
  nsim = 10000
) {
  # SDs robustos cuando no hay casos / defunciones
  defun_sd <- if_else(defun_mean > 0, defun_se, 1e-6)
  dm2_sd <- if_else(dm2_total > 0, dm2_total_se, 1e-6)

  # Simular defunciones (truncadas en 0)
  defun_sim <- rtruncnorm(
    n = nsim,
    a = 0,
    mean = defun_mean,
    sd = defun_sd
  )

  # AVP
  AVP_sim <- defun_sim * ex

  # Simular total de casos DM2 (truncado en 0)
  dm2_sim <- rtruncnorm(
    n = nsim,
    a = 0,
    mean = dm2_total,
    sd = dm2_sd
  )

  # # Evitar casos mayores a la población del estrato
  # dm2_sim <- pmin(dm2_sim, pob)

  # AVD (enfoque prevalente)
  AVD_sim <- dm2_sim * fwd

  # AVAD
  AVAD_sim <- AVP_sim + AVD_sim

  # Resumen (mediana e IC95%)
  tibble(
    AVP = quantile(AVP_sim, 0.5, na.rm = TRUE),
    AVP_inf = quantile(AVP_sim, 0.025, na.rm = TRUE),
    AVP_sup = quantile(AVP_sim, 0.975, na.rm = TRUE),

    AVD = quantile(AVD_sim, 0.5, na.rm = TRUE),
    AVD_inf = quantile(AVD_sim, 0.025, na.rm = TRUE),
    AVD_sup = quantile(AVD_sim, 0.975, na.rm = TRUE),

    AVAD = quantile(AVAD_sim, 0.5, na.rm = TRUE),
    AVAD_inf = quantile(AVAD_sim, 0.025, na.rm = TRUE),
    AVAD_sup = quantile(AVAD_sim, 0.975, na.rm = TRUE)
  )
}


# Limpiar datos ----------------------------------------------------------
## Esperanza de vida Argentina (2019) ----
ex_ge10 <- ex_ge10 |>
  # Estandarizar nombres de columnas
  clean_names() |>
  select(
    indicator,
    age_group,
    "Varón" = 4,
    "Mujer" = 5
  ) |>

  # Filtrar menores de 30 años y totales
  filter(
    between(age_group, "30-34 years", "45-49 years") |
      between(age_group, "50-54 years", "85+ years")
  ) |>

  # Cambiar etiquetas indicadores
  mutate(indicator = str_extract(indicator, '^[^ ]+')) |>

  # Pasar a formato long
  pivot_longer(cols = c(Varón, Mujer), names_to = "sexo") |>

  # Volver a formato wide
  pivot_wider(names_from = indicator, values_from = value) |>

  # Crear grupo etario decenal
  mutate(
    grupo_edad_10 = case_when(
      between(age_group, "30 a 34", "35 a 39") ~ "30 a 39",
      between(age_group, "40 a 44", "45 a 49") ~ "40 a 49",
      between(age_group, "50 a 54", "55 a 59") ~ "50 a 59",
      between(age_group, "60 a 64", "65 a 69") ~ "60 a 69",
      between(age_group, "70 a 74", "75 a 79") ~ "70 a 79",
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


## Población estándar Censo 2010 ----
pob_est_2010 <- pob_est_2010 |>
  # Seleccionar columnas relevantes
  select(
    grupo_edad = 1,
    Varón = 3,
    Mujer = 4
  ) |>

  # Aplicar función de limpieza
  clean_indec() |>

  # Calcular proyecciones por sexo y grupo etario decenal
  count(sexo, grupo_edad_10, wt = value, name = "pob_est_2010")


## Complicaciones DM2 ----
comp_dm2 <- comp_dm2 |>
  # Calcular promedio ponderado de discapacidad (fwd)
  group_by(sexo, grupo_edad_10) |>
  summarise(
    fwd = sum(comp_frec * dw, na.rm = TRUE),
    .groups = "drop"
  )


## Proyecciones poblacionales ----
proy_pob <- bind_rows(
  ### Proyecciones 2001 y 2005 ##
  proy_01_05_raw |>
    # Asignar identificador numérico de provincia
    set_names(unique(prov$codprov_censo)) |>

    # Unir tablas de provincias
    list_rbind(names_to = "codprov_censo") |>

    # Seleccionar y renombrar columnas
    select(
      codprov_censo,
      grupo_edad = 2,
      Varón_2001 = 4,
      Mujer_2001 = 5,
      Varón_2005 = 7,
      Mujer_2005 = 8
    ) |>

    # Aplicar función de limpieza
    clean_indec(),

  ## Proyecciones 2010, 2013 y 2018 ##
  proy_10_18_raw |>
    # Seleccionar y renombrar columnas
    select(
      codprov_censo = 1,
      grupo_edad = 2,
      Varón_2010 = 4,
      Mujer_2010 = 5,
      Varón_2013 = 16,
      Mujer_2013 = 17,
      Varón_2018 = 37,
      Mujer_2018 = 38
    ) |>

    # Modificar identificador numérico de provincia
    mutate(codprov_censo = str_remove(codprov_censo, "-.*")) |>

    # Aplicar función de limpieza
    clean_indec()
) |>

  # Añadir etiquetas provincias y regiones DEIS
  left_join(prov) |>

  # Calcular proyecciones por año, provincia, sexo y grupo etario decenal
  count(
    anio_enfr,
    prov_nombre,
    region_deis,
    codprov_censo,
    sexo,
    grupo_edad_10,
    wt = value,
    name = "proy_pob"
  ) |>

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
  })()


## Defunciones por DM2 2004-2019 ----
defun_dm2 <- bind_rows(
  ## Defunciones 2004 ##
  def04_raw |>
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

    # Añadir identificador numérico provincias y región DEIS
    left_join(prov),

  ## Defunciones 2005-2019 ##
  def05_19_raw |>
    # Crear columna para el año
    set_names(nm = paste0("20", str_sub(def05_19_raw, 24, 25))) |>

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
) |>

  # Filtrar muertes por DM2 (E11 y E14)
  filter(cie10_causa %in% c("E11", "E14")) |>

  # Cambiar etiquetas grupo etario
  mutate(grupo_edad = str_sub(grupo_edad, 4)) |>

  # Crear grupo edad decenal
  mutate(
    grupo_edad_10 = case_when(
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

  # Crear columna para año ENFR
  mutate(
    anio_enfr = case_when(
      between(anio, "2004", "2006") ~ "2005",
      between(anio, "2008", "2010") ~ "2009",
      between(anio, "2012", "2014") ~ "2013",
      between(anio, "2017", "2019") ~ "2018"
    )
  ) |>

  # Añadir filas faltantes
  complete(
    nesting(anio, anio_enfr),
    nesting(codprov_censo, prov_nombre, region_deis),
    nesting(grupo_edad, grupo_edad_10),
    sexo,
    fill = list(total = 0)
  ) |>

  # Agrupar datos por grupo etario decenal
  count(
    anio,
    anio_enfr,
    codprov_censo,
    prov_nombre,
    region_deis,
    grupo_edad_10,
    sexo,
    wt = total
  )


# Calcular prevalencia y defunciones por DM2 -----------------------------
## Provincia, sexo y grupo etario ----
datos_dm2_prov <- list(
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
      group_by(codprov_censo, prov_nombre, region_deis, sexo, grupo_edad_10) |>
      summarise(
        dm_total = survey_total(dm_auto),
        dm2_total = survey_total(dm2_auto),
        dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv"), na.rm = TRUE),
        .groups = "drop"
      )
  }) |>
  bind_rows(.id = "anio_enfr") |>

  # Combinar con defunciones por DM2
  left_join(
    defun_dm2 |>
      # Calcular defunciones por trienio ENFR
      group_by(
        anio_enfr,
        codprov_censo,
        prov_nombre,
        region_deis,
        grupo_edad_10,
        sexo
      ) |>

      summarise(
        defun_n = sum(n, na.rm = TRUE),
        defun_mean = mean(n, na.rm = TRUE),
        defun_se = sqrt(defun_mean / 3),
        .groups = "drop"
      )
  ) |>

  # Combinar con proyecciones poblacionales
  left_join(proy_pob) |>

  # Combinar con pesos discapacidad DM2
  left_join(comp_dm2) |>

  # Combinar con esperanza de vida
  left_join(ex_ge10) |>

  # Combinar con población estándar 2010
  left_join(pob_est_2010)


## Región DEIS, sexo y grupo etario ----
datos_dm2_reg <- list(
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
      group_by(region_deis, sexo, grupo_edad_10) |>
      summarise(
        dm_total = survey_total(dm_auto),
        dm2_total = survey_total(dm2_auto),
        dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv"), na.rm = TRUE),
        .groups = "drop"
      )
  }) |>
  bind_rows(.id = "anio_enfr") |>

  # Combinar con defunciones por DM2
  left_join(
    defun_dm2 |>
      # Calcular defunciones por trienio ENFR
      group_by(anio_enfr, region_deis, grupo_edad_10, sexo) |>

      summarise(
        defun_n = sum(n, na.rm = TRUE),
        defun_mean = mean(n, na.rm = TRUE),
        defun_se = sqrt(defun_mean / 3),
        .groups = "drop"
      )
  ) |>

  # Combinar con proyecciones poblacionales
  left_join(
    proy_pob |>
      # Calcular proyecciones por región
      count(
        anio_enfr,
        region_deis,
        sexo,
        grupo_edad_10,
        wt = proy_pob,
        name = "proy_pob"
      )
  ) |>

  # Combinar con pesos discapacidad DM2
  left_join(comp_dm2) |>

  # Combinar con esperanza de vida
  left_join(ex_ge10) |>

  # Combinar con población estándar 2010
  left_join(pob_est_2010)


## Total país por sexo y grupo etario ----
datos_dm2_arg <- list(
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
        dm2_prev = survey_mean(dm2_auto, vartype = c("se", "cv"), na.rm = TRUE),
        .groups = "drop"
      )
  }) |>
  bind_rows(.id = "anio_enfr") |>

  # Combinar con defunciones por DM2
  left_join(
    defun_dm2 |>
      # Calcular defunciones por trienio ENFR
      group_by(anio_enfr, grupo_edad_10, sexo) |>

      summarise(
        defun_n = sum(n, na.rm = TRUE),
        defun_mean = mean(n, na.rm = TRUE),
        defun_se = sqrt(defun_mean / 3),
        .groups = "drop"
      )
  ) |>

  # Combinar con proyecciones poblacionales
  left_join(
    proy_pob |>
      # Calcular proyecciones por región
      count(
        anio_enfr,
        sexo,
        grupo_edad_10,
        wt = proy_pob,
        name = "proy_pob"
      )
  ) |>

  # Combinar con pesos discapacidad DM2
  left_join(comp_dm2) |>

  # Combinar con esperanza de vida
  left_join(ex_ge10) |>

  # Combinar con población estándar 2010
  left_join(pob_est_2010)


# Simular AVP, AVD y AVAD ------------------------------------------------
## Provincia, sexo y grupo etario ----
set.seed(123)
sim_avad_prov <- datos_dm2_prov |>
  mutate(
    sim = pmap(
      list(
        defun_mean,
        defun_se,
        dm2_total,
        dm2_total_se,
        ex,
        fwd
      ),
      sim_IU_DALYs
    )
  ) |>
  unnest_wider(sim) |>

  # Reordenar columnas
  select(
    anio_enfr:grupo_edad_10,
    contains(c("pob", "dm", "defun")),
    ex,
    fwd,
    AVP:AVAD_sup
  ) |>

  # Columnas caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))


# Región, sexo y grupo etario ----
sim_avad_reg <- datos_dm2_reg |>
  mutate(
    sim = pmap(
      list(
        defun_mean,
        defun_se,
        dm2_total,
        dm2_total_se,
        ex,
        fwd
      ),
      sim_IU_DALYs
    )
  ) |>
  unnest_wider(sim) |>

  # Reordenar columnas
  select(
    anio_enfr:grupo_edad_10,
    contains(c("pob", "dm", "defun")),
    ex,
    fwd,
    AVP:AVAD_sup
  ) |>

  # Columnas caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))


# Sexo y grupo etario ----
sim_avad_arg <- datos_dm2_arg |>
  mutate(
    sim = pmap(
      list(
        defun_mean,
        defun_se,
        dm2_total,
        dm2_total_se,
        ex,
        fwd
      ),
      sim_IU_DALYs
    )
  ) |>
  unnest_wider(sim) |>

  # Reordenar columnas
  select(
    anio_enfr:grupo_edad_10,
    contains(c("pob", "dm", "defun")),
    ex,
    fwd,
    AVP:AVAD_sup
  ) |>

  # Columnas caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))


# Diccionario de datos ---------------------------------------------------
data_dicc <- tibble(
  variable = names(sim_avad_prov),

  descripción = c(
    "Año de realización de la Encuesta Nacional de Factores de Riesgo (ENFR)",
    "Código numérico de provincia según Censo Nacional 2010",
    "Nombre de la provincia",
    "Región geográfica según clasificación de la DEIS (2021)",
    "Sexo asignado al nacer",
    "Grupo etario decenal",
    "Proyección poblacional por provincia, sexo y grupo etario decenal según Censo Nacional 2010",
    "Población estándar por sexo y grupo etario decenal según Censo Nacional 2010",
    "Total estimado de personas con diabetes mellitus (DM) por autorreporte según resultados ENFR",
    "Error estándar del total estimado de personas con DM por autorreporte según resultados ENFR",
    "Total estimado de personas con DM tipo 2 (DM2) por autorreporte según resultados ENFR",
    "Error estándar del total estimado de personas con DM2 por autorreporte según resultados ENFR",
    "Prevalencia de personas con DM2 por autorreporte según resultados ENFR",
    "Error estándar de la prevalencia de personas con DM2 por autorreporte según resultados ENFR",
    "Coeficiente de variación de la prevalencia de personas con DM2 por autorreporte según resultados ENFR",
    "Defunciones por DM2 para el trienio correspondiente a la ENFR",
    "Defunciones promedio por DM2 para el trienio correspondiente a la ENFR",
    "Error estándar de las defunciones promedio por DM2 para el trienio correspondiente a la ENFR",
    "Esperanza de vida a la edad X según sexo y grupo etario decenal",
    "Peso de discapacidad ponderado para secuelas de DM2",
    "Años de vida perdidos (AVP) por muerte prematura por DM2",
    "Límite inferior del intervalo de incertidumbre (IU) de los AVP por muerte prematura por DM2",
    "Límite superior del IU de los AVP por muerte prematura por DM2",
    "Años vividos con discapacidad (AVD) por DM2",
    "Límite inferior del intervalo de incertidumbre (IU) de los AVD por DM2",
    "Límite superior del IU de los AVD por DM2",
    "Años de vida ajustados por discapacidad (AVAD) para DM2",
    "Límite inferior del intervalo de incertidumbre (IU) de los AVAD por DM2",
    "Límite superior del IU de los AVAD por DM2"
  ),

  tipo_var = map_chr(sim_avad_prov, ~ paste(class(.x), collapse = ", ")),

  niveles = map_chr(
    sim_avad_prov,
    ~ if (is.factor(.x)) {
      paste(levels(.x), collapse = ", ")
    } else {
      "0-Inf"
    }
  )
)


# Exportar datos limpios -------------------------------------------------
# Por provincia, sexo y grupo etario
export(sim_avad_prov, file = "datos_limpios/arg_sim_avad_dm2_prov.csv")

# Por región, sexo y grupo etario
export(sim_avad_reg, file = "datos_limpios/arg_sim_avad_dm2_reg.csv")

# Total país por sexo y grupo etario
export(sim_avad_arg, file = "datos_limpios/arg_sim_avad_dm2.csv")

# Diccionario de datos
export(data_dicc, file = "datos_limpios/dic_arg_sim_avad_dm2.xlsx")


# Limpiar environment y desactivar paquetes ------------------------------
rm(list = ls())

pacman::p_unload("all")
