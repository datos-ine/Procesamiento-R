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
# Última modificación: 23-04-2026 12:07

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
  readxl,
  miniUI
)

# Cargar datos -----------------------------------------------------------
## Proyecciones poblacionales 2001 y 2005 ----
proy_01_05_raw <- extract_tables(
  file = "bases_datos/INDEC_proyec 2001-2015.pdf",
  # pages = c(22:24, 27:28, 25:26, 29:43, 45, 44),
  pages = 21,
  area = locate_areas(
    file = "bases_datos/INDEC_proyec 2001-2015.pdf",
    pages = 21
  ),
  guess = FALSE
)


## Proyecciones poblacionales 2010, 2013 y 2018 ----
proy_10_18_raw <- bind_cols(
  ## 2010-2015 ##
  excel_sheets("bases_datos/c2_proyecciones_prov_2010_2040.xls")[2] |>
    set_names() |>
    map(
      ~ read_excel(
        "bases_datos/c2_proyecciones_prov_2010_2040.xls",
        sheet = .x,
        range = "A3:X28"
      )
    ) |>
    list_rbind(names_to = "arg"),

  ## 2018-2021 ##
  excel_sheets("bases_datos/c2_proyecciones_prov_2010_2040.xls")[2] |>
    set_names() |>
    map(
      ~ read_excel(
        "bases_datos/c2_proyecciones_prov_2010_2040.xls",
        sheet = .x,
        range = "A31:X56"
      )
    ) |>
    list_rbind(names_to = "arg")
)


## ENFR 2005 ----
enfr05 <- read_delim(
  "bases_datos/ENFR/ENFR 2005 - Base usuario.txt",
  col_select = c(
    id = IDENTIFI,
    # codprov_censo = PROV,
    sexo = CHCH04,
    edad = CHCH05,
    dm_auto = CIDI01,
    dm_g = CIDI02,
    wt = PONDERACION
  )
)


## ENFR 2009 ----
enfr09 <- read_delim(
  "bases_datos/ENFR/ENFR 2009 - Base usuario.txt",
  col_select = c(
    id = IDENTIFI,
    # codprov_censo = PRVNC,
    sexo = BHCH04,
    edad = BHCH05,
    dm_auto = BIDI01,
    dm_g = BIDI02,
    wt = PONDERACION
  )
)


## ENFR 2013 ----
enfr13 <- read_delim(
  "bases_datos/ENFR/ENFR 2013 - Base usuario.txt",
  col_select = c(
    ID,
    # codprov_censo = COD_PROVINCIA,
    sexo = BHCH04,
    edad = BHCH05,
    dm_auto = BIDI01,
    dm_g = BIDI02,
    wt = PONDERACION
  )
)


## ENFR 2018 ----
enfr18 <- read_delim(
  "bases_datos/ENFR/ENFR 2018 - Base usuario.txt",
  col_select = c(
    id,
    # codprov_censo = cod_provincia,
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


## Complicaciones DM2 por sexo, grupo etario y año ----
comp_dm2 <- import("datos_limpios/fr_comp_DW_ge10.csv")


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
sim_AVAD <- function(
  defun_mean,
  defun_se,
  dm2_total,
  dm2_total_se,
  ex,
  fwd,
  proy_pob,
  nsim = 10000
) {
  
  # SDs robustos cuando no hay casos / defunciones
  defun_sd <- if_else(defun_mean > 0, defun_se, 1e-6)
  dm2_sd <- if_else(dm2_total > 0, dm2_total_se, 1e-6)

  # Simular defunciones y casos (truncados en 0)
  defun_sim <- rtruncnorm(
    n = nsim,
    a = 0,
    mean = defun_mean,
    sd = defun_sd
  )
  dm2_sim <- rtruncnorm(
    n = nsim,
    a = 0,
    mean = dm2_total,
    sd = dm2_sd
  )

  # AVP, AVD, AVAD
  AVP_sim <- defun_sim * ex
  AVD_sim <- dm2_sim * fwd
  AVAD_sim <- AVP_sim + AVD_sim

  # Tasas específicas por 100.000
  AVP_t_sim <- (AVP_sim / proy_pob) * 1e5
  AVD_t_sim <- (AVD_sim / proy_pob) * 1e5
  AVAD_t_sim <- (AVAD_sim / proy_pob) * 1e5

  # devolver lista con nombres fijos
  list(
    AVP_sim = AVP_sim,
    AVD_sim = AVD_sim,
    AVAD_sim = AVAD_sim,
    AVP_t_sim = AVP_t_sim,
    AVD_t_sim = AVD_t_sim,
    AVAD_t_sim = AVAD_t_sim
  )
}


## AVP, AVD, AVAD y tasas específicas con IU ----
sim_AVAD_IU <- function(
  defun_mean,
  defun_se,
  dm2_total,
  dm2_total_se,
  ex,
  fwd,
  proy_pob,
  nsim = 10000
) {
  sims <- sim_AVAD(
    defun_mean,
    defun_se,
    dm2_total,
    dm2_total_se,
    ex,
    fwd,
    proy_pob,
    nsim
  )

  tibble(
    AVP = quantile(sims$AVP_sim, 0.50, na.rm = TRUE),
    AVP_low = quantile(sims$AVP_sim, 0.025, na.rm = TRUE),
    AVP_upp = quantile(sims$AVP_sim, 0.975, na.rm = TRUE),

    AVD = quantile(sims$AVD_sim, 0.50, na.rm = TRUE),
    AVD_low = quantile(sims$AVD_sim, 0.025, na.rm = TRUE),
    AVD_upp = quantile(sims$AVD_sim, 0.975, na.rm = TRUE),

    AVAD = quantile(sims$AVAD_sim, 0.50, na.rm = TRUE),
    AVAD_low = quantile(sims$AVAD_sim, 0.025, na.rm = TRUE),
    AVAD_upp = quantile(sims$AVAD_sim, 0.975, na.rm = TRUE),

    AVP_tasa = quantile(sims$AVP_t_sim, 0.50, na.rm = TRUE),
    AVP_tasa_low = quantile(sims$AVP_t_sim, 0.025, na.rm = TRUE),
    AVP_tasa_upp = quantile(sims$AVP_t_sim, 0.975, na.rm = TRUE),

    AVD_tasa = quantile(sims$AVD_t_sim, 0.50, na.rm = TRUE),
    AVD_tasa_low = quantile(sims$AVD_t_sim, 0.025, na.rm = TRUE),
    AVD_tasa_upp = quantile(sims$AVD_t_sim, 0.975, na.rm = TRUE),

    AVAD_tasa = quantile(sims$AVAD_t_sim, 0.50, na.rm = TRUE),
    AVAD_tasa_low = quantile(sims$AVAD_t_sim, 0.025, na.rm = TRUE),
    AVAD_tasa_upp = quantile(sims$AVAD_t_sim, 0.975, na.rm = TRUE)
  )
}


## Tasas estandarizadas con IU ----
tasa_est_AVAD <- function(df) {
  # df = una provincia-año-sexo con 6 filas (una por grupo_edad_10)
  df <- df |> arrange(grupo_edad_10)

  # pesos normalizados de la población estándar
  w <- df$pob_est_2010 / sum(df$pob_est_2010, na.rm = TRUE)

  # construir matrices nsim x nage a partir de sim_raw
  AVP_t_mat <- do.call(cbind, lapply(df$sim_raw, \(s) s$AVP_t_sim))
  AVD_t_mat <- do.call(cbind, lapply(df$sim_raw, \(s) s$AVD_t_sim))
  AVAD_t_mat <- do.call(cbind, lapply(df$sim_raw, \(s) s$AVAD_t_sim))

  # tasa estandarizada por réplica (producto matricial)
  AVP_std <- as.numeric(AVP_t_mat %*% w)
  AVD_std <- as.numeric(AVD_t_mat %*% w)
  AVAD_std <- as.numeric(AVAD_t_mat %*% w)

  tibble(
    AVP_tasa_std = quantile(AVP_std, 0.50, na.rm = TRUE),
    AVP_tasa_std_low = quantile(AVP_std, 0.025, na.rm = TRUE),
    AVP_tasa_std_upp = quantile(AVP_std, 0.975, na.rm = TRUE),

    AVD_tasa_std = quantile(AVD_std, 0.50, na.rm = TRUE),
    AVD_tasa_std_low = quantile(AVD_std, 0.025, na.rm = TRUE),
    AVD_tasa_std_upp = quantile(AVD_std, 0.975, na.rm = TRUE),

    AVAD_tasa_std = quantile(AVAD_std, 0.50, na.rm = TRUE),
    AVAD_tasa_std_low = quantile(AVAD_std, 0.025, na.rm = TRUE),
    AVAD_tasa_std_upp = quantile(AVAD_std, 0.975, na.rm = TRUE)
  )
}


## Simulaciones de Monte-Carlo para AVD por cada complicación ----
sim_AVD_comp <- function(
    dm2_total,
    dm2_total_se,
    fwd,
    proy_pob,
    nsim = 10000
) {
  
  # SDs robustos cuando no hay casos
  dm2_sd <- if_else(dm2_total > 0, dm2_total_se, 1e-6)
  
  # Simular casos (truncados en 0)
  dm2_sim <- rtruncnorm(
    n = nsim,
    a = 0,
    mean = dm2_total,
    sd = dm2_sd
  )
  
  # AVP, AVD, AVAD
  AVD_sim <- dm2_sim * fwd
  
  # devolver lista con nombres fijos
  list(
    AVD_sim = AVD_sim
  )
}

## AVD por complicación con IU ----
sim_AVD_IU_ind <- function(
    dm2_total,
    dm2_total_se,
    fwd,
    proy_pob,
    nsim = 10000
) {
  
  sims <- sim_AVD_comp(
    dm2_total,
    dm2_total_se,
    fwd,
    proy_pob,
    nsim
  )
  
  tibble(
    
    AVD = quantile(sims$AVD_sim, 0.50, na.rm = TRUE),
    AVD_low = quantile(sims$AVD_sim, 0.025, na.rm = TRUE),
    AVD_upp = quantile(sims$AVD_sim, 0.975, na.rm = TRUE)
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
      between(age_group, "30-34 years", "35-39 years") ~ "30 a 39",
      between(age_group, "40-44 years", "45-49 years") ~ "40 a 49",
      between(age_group, "50-54 years", "55-59 years") ~ "50 a 59",
      between(age_group, "60-64 years", "65-69 years") ~ "60 a 69",
      between(age_group, "70-74 years", "75-79 years") ~ "70 a 79",
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
### Combinación de complicaciones ----
comp_dm2_t <- comp_dm2 |>
  
  # Agregar variable anio_enfr para posterior join
  mutate(anio_enfr = as.character(anio)) %>% 
  
  # Calcular promedio ponderado de discapacidad (fwd)
  group_by(anio_enfr, sexo, grupo_edad_10) |>
  summarise(
    fwd = sum(comp_frec * dw, na.rm = TRUE),
    .groups = "drop"
  )

### Complicaciones individuales ----
comp_dm2_ind <- comp_dm2 |>
  
  # Agregar variable anio_enfr para posterior join
  mutate(anio_enfr = as.character(anio)) %>% 
  
  # Calcular promedio ponderado de discapacidad (fwd) por complicación
  group_by(anio_enfr, sexo, grupo_edad_10, comp_tipo, comp_qualidiab) |>
  summarise(
    fwd = sum(comp_frec * dw, na.rm = TRUE),
    .groups = "drop"
  )


## Proyecciones poblacionales ----
proy_pob <- bind_rows(
  ### Proyecciones 2001 y 2005 ##
  proy_01_05_raw |>
    
    list_rbind() |>

    # Seleccionar y renombrar columnas
    select(
      # codprov_censo,
      grupo_edad = 1,
      Varón_2001 = 3,
      Mujer_2001 = 4,
      Varón_2005 = 6,
      Mujer_2005 = 7
    ) |>

    # Aplicar función de limpieza
    clean_indec(),

  ## Proyecciones 2010, 2013 y 2018 ##
  proy_10_18_raw |>
    # Seleccionar y renombrar columnas
    select(
      grupo_edad = 2,
      Varón_2010 = 4,
      Mujer_2010 = 5,
      Varón_2013 = 16,
      Mujer_2013 = 17,
      Varón_2018 = 37,
      Mujer_2018 = 38
    ) |>

    # Aplicar función de limpieza
    clean_indec()
) |>

  # Calcular proyecciones por año, sexo y grupo etario decenal
  count(
    anio_enfr,
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

    # Filtrar datos de otro país
    filter(!prov_nombre %in% c("Otro país")) |>
    
    # Filtrar datos ausentes sexo
    filter(between(sexo, "Mujer", "Varón")) |>

    # Filtrar menores de edad y datos ausentes
    filter(between(grupo_edad, "13.30 a 34", "24.85 y más")), #saco pipe, agrego "," para el bind_rows

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

    # Filtrar datos de otro país
    filter(!codprov_censo %in% c("98")) |>

    # Filtrar datos ausentes sexo
    filter(between(sexo, 1, 2)) |>

    # Filtrar menores de edad y datos ausentes
    filter(between(grupo_edad, "07_30 a 34", "17_80 y más")) |>

    # Cambiar niveles sexo
    mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) # saco pipe

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
    nesting(grupo_edad, grupo_edad_10),
    sexo,
    fill = list(total = 0)
  ) |>

  # Agrupar datos por grupo etario decenal
  count(
    anio,
    anio_enfr,
    grupo_edad_10,
    sexo,
    wt = total
  )


# Calcular prevalencia y defunciones por DM2 -----------------------------

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
        dm2_prev = survey_mean(
          dm2_auto,
          vartype = c("ci", "cv"),
          na.rm = TRUE
        ),
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
        defun_n = sum(n, na.rm = TRUE)) %>% # recuento de defunciones totales
      
      mutate(
        defun_mean = (defun_n/3), # promedio de defunciones entre 3 años
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
  left_join(comp_dm2_t, by = join_by(anio_enfr, sexo, grupo_edad_10)) |>

  # Combinar con esperanza de vida
  left_join(ex_ge10) |>

  # Añadir población estándar 2010
  left_join(pob_est_2010)


## Prevalencia DM para AVD individual - Total país por sexo y grupo etario ----
datos_dm2_arg_AVD_ind <- list(
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
  bind_rows(.id = "anio_enfr") |>
  
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
  left_join(comp_dm2_ind, by = join_by(anio_enfr, sexo, grupo_edad_10)) |>
  
  # Añadir población estándar 2010
  left_join(pob_est_2010)



# Simular AVP, AVD y AVAD ------------------------------------------------

## Sexo y grupo etario ----
set.seed(123)

sim_avad_arg <- datos_dm2_arg |>
  # Crear columna para simulaciones
  mutate(
    sim_raw = pmap(
      .l = list(
        defun_mean,
        defun_se,
        dm2_total,
        dm2_total_se,
        ex,
        fwd,
        proy_pob
      ),
      .f = sim_AVAD
    )
  ) |>

  # Simular indicadores y tasas específicas
  mutate(
    sim = pmap(
      .l = list(
        defun_mean,
        defun_se,
        dm2_total,
        dm2_total_se,
        ex,
        fwd,
        proy_pob
      ),
      .f = sim_AVAD_IU
    )
  ) |>
  unnest_wider(sim) |>

  # Añadir población estándar 2010
  left_join(pob_est_2010)

## AVD por complicación, sexo y grupo etario ----
set.seed(123)

sim_avd_ind <- datos_dm2_arg_AVD_ind |>
  # Crear columna para simulaciones
  mutate(
    sim_raw = pmap(
      .l = list(
        dm2_total,
        dm2_total_se,
        fwd,
        proy_pob
      ),
      .f = sim_AVD_comp
    )
  ) |>
  
  # Simular indicadores y tasas específicas
  mutate(
    sim = pmap(
      .l = list(
        dm2_total,
        dm2_total_se,
        fwd,
        proy_pob
      ),
      .f = sim_AVD_IU_ind
    )
  ) |>
  unnest_wider(sim) |>
  
  # Añadir población estándar 2010
  left_join(pob_est_2010)


# Simular tasas estandarizadas -------------------------------------------

## Año y sexo ----
tasa_est_arg <- sim_avad_arg |>
  group_by(anio_enfr, sexo) |>
  group_modify(~ tasa_est_AVAD(.x)) |>
  ungroup()


# Reordenar datos --------------------------------------------------------

## Total país por sexo y grupo etario ----
sim_avad_arg <- sim_avad_arg |>
  # Reordenar columnas
  select(
    anio_enfr:grupo_edad_10,
    contains(c("pob", "dm", "defun")),
    ex,
    fwd,
    AVP:AVAD_tasa_upp
  ) |>

  # Columnas caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))

## AVD individuales por sexo y grupo etario ----
sim_avd_ind <- sim_avd_ind |>
  # Reordenar columnas
  select(
    anio_enfr:grupo_edad_10,
    contains(c("pob", "dm")),
    comp_tipo,
    comp_qualidiab,
    fwd,
    AVD:AVD_upp
  ) |>
  
  # Columnas caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))

# Recuento de AVAD, AVD y AVP totales -------------------------------------

## Año y sexo ----
abs_avad_arg <- sim_avad_arg %>% 
  group_by(anio_enfr, sexo) %>% 
  summarise(
    AVAD = sum(AVAD),
    AVD = sum(AVD),
    AVP = sum(AVP))

# Diccionario de datos ---------------------------------------------------
levels_comp <- levels(sim_avd_ind$comp_qualidiab)

data_dicc <- bind_rows(
  tibble(
  variable = names(sim_avad_arg),

  descripción = c(
    "Año de realización de la Encuesta Nacional de Factores de Riesgo (ENFR)",
    "Sexo asignado al nacer",
    "Grupo etario decenal",
    "Proyección poblacional por sexo y grupo etario decenal según Censo Nacional 2010",
    "Población estándar por sexo y grupo etario decenal según Censo Nacional 2010",
    "Total estimado de personas con diabetes mellitus (DM) por autorreporte según resultados ENFR",
    "Error estándar del total estimado de personas con DM por autorreporte según resultados ENFR",
    "Total estimado de personas con DM tipo 2 (DM2) por autorreporte según resultados ENFR",
    "Error estándar del total estimado de personas con DM2 por autorreporte según resultados ENFR",
    "Prevalencia de personas con DM2 por autorreporte según resultados ENFR",
    "Límite inferior del intervalo de confianza (CI) de la prevalencia de personas con DM2 por autorreporte según resultados ENFR",
    "Límite superior del intervalo de confianza (CI) de la prevalencia de personas con DM2 por autorreporte según resultados ENFR",
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
    "Límite superior del IU de los AVAD por DM2",
    "Tasa específica de AVP por DM2",
    "Límite inferior del intervalo de incertidumbre (IU) de la tasa de AVP por DM2",
    "Límite superior del IU de la tasa de AVP por DM2",
    "Tasa específica de AVD por DM2",
    "Límite inferior del intervalo de incertidumbre (IU) de la tasa de AVD por DM2",
    "Límite superior del IU de la tasa de AVD por DM2",
    "Tasa específica de AVAD por DM2",
    "Límite inferior del intervalo de incertidumbre (IU) de la tasa de AVAD por DM2",
    "Límite superior del IU de la tasa de AVAD por DM2"
  ),

  tipo_var = map_chr(sim_avad_arg, ~ paste(class(.x), collapse = ", ")),

  niveles = map_chr(
    sim_avad_arg,
    ~ if (is.factor(.x)) {
      paste(levels(.x), collapse = ", ")
    } else {
      "0-Inf"
    }
  )
),
tibble(variable = "comp_qualidiab",
       descripción = "Complicación crónica asociada a la DM2",
       tipo_var = "factor",
       niveles =  paste(levels_comp, collapse = ", ")
))


# Exportar datos limpios -------------------------------------------------

# AVP, AVD, AVAD y tasas específicas por sexo y grupo etario
export(sim_avad_arg, file = "datos_limpios/arg_sim_avad_dm2.rds")

# AVD por complicación, sexo y grupo etario
export(sim_avd_ind, file = "datos_limpios/arg_sim_avd_ind.rds")

# Tasas estandarizadas por año y sexo
export(tasa_est_arg, file = "datos_limpios/arg_tasas_est.rds")

export(tasa_est_arg, file = "datos_limpios/arg_tasas_est.xlsx") # para Joinpoint

# Recuentos absolutos por año y sexo
export(abs_avad_arg, file = "datos_limpios/arg_avad_abs.rds")

export(abs_avad_arg, file = "datos_limpios/arg_avad_abs.xlsx") # para Joinpoint

# Población estándar Censo 2010
export(pob_est_2010, file = "datos_limpios/pob_est_2010.rds")


# Diccionario de datos
export(data_dicc, file = "datos_limpios/dic_arg_avad_dm2.xlsx")


# Limpiar environment ----------------------------------------------------
rm(list = ls())

pacman::p_unload("all")
