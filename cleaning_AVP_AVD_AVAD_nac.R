### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
### en Argentina, período 2005-2018
### Limpieza de datos y simulación de AVP, AVD, AVAD y tasas mediante cadenas de
### Monte-Carlo para obtener intervalos de incertidumbre (IU)
### Autoras:
## - Micaela Gauto
## - Tamara Ricardo
### Fecha de creación: 27-01-2026
# Última modificación: 14-05-2026 09:46

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Grupos etarios
  epikit,
  # Diseño muestral y prevalencia
  srvyr,
  # Cadenas de Monte-Carlo
  truncnorm,
  # Manejo de datos
  rio,
  janitor,
  tidyverse,
  readxl
)


# Cargar datos AVP -------------------------------------------------------
## WHO-GHO: Tablas de vida Argentina (2019) -----
ex_ge10 <- read_csv2(
  "bases_datos/argentina_tabla de vida_GHO.csv",
  skip = 1
)


# MSAL - DEIS: Defunciones ocurridas y registradas en Argentina por sexo
# y grupo etario quinquenal para el periodo 2004-2019.
# Se consideró DM2 como causa de muerte para los códigos E11 y E14 de la CIE-10.
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
# QualiDiab: Complicaciones DM2 por sexo, grupo etario y año -----
# Frecuencia de complicaciones microvasculares y macrovasculares por sexo y grupo etario
# según registros de la Red Qualidiab.
# Se incluyen aquellas complicaciones consensuadas con la Red.
# Pesos de discapacidad: se consideraron los publicados por el Global Burden Disease.
comp_dm2_raw <- import("datos_limpios/fr_comp_DW_ge10.csv")


# INDEC - Encuesta Nacional de Factores de Riesgo (ENFR): Autorreporte de diabetes
# mellitus (DM) por sexo y edad para los años 2005, 2009, 2013 y 2018.
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
# Censo Nacional 2001 y 2010: Proyecciones poblacionales por sexo y
# grupo etario quinquenal (2001-2018).
## Proyecciones poblacionales -----
proy_pob <- import("datos_limpios/arg_proy_pob.rds")

## Población estándar 2010 -----
pob_est_2010 <- import("datos_limpios/pob_est_2010.rds")


# Funciones auxiliares ---------------------------------------------------
## Limpiar datos ENFR ----------------------------------------------------
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

## Simulación de AVP, AVD y AVAD -----------------------------------------
source("fun_auxiliares.R")


# Limpiar datos AVP ------------------------------------------------------
## Esperanza de vida -----
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


## Defunciones 2004-2019 -----
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
## Complicaciones DM2 -----
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


# Prevalencia DM2 -----
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
  left_join(comp_dm2) |>

  # Seleccionar columnas relevantes
  select(
    anio_enfr,
    grupo_edad_10,
    sexo,
    contains("dm2_"),
    defun_mean,
    defun_se,
    ex,
    proy_pob,
    contains("fwd")
  )

# ### Complicaciones individuales ----
# comp_dm2_ind <- comp_dm2 |>

#   # Agregar variable anio_enfr para posterior join
#   mutate(anio_enfr = as.character(anio)) %>%

#   # Calcular promedio ponderado de discapacidad (fwd) por complicación
#   group_by(anio_enfr, sexo, grupo_edad_10, comp_tipo, comp_qualidiab) |>
#   summarise(
#     fwd = sum(comp_frec * dw, na.rm = TRUE),
#     .groups = "drop"
#   )

# ## Prevalencia DM para AVD individual - Total país por sexo y grupo etario ----
# datos_dm2_arg_AVD_ind <- list(
#   "2005" = enfr05,
#   "2009" = enfr09,
#   "2013" = enfr13,
#   "2018" = enfr18
# ) |>
#   map(\(x) {
#     x |>
#       # Aplicar función de limpieza
#       clean_enfr() |>
#       # Calcular total personas con DM y prevalencia
#       group_by(sexo, grupo_edad_10) |>
#       summarise(
#         dm_total = survey_total(dm_auto),
#         dm2_total = survey_total(dm2_auto),
#         dm2_prev = survey_mean(
#           dm2_auto,
#           vartype = c("ci", "cv"),
#           na.rm = TRUE
#         ),
#         .groups = "drop"
#       )
#   }) |>
#   bind_rows(.id = "anio_enfr") |>

#   # Combinar con proyecciones poblacionales
#   left_join(
#     proy_pob |>
#       # Calcular proyecciones por región
#       count(
#         anio_enfr,
#         sexo,
#         grupo_edad_10,
#         wt = proy_pob,
#         name = "proy_pob"
#       )
#   ) |>

#   # Combinar con pesos discapacidad DM2
#   left_join(comp_dm2_ind, by = join_by(anio_enfr, sexo, grupo_edad_10)) |>

#   # Añadir población estándar 2010
#   left_join(pob_est_2010)
#

# Simular AVP, AVD y AVAD ------------------------------------------------
# Cálculo de AVP, AVD y AVAD e intervalos de incertidumbre mediante cadenas de
# Monte-Carlo con 10.000 réplicas. Se usaron las siguientes simulaciones:
# - Defunciones: distribución normal truncada en cero, con media igual al
## valor estimado y SD aproximada por sqrt(mu/3).
# - Prevalencia DM2: se simularon con una normal truncada en [0,1], con media
## igual a la estimación puntual y desviación estándar igual a su error estándar.
set.seed(123)

sim_avad_dm2 <- datos_dm2 |>
  # Seleccionar columnas
  select(anio_enfr:proy_pob, contains("total")) |>

  ## Simular AVP ##
  mutate(
    avp = pmap(
      list(
        defun_mean,
        defun_se,
        ex,
        proy_pob
      ),
      sim_AVP
    ),

    avp_res = map(avp, "resumen")
  ) |>

  ## Simular AVD y AVAD total complicaciones ##
  mutate(
    avd = pmap(
      list(
        dm2_total,
        dm2_total_se,
        total_comp_fwd,
        proy_pob
      ),
      sim_AVD
    ),

    avad = map2(avp, avd, sim_AVAD),

    res_total = pmap(
      list(avd, avad),
      \(b, c) {
        bind_cols(
          b$resumen,
          c$resumen
        )
      }
    )
  ) |>

  ## Simular AVD y AVAD complicaciones microvasculares ##
  mutate(
    avd_micro = pmap(
      list(
        dm2_total,
        dm2_total_se,
        total_micro_fwd,
        proy_pob
      ),
      sim_AVD
    ),

    avad_micro = map2(avp, avd_micro, sim_AVAD),

    res_micro = pmap(
      list(avd_micro, avad_micro),
      \(b, c) {
        bind_cols(
          b$resumen,
          c$resumen
        ) |>

          rename_with(
            ~ paste0(.x, "_micro")
          )
      }
    )
  ) |>

  ## Simular AVD y AVAD complicaciones macrovasculares ##
  mutate(
    avd_macro = pmap(
      list(
        dm2_total,
        dm2_total_se,
        total_macro_fwd,
        proy_pob
      ),
      sim_AVD
    ),

    avad_macro = map2(avp, avd_macro, sim_AVAD),

    res_macro = pmap(
      list(avd_macro, avad_macro),
      \(b, c) {
        bind_cols(
          b$resumen,
          c$resumen
        ) |>

          rename_with(
            ~ paste0(.x, "_macro")
          )
      }
    )
  ) |>

  ## Expandir a columnas
  unnest_wider(c(
    avp_res,
    res_total,
    res_micro,
    res_macro
  )) |>

  # Añadir población estándar 2010
  left_join(pob_est_2010) |>

  # Columnas caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))


# ## AVD por complicación, sexo y grupo etario ----
# set.seed(123)

# sim_avd_ind <- datos_dm2_arg_AVD_ind |>
#   # Crear columna para simulaciones
#   mutate(
#     sim_raw = pmap(
#       .l = list(
#         dm2_total,
#         dm2_total_se,
#         fwd,
#         proy_pob
#       ),
#       .f = sim_AVD_comp
#     )
#   ) |>

#   # Simular indicadores y tasas específicas
#   mutate(
#     sim = pmap(
#       .l = list(
#         dm2_total,
#         dm2_total_se,
#         fwd,
#         proy_pob
#       ),
#       .f = sim_AVD_IU_ind
#     )
#   ) |>
#   unnest_wider(sim) |>

#   # Añadir población estándar 2010
#   left_join(pob_est_2010)  |>
#
#   # Reordenar columnas
#   select(
#     anio_enfr:grupo_edad_10,
#     contains(c("pob", "dm")),
#     comp_tipo,
#     comp_qualidiab,
#     fwd,
#     AVD:AVD_upp
#   ) |>

#   # Columnas caracter a factor
#   mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))
#

# Recuento de AVAD, AVD y AVP totales -------------------------------------
abs_avad_dm2 <- sim_avad_dm2 %>%
  group_by(anio_enfr, sexo) %>%
  summarise(
    AVAD = sum(AVAD),
    AVD = sum(AVD),
    AVP = sum(AVP)
  )


# Simular tasas estandarizadas -------------------------------------------
# Cálculo de tasas estandarizadas AVP, AVD y AVAD e intervalos de incertidumbre (IU)
# mediante cadenas de Monte-Carlo con 10.000 réplicas.
set.seed(123)

tasa_est_dm2 <- sim_avad_dm2 |>
  # Agrupar datos
  group_by(anio_enfr, sexo) |>

  group_modify(
    \(df, key) {
      bind_cols(
        # AVP
        tasa_est(
          df,
          sim_col = "avp",
          nombre = "AVP",
          pob_est = pob_est
        ),

        # AVD total
        tasa_est(
          df,
          sim_col = "avd",
          nombre = "AVD",
          pob_est = pob_est
        ),

        # AVAD total
        tasa_est(
          df,
          sim_col = "avad",
          nombre = "AVAD",
          pob_est = pob_est
        ),

        # AVD micro
        tasa_est(
          df,
          sim_col = "avd_micro",
          nombre = "AVD_micro",
          pob_est = pob_est
        ),

        # AVAD micro
        tasa_est(
          df,
          sim_col = "avad_micro",
          nombre = "AVAD_micro",
          pob_est = pob_est
        ),

        # AVD macro
        tasa_est(
          df,
          sim_col = "avd_macro",
          nombre = "AVD_macro",
          pob_est = pob_est
        ),

        # AVAD macro
        tasa_est(
          df,
          sim_col = "avad_macro",
          nombre = "AVAD_macro",
          pob_est = pob_est
        )
      )
    }
  ) |>

  ungroup()

# ## Año y sexo ----
# tasa_est_arg <- sim_avad |>
#   group_by(anio_enfr, sexo) |>
#   group_modify(~ tasa_est_AVAD(.x, pob_est = pob_est)) |>
#   ungroup()
#

# Guardar datos limpios --------------------------------------------------
## Datos para calcular AVP, AVD y AVAD
export(datos_dm2, file = "datos_limpios/arg_datos_dm2.rds")

## AVP, AVD, AVAD y tasas simuladas
export(sim_avad_dm2, file = "datos_limpios/arg_sim_avad.rds")

## Tasas estandarizadas simuladas
export(tasa_est_dm2, file = "datos_limpios/arg_sim_tasa_est.rds")

# # AVD por complicación, sexo y grupo etario
# export(sim_avd_ind, file = "datos_limpios/arg_sim_avd_ind.rds")

# Recuentos absolutos por año y sexo
export(abs_avad_dm2, file = "datos_limpios/arg_avad_abs.rds")
export(abs_avad_dm2, file = "datos_limpios/arg_avad_abs.xlsx") # para Joinpoint


# # Diccionario de datos ---------------------------------------------------
# levels_comp <- levels(sim_avd_ind$comp_qualidiab)

# data_dicc <- bind_rows(
#   tibble(
#     variable = names(sim_avad_arg),

#     descripción = c(
#       "Año de realización de la Encuesta Nacional de Factores de Riesgo (ENFR)",
#       "Sexo asignado al nacer",
#       "Grupo etario decenal",
#       "Proyección poblacional por sexo y grupo etario decenal según Censo Nacional 2010",
#       "Población estándar por sexo y grupo etario decenal según Censo Nacional 2010",
#       "Total estimado de personas con diabetes mellitus (DM) por autorreporte según resultados ENFR",
#       "Error estándar del total estimado de personas con DM por autorreporte según resultados ENFR",
#       "Total estimado de personas con DM tipo 2 (DM2) por autorreporte según resultados ENFR",
#       "Error estándar del total estimado de personas con DM2 por autorreporte según resultados ENFR",
#       "Prevalencia de personas con DM2 por autorreporte según resultados ENFR",
#       "Límite inferior del intervalo de confianza (CI) de la prevalencia de personas con DM2 por autorreporte según resultados ENFR",
#       "Límite superior del intervalo de confianza (CI) de la prevalencia de personas con DM2 por autorreporte según resultados ENFR",
#       "Coeficiente de variación de la prevalencia de personas con DM2 por autorreporte según resultados ENFR",
#       "Defunciones por DM2 para el trienio correspondiente a la ENFR",
#       "Defunciones promedio por DM2 para el trienio correspondiente a la ENFR",
#       "Error estándar de las defunciones promedio por DM2 para el trienio correspondiente a la ENFR",
#       "Esperanza de vida a la edad X según sexo y grupo etario decenal",
#       "Peso de discapacidad ponderado para secuelas de DM2",
#       "Años de vida perdidos (AVP) por muerte prematura por DM2",
#       "Límite inferior del intervalo de incertidumbre (IU) de los AVP por muerte prematura por DM2",
#       "Límite superior del IU de los AVP por muerte prematura por DM2",
#       "Años vividos con discapacidad (AVD) por DM2",
#       "Límite inferior del intervalo de incertidumbre (IU) de los AVD por DM2",
#       "Límite superior del IU de los AVD por DM2",
#       "Años de vida ajustados por discapacidad (AVAD) para DM2",
#       "Límite inferior del intervalo de incertidumbre (IU) de los AVAD por DM2",
#       "Límite superior del IU de los AVAD por DM2",
#       "Tasa específica de AVP por DM2",
#       "Límite inferior del intervalo de incertidumbre (IU) de la tasa de AVP por DM2",
#       "Límite superior del IU de la tasa de AVP por DM2",
#       "Tasa específica de AVD por DM2",
#       "Límite inferior del intervalo de incertidumbre (IU) de la tasa de AVD por DM2",
#       "Límite superior del IU de la tasa de AVD por DM2",
#       "Tasa específica de AVAD por DM2",
#       "Límite inferior del intervalo de incertidumbre (IU) de la tasa de AVAD por DM2",
#       "Límite superior del IU de la tasa de AVAD por DM2"
#     ),

#     tipo_var = map_chr(sim_avad_arg, ~ paste(class(.x), collapse = ", ")),

#     niveles = map_chr(
#       sim_avad_arg,
#       ~ if (is.factor(.x)) {
#         paste(levels(.x), collapse = ", ")
#       } else {
#         "0-Inf"
#       }
#     )
#   ),
#   tibble(
#     variable = "comp_qualidiab",
#     descripción = "Complicación crónica asociada a la DM2",
#     tipo_var = "factor",
#     niveles = paste(levels_comp, collapse = ", ")
#   )
# )

## Guardar diccionario de datos -----
# export(data_dicc, file = "datos_limpios/dic_arg_avad_dm2.xlsx")

# Limpiar environment ----------------------------------------------------
rm(list = ls())

pacman::p_unload("all")
