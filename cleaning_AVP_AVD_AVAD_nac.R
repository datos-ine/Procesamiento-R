### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
### en Argentina, período 2005-2018
### Limpieza de datos y simulación de AVP, AVD, AVAD y tasas mediante cadenas de
### Monte-Carlo para obtener intervalos de incertidumbre (IU)
### Autoras:
## - Micaela Gauto
## - Tamara Ricardo
### Fecha de creación: 27-01-2026
# Última modificación: 27-05-2026 13:24

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
ex_ge10_raw <- read_csv2(
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
# Qualidiab: Complicaciones DM2 por sexo, grupo etario y año -----
# Frecuencia de complicaciones microvasculares y macrovasculares por sexo y grupo etario
# según registros de la Red Qualidiab.
# Se incluyen aquellas complicaciones consensuadas con la Red.
# Pesos de discapacidad: se consideraron los publicados por el Global Burden Disease.
comp_dm2_raw <- import(
  "datos_limpios/fr_comp_DW_ge10.csv",
  na.strings = ""
)


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
source("fun_auxiliares.R")

# Limpiar datos AVP ------------------------------------------------------
## Esperanza de vida -----
ex_ge10 <- ex_ge10_raw |>
  # Seleccionar columnas
  select(
    ind = 1,
    grupo_edad = 2,
    # Total = 3,
    "Varón" = 4,
    Mujer = 5
  ) |>

  # Filtrar menores de 30 años y totales
  filter(
    between(grupo_edad, "30-34 years", "45-49 years") |
      between(grupo_edad, "50-54 years", "85+ years")
  ) |>

  # Cambiar etiquetas indicadores
  mutate(ind = str_extract(ind, '^[^ ]+')) |>

  # Pasar a formato long
  pivot_longer(cols = c("Varón":Mujer), names_to = "sexo") |>

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
    grupo_edad = grupo_de_edad,
    cie10_causa = causa_de_muerte_cie_10
  ) |>

  ### Añadir defunciones 2005-2019 ###
  bind_rows(
    def05_19 |>
      # Estandarizar nombres de columnas
      clean_names() |>
      rename(
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
  filter_out(jurisdiccion == "Otro país" | provres == "98") |>

  # Filtrar valores ausentes sexo
  filter_out(sexo %in% c("Desconocido", "Indeterminado", "9")) |>

  # Modificar etiquetas grupo edad
  mutate(grupo_edad = str_sub(grupo_edad, 4)) |>

  # Filtrar menores de 30 años
  filter_out(
    between(grupo_edad, "1 a 9", "3 años") |
      str_detect(grupo_edad, "4 |5 a 9|Men|esp")
  ) |>

  # Cambiar etiquetas sexo
  mutate(sexo = fct_recode(sexo, Varón = "1", Mujer = "2")) |>

  # Crear grupo etario decenal
  mutate(
    grupo_edad_10 = fct_collapse(
      grupo_edad,
      "30 a 39" = c("30 a 34", "35 a 39"),
      "40 a 49" = c("40 a 44", "45 a 49"),
      "50 a 59" = c("50 a 54", "55 a 59"),
      "60 a 69" = c("60 a 64", "65 a 69"),
      "70 a 79" = c("70 a 74", "75 a 79"),
      other_level = "80+"
    )
  ) |>

  # # Completar datos faltantes año
  mutate(anio = replace_na(anio, "2004")) |>

  # Crear columna para año ENFR
  mutate(
    anio_enfr = fct_collapse(
      anio,
      "2005" = c("2004", "2005", "2006"),
      "2009" = c("2008", "2009", "2010"),
      "2013" = c("2012", "2013", "2014"),
      other_level = "2018"
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

  # Crear variable año ENFR
  mutate(anio_enfr = factor(anio)) |>

  # Crear variable complicacion:tipo
  mutate(
    comp_qd_tipo = case_when(
      comp_tipo == "macrovascular" ~ paste0(
        "macro_",
        make_clean_names(comp_qualidiab, allow_dupes = TRUE)
      ),
      comp_tipo == "microvascular" ~ paste0(
        "micro_",
        make_clean_names(comp_qualidiab, allow_dupes = TRUE)
      ),
      .default = "sin_complicaciones"
    )
  ) |>

  # Calcular promedio ponderado de discapacidad (fwd)
  group_by(anio_enfr, grupo_edad_10, sexo, comp_qd_tipo) |>
  summarise(
    fwd = sum(comp_frec * dw, na.rm = TRUE),
    .groups = "drop"
  ) |>

  # Pasar a formato wide
  pivot_wider(
    names_from = comp_qd_tipo,
    values_from = fwd
  ) |>

  # Crear columnas de totales micro y macro
  mutate(
    # total_micro = rowSums(pick(starts_with("micro")), na.rm = TRUE),
    # total_macro = rowSums(pick(starts_with("macro")), na.rm = TRUE),
    total_compl = rowSums(pick(starts_with(c("micro", "macro", "sin"))), na.rm = TRUE)
  ) |>

  # Ordenar columnas
  select(
    anio_enfr:sexo,
    micro_neuropatia_p = micro_neuropatia_periferica,
    micro_retinopatia_np = micro_retinopatia_no_proliferativa,
    micro_retinopatia_p = micro_retinopatia_proliferativa,
    micro_disf_erectil = micro_disfuncion_erectil,
    starts_with("micro"),
    macro_claudicacion = macro_claudicacion_miembros_inferiores,
    starts_with("macro"),
    sin_complicaciones:total_compl
  )


# Prevalencia DM2 -----
prev_dm2 <- list(
  "2005" = enfr05,
  "2009" = enfr09,
  "2013" = enfr13,
  "2018" = enfr18
) |>

  map(
    \(x) {
      # =========================
      # Limpieza
      # =========================
      x <- x |>

        filter(edad >= 30) |>

        mutate(
          grupo_edad_10 = age_categories(
            edad,
            lower = 30,
            upper = 80,
            by = 10,
            separator = " a "
          ),

          sexo = if_else(
            sexo == 1,
            "Varón",
            "Mujer"
          ),

          dm_auto = if_else(
            dm_auto == 1,
            1,
            0
          ),

          dm2_auto = dm_auto * 0.9
        )

      # =========================
      # Diseño muestral
      # =========================
      diseno <- if ("wt" %in% names(x)) {
        x |>
          as_survey_design(
            weights = wt
          )
      } else {
        x |>
          as_survey_rep(
            weights = wf1p,
            repweights = starts_with("wf1p"),
            type = "bootstrap"
          )
      }

      # =========================
      # Prevalencia
      # =========================
      diseno |>

        group_by(
          sexo,
          grupo_edad_10
        ) |>

        summarise(
          dm2_total = survey_total(
            dm2_auto
          ),

          dm2_prev = survey_mean(
            dm2_auto,
            vartype = c("se", "cv"),
            na.rm = TRUE
          ),

          .groups = "drop"
        )
    }
  ) |>

  bind_rows(
    .id = "anio_enfr"
  )


# Unir datos limpios -----------------------------------------------------
datos_dm2 <- prev_dm2 |>
  # Añadir fallecimientos
  left_join(defun_dm2) |>

  # Añadir proyecciones poblacionales y población estándar
  left_join(proy_pob) |>
  left_join(pob_est_2010) |>

  # Añadir esperanza de vida
  left_join(
    ex_ge10 |>
      select(sexo, grupo_edad_10, ex)
  ) |>

  # Añadir pesos discapacidad
  left_join(comp_dm2)


# Simular AVP, AVD, AVAD y tasas -----------------------------------------
# Cálculo de AVP, AVD y AVAD e intervalos de incertidumbre mediante cadenas de
# Monte-Carlo con 10.000 réplicas. Se usaron las siguientes simulaciones:
# - Defunciones: distribución normal truncada en cero, con media igual al
## valor estimado y SD aproximada por sqrt(mu/3).
# - Prevalencia DM2: se simularon con una normal truncada en [0,1], con media
## igual a la estimación puntual y desviación estándar igual a su error estándar.

# set.seed(123)

# sim_avad_dm2 <- datos_dm2 |>
#   # Seleccionar columnas
#   select(anio_enfr:ex, contains("total")) |>
# 
#   ## Simular AVP ##
#   mutate(
#     avp = pmap(
#       list(
#         defun_mean,
#         defun_se,
#         ex,
#         proy_pob
#       ),
#       sim_AVP
#     ),
# 
#     avp_res = map(avp, "resumen")
#   ) |>
# 
#   ## Simular AVD y AVAD complicaciones microvasculares ##
#   mutate(
#     avd_micro = pmap(
#       list(
#         dm2_total,
#         dm2_total_se,
#         total_micro,
#         proy_pob
#       ),
#       sim_AVD
#     ),
# 
#     avad_micro = map2(avp, avd_micro, sim_AVAD),
# 
#     res_micro = pmap(
#       list(avd_micro, avad_micro),
#       \(b, c) {
#         bind_cols(
#           b$resumen,
#           c$resumen
#         ) |>
# 
#           rename_with(
#             ~ paste0(.x, "_micro")
#           )
#       }
#     )
#   ) |>
# 
#   ## Simular AVD y AVAD complicaciones macrovasculares ##
#   mutate(
#     avd_macro = pmap(
#       list(
#         dm2_total,
#         dm2_total_se,
#         total_macro,
#         proy_pob
#       ),
#       sim_AVD
#     ),
# 
#     avad_macro = map2(avp, avd_macro, sim_AVAD),
# 
#     res_macro = pmap(
#       list(avd_macro, avad_macro),
#       \(b, c) {
#         bind_cols(
#           b$resumen,
#           c$resumen
#         ) |>
# 
#           rename_with(
#             ~ paste0(.x, "_macro")
#           )
#       }
#     )
#   ) |>
# 
#   ## Simular AVD y AVAD total complicaciones ##
#   mutate(
#     avd = pmap(
#       list(
#         dm2_total,
#         dm2_total_se,
#         total_compl,
#         proy_pob
#       ),
#       sim_AVD
#     ),
# 
#     avad = map2(avp, avd, sim_AVAD),
# 
#     res_total = pmap(
#       list(avd, avad),
#       \(b, c) {
#         bind_cols(
#           b$resumen,
#           c$resumen
#         )
#       }
#     )
#   ) |>
# 
#   ## Expandir a columnas
#   unnest_wider(c(
#     avp_res,
#     res_total,
#     res_micro,
#     res_macro
#   )) |>
# 
#   # Columnas caracter a factor
#   mutate(across(
#     .cols = where(is.character),
#     .fns = ~ factor(.x)
#   ))


# Alternativa para el cálculo de indicadores ------------------------------
# AVP, AVD (individuales, microvasculares, macrovasculares y totales) y AVAD

## Nombres de las columnas fwd organizados por grupo
cols_micro <- c(
  "micro_neuropatia_p",
  "micro_retinopatia_np",
  "micro_retinopatia_p",
  "micro_disf_erectil",
  "micro_nefropatia",
  "micro_ceguera",
  "micro_amputacion"
)
cols_macro <- c(
  "macro_claudicacion",
  "macro_acv",
  "macro_iam",
  "macro_ic"
)
cols_fwd_individuales <- c(
  cols_micro,
  cols_macro,
  "sin_complicaciones"
)

## Simulaciones
set.seed(123)
sim_avad_dm2 <- datos_dm2 |>
  
  mutate(
    
    # Cálculo de AVP
    avp = pmap(
      list(defun_mean, 
           defun_se, 
           ex, 
           proy_pob),
      sim_AVP
    ),
    avp_res = map(avp, "resumen"),
    
    # Cálculo de AVD (por complicación, tipo de complicación y total) y AVAD
    avd_comp = pmap(
      c(
        list(
          n        = dm2_total,
          n_se     = dm2_total_se,
          proy_pob = proy_pob
        ),
        select(datos_dm2, all_of(cols_fwd_individuales))
      ),
      function(n, n_se, proy_pob, ...) {
        fwd_vec <- c(...)
        sim_AVD_multi(
          n          = n,
          n_se       = n_se,
          fwd_vec    = fwd_vec,
          proy_pob   = proy_pob,
          cols_micro = cols_micro,
          cols_macro = cols_macro
        )
      }
    ),
    
    # Cálculo de AVD total
    avd = map(avd_comp, "total_compl"),
    
    # Cálculo de AVAD total
    avad = map2(avp, avd, sim_AVAD),
    
    # Resúmenes
    # AVP
    avp_res   = map(avp,  "resumen"),
    # AVD y AVAD
    res_total = pmap(
      list(avd, avad),
      \(b, c) bind_cols(b$resumen, c$resumen)
    ),
    # AVD por tipo de complicación
    res_tipo = map(avd_comp, \(comp_list) {
      purrr::map_dfc(
        c("total_micro", "total_macro"),
        \(nm) comp_list[[nm]]$resumen |>
          rename_with(~ paste0(.x, "_", nm))
      )
    }), 
    # AVD individuales
    res_comp = map(avd_comp, \(comp_list) {
      purrr::map_dfc(
        cols_fwd_individuales,
        \(nm) comp_list[[nm]]$resumen |>
          rename_with(~ paste0(.x, "_", nm))
      )
    })
    ) %>% 
    
  ## Expandir a columnas
  unnest_wider(c(avp_res, res_total, res_tipo, res_comp)) %>% 
  
  ## Columnas caracter a factor
  mutate(across(
    .cols = where(is.character),
    .fns = ~ factor(.x)
  ))

names(sim_avad_dm2)

# Recuento de AVAD, AVD y AVP totales -------------------------------------
abs_avad_dm2 <- sim_avad_dm2 %>%
  group_by(anio_enfr, sexo) %>%
  summarise(
    across(
      .cols = c(AVP, AVD, AVAD, AVD_total_micro, AVD_total_macro, AVD_sin_complicaciones,
                contains("AVD_micro"),
                contains("AVD_macro")),
      .fns = ~ sum(.x)
    ),
    .groups = "drop"
  ) |>

  # Añadir totales
  bind_rows(
    sim_avad_dm2 |>
      group_by(anio_enfr) |>
      summarise(
        across(
          .cols = c(AVP, AVD, AVAD, AVD_total_micro, AVD_total_macro, AVD_sin_complicaciones,
                    contains("AVD_micro"),
                    contains("AVD_macro")),
          .fns = ~ sum(.x)
        ),
        .groups = "drop"
      ) |>
      mutate(sexo = "Ambos sexos")
  )


# Simular tasas estandarizadas -------------------------------------------
# Cálculo de tasas estandarizadas AVP, AVD y AVAD e intervalos de incertidumbre (IU)
# mediante cadenas de Monte-Carlo con 10.000 réplicas.
set.seed(123)
tasa_est_dm2 <- sim_avad_dm2 |>
  
  mutate(
    avd_micro = map(avd_comp, "total_micro"),
    avd_macro = map(avd_comp, "total_macro")
  ) %>% 

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

        # AVD macro
        tasa_est(
          df,
          sim_col = "avd_macro",
          nombre = "AVD_macro",
          pob_est = pob_est
        )

      )
    }
  ) |>

  ungroup()

# Remover columnas innecesarias ------------------------------------------
sim_avad_dm2 <- sim_avad_dm2 |>
  select(
    !where(is.list),
    -contains("dm2_total"),
    -contains("defun"),
    -ex,
    -(micro_neuropatia_p:total_compl)
  )


# Guardar datos limpios --------------------------------------------------
## Dataset para simulaciones AVP, AVD y AVAD
export(datos_dm2, file = "datos_limpios/arg_datos_dm2.rds")

## AVP, AVD, AVAD y tasas simuladas
export(sim_avad_dm2, file = "datos_limpios/arg_sim_avad.rds")

## Tasas estandarizadas simuladas
export(tasa_est_dm2, file = "datos_limpios/arg_sim_tasa_est.rds")

# Recuentos absolutos por año y sexo
export(abs_avad_dm2, file = "datos_limpios/arg_avad_abs.rds")
export(abs_avad_dm2, file = "datos_limpios/arg_avad_abs.xlsx") # para Joinpoint


# Diccionarios de datos --------------------------------------------------
## Dataset para simulaciones AVP, AVD y AVAD -----
dicc_dm2 <- tibble(
  variable = names(datos_dm2),
  tipo_var = map_chr(datos_dm2, ~ paste(class(.x), collapse = ", ")),
  niveles = map_chr(
    datos_dm2,
    ~ if (is.factor(.x)) {
      paste(levels(.x), collapse = ", ")
    } else {
      "0-Inf"
    }
  ),
  descripcion = c(
    "Año de realización de la Encuesta Nacional de Factores de Riesgo (ENFR)",
    "Sexo asignado al nacer",
    "Grupo etario decenal",
    "Total estimado de personas con DM tipo 2 (DM2) por autorreporte según resultados ENFR",
    "Error estándar del total estimado de personas con DM2 por autorreporte según resultados ENFR",
    "Prevalencia estimada de personas con DM2 por autorreporte según resultados ENFR",
    "Error estándar de la prevalencia estimada de personas con DM2 por autorreporte según resultados ENFR",
    "Coeficiente de variación de la prevalencia estimada de personas con DM2 por autorreporte según resultados ENFR",
    "Número de defunciones por DM2 para el trienio correspondiente a la ENFR",
    "Defunciones promedio por DM2 para el trienio correspondiente a la ENFR",
    "Error estándar de las defunciones promedio por DM2 para el trienio correspondiente a la ENFR",
    "Proyección poblacional según sexo y grupo etario decenal",
    "Población estándar según sexo y grupo etario decenal según Censo Nacional 2010",
    "Esperanza de vida a la edad X según sexo y grupo etario decenal",
    "Peso de discapacidad ponderado para DM2 con neuropatía periférica",
    "Peso de discapacidad ponderado para DM2 con retinopatía no proliferativa",
    "Peso de discapacidad ponderado para DM2 con retinopatía proliferativa",
    "Peso de discapacidad ponderado para DM2 con disfunción eréctil",
    "Peso de discapacidad ponderado para DM2 con amputación",
    "Peso de discapacidad ponderado para DM2 con ceguera",
    "Peso de discapacidad ponderado para DM2 con nefropatía",
    "Peso de discapacidad ponderado para DM2 con claudicación de miembros inferiores",
    "Peso de discapacidad ponderado para DM2 con ACV",
    "Peso de discapacidad ponderado para DM2 con IAM",
    "Peso de discapacidad ponderado para DM2 con IC",
    "Peso de discapacidad ponderado para DM2 sin secuelas",
    "Peso de discapacidad ponderado para DM2 total"
  )
)

## Guardar
export(dicc_dm2, file = "datos_limpios/dicc_arg_datos_dm2.xlsx")


## Dataset AVP, AVD y AVAD simulados -----
dicc_dm2_sim <- tibble(
  variable = names(sim_avad_dm2),
  tipo_var = map_chr(sim_avad_dm2, ~ paste(class(.x), collapse = ", ")),
  niveles = map_chr(
    sim_avad_dm2,
    ~ if (is.factor(.x)) {
      paste(levels(.x), collapse = ", ")
    } else {
      "0-Inf"
    }
  ),
  descripcion = c(
    "Año de realización de la Encuesta Nacional de Factores de Riesgo (ENFR)",
    "Sexo asignado al nacer",
    "Grupo etario decenal",
    "Prevalencia estimada de personas con DM2 por autorreporte según resultados ENFR",
    "Error estándar de la prevalencia estimada de personas con DM2 por autorreporte según resultados ENFR",
    "Coeficiente de variación de la prevalencia estimada de personas con DM2 por autorreporte según resultados ENFR",
    "Proyección poblacional según sexo y grupo etario decenal",
    "Población estándar según sexo y grupo etario decenal según Censo Nacional 2010",
    
    "Años de vida perdidos por muerte prematura (AVP) por DM2",
    "Límite inferior intervalo de incertidumbre AVP por DM2",
    "Límite superior intervalo de incertidumbre AVP por DM2",
    "Tasa AVP por DM2",
    "Límite inferior intervalo de incertidumbre tasa AVP por DM2",
    "Límite superior intervalo de incertidumbre tasa AVP por DM2",
    "Años vividos con discapacidad (AVD) por DM2",
    "Límite inferior intervalo de incertidumbre AVD por DM2",
    "Límite superior intervalo de incertidumbre AVD por DM2",
    "Tasa AVD por DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por DM2",
    "Años de vida ajustados por discapacidad (AVAD) por DM2",
    "Límite inferior intervalo de incertidumbre AVAD por DM2",
    "Límite superior intervalo de incertidumbre AVAD por DM2",
    "Tasa AVAD por DM2",
    "Límite inferior intervalo de incertidumbre tasa AVAD por DM2",
    "Límite superior intervalo de incertidumbre tasa AVAD por DM2",

    "Años vividos con discapacidad (AVD) por complicaciones microvasculares de DM2",
    "Límite inferior intervalo de incertidumbre AVD por complicaciones microvasculares de DM2",
    "Límite superior intervalo de incertidumbre AVD por complicaciones microvasculares de DM2",
    "Tasa AVD por complicaciones microvasculares de DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por complicaciones microvasculares de DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por complicaciones microvasculares de DM2",
    
    "Años vividos con discapacidad (AVD) por complicaciones macrovasculares de DM2",
    "Límite inferior intervalo de incertidumbre AVD por complicaciones macrovasculares de DM2",
    "Límite superior intervalo de incertidumbre AVD por complicaciones macrovasculares de DM2",
    "Tasa AVD por complicaciones macrovasculares de DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por complicaciones macrovasculares de DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por complicaciones macrovasculares de DM2",
    
    "Años vividos con discapacidad (AVD) por neuropatía periférica asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por neuropatía periférica asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por neuropatía periférica asociada a DM2",
    "Tasa AVD por neuropatía periférica asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por neuropatía periférica asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por neuropatía periférica asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por retinopatía no proliferativa asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por retinopatía no proliferativa asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por retinopatía no proliferativa asociada a DM2",
    "Tasa AVD por retinopatía no proliferativa asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por retinopatía no proliferativa asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por retinopatía no proliferativa asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por retinopatía proliferativa asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por retinopatía proliferativa asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por retinopatía proliferativa asociada a DM2",
    "Tasa AVD por retinopatía proliferativa asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por retinopatía proliferativa asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por retinopatía proliferativa asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por disfunción eréctil asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por disfunción eréctil asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por disfunción eréctil asociada a DM2",
    "Tasa AVD por disfunción eréctil asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por disfunción eréctil asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por disfunción eréctil asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por nefropatía asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por nefropatía asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por nefropatía asociada a DM2",
    "Tasa AVD por nefropatía asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por nefropatía asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por nefropatía asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por ceguera asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por ceguera asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por ceguera asociada a DM2",
    "Tasa AVD por ceguera asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por ceguera asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por ceguera asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por amputación asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por amputación asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por amputación asociada a DM2",
    "Tasa AVD por amputación asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por amputación asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por amputación asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por claudicación de miembros inferiores asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por claudicación de miembros inferiores asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por claudicación de miembros inferiores asociada a DM2",
    "Tasa AVD por claudicación de miembros inferiores asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por claudicación de miembros inferiores asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por claudicación de miembros inferiores asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por accidente cerebrovascular asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por accidente cerebrovascular asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por accidente cerebrovascular asociada a DM2",
    "Tasa AVD por accidente cerebrovascular asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por accidente cerebrovascular asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por accidente cerebrovascular asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por infarto agudo de miocardio asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por infarto agudo de miocardio asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por infarto agudo de miocardio asociada a DM2",
    "Tasa AVD por infarto agudo de miocardio asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por infarto agudo de miocardio asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por infarto agudo de miocardio asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por insuficiencia cardíaca asociada a DM2",
    "Límite inferior intervalo de incertidumbre AVD por insuficiencia cardíaca asociada a DM2",
    "Límite superior intervalo de incertidumbre AVD por insuficiencia cardíaca asociada a DM2",
    "Tasa AVD por insuficiencia cardíaca asociada a DM2",
    "Límite inferior intervalo de incertidumbre tasa AVD por insuficiencia cardíaca asociada a DM2",
    "Límite superior intervalo de incertidumbre tasa AVD por insuficiencia cardíaca asociada a DM2",
    
    "Años vividos con discapacidad (AVD) por DM2 sin secuelas",
    "Límite inferior intervalo de incertidumbre AVD por DM2 sin secuelas",
    "Límite superior intervalo de incertidumbre AVD por DM2 sin secuelas",
    "Tasa AVD por DM2 sin secuelas",
    "Límite inferior intervalo de incertidumbre tasa AVD por DM2 sin secuelas",
    "Límite superior intervalo de incertidumbre tasa AVD por DM2 sin secuelas"
    
  )
)

## Guardar
export(dicc_dm2_sim, file = "datos_limpios/dicc_arg_sim_avad.xlsx")


# Limpiar environment ----------------------------------------------------
rm(list = ls())

pacman::p_unload("all")

