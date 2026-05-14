### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
###  en Argentina, período 2005-2018
### Cálculo de AVP, AVD y AVAD e intervalos de incertidumbre mediante cadenas de
### Monte-Carlo con 10.000 réplicas. Se usaron las siguientes simulaciones:
## - Defunciones: distribución normal truncada en cero, con media igual al
## valor estimado y SD aproximada por sqrt(mu/3).
## - Prevalencia DM2: se simularon con una normal truncada en [0,1], con media
## igual a la estimación puntual y desviación estándar igual a su error estándar.
### Cálculo de tasas estandarizadas AVP, AVD y AVAD e intervalos de incertidumbre (IU)
### mediante cadenas de Monte-Carlo con 10.000 réplicas.
### Autoras:
## - Micaela Gauto
## - Tamara Ricardo
### Fecha de creación: 27-01-2026
# Última modificación: 14-05-2026 09:24

# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  # Simulaciones de Monte-Carlo
  truncnorm,
  # Manejo de datos
  rio,
  janitor,
  tidyverse
)


# Cargar datos -----------------------------------------------------------
## Dataset DM2
datos_dm2 <- import("datos_limpios/arg_datos_dm2.rds")

# Población estándar 2010
pob_est_2010 <- import("datos_limpios/pob_est_2010.rds")


# Funciones auxiliares ---------------------------------------------------
source("fun_auxiliares.R")


# Simular AVP, AVD y AVAD ------------------------------------------------
## AVD total, total microvascular y total macrovascular -----
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
## Año y sexo ----
abs_avad_dm2 <- sim_avad_dm2 %>%
  group_by(anio_enfr, sexo) %>%
  summarise(
    AVAD = sum(AVAD),
    AVD = sum(AVD),
    AVP = sum(AVP)
  )


# Simular tasas estandarizadas -------------------------------------------
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

# Guardar datos ----------------------------------------------------------
## AVP, AVD, AVAD y tasas
export(sim_avad_dm2, file = "datos_limpios/arg_sim_avad.rds")

## Tasas estandarizadas
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
