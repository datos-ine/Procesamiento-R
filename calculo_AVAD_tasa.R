### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
###  en Argentina, período 2005-2018
### Cálculo de AVP, AVD, AVAD y tasas por 100.000 hab para DM2 en Argentina
### para los periodos correspondientes a las cuatro ENF (2005, 2009, 2013, 2018)
### Para obtener los intervalos de incertidumbre (IU) se usaron simulaciones de
### Monte-Carlo (10.000 iteraciones por estrato) según Torgerson et al. (2015):
## - Defunciones: distribución normal truncada en cero, con media igual al
## valor estimado y SD aproximada por sqrt(mu/3).
## - Prevalencia DM2: se simularon con una normal truncada en [0,1], con media
## igual a la estimación puntual y desviación estándar igual a su error estándar.
## - Pesos de discapacidad: se consideraron fijos (Qualidiab 2014).
## En cada iteración se calcularon AVP, AVD y AVAD y se obtuvieron la mediana
##  y los percentiles 2,5 y 97,5 como intervalos de incertidumbre (IU).
## Las tasas se derivaron en cada iteración dividiendo por la proyección
## poblacional correspondiente y multiplicando por 100.000 habitantes.
### Autoras:
## - Micaela Gauto
## - Tamara Ricardo

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  truncnorm,
  skimr,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
## Prevalencia DM2 y población por provincia, sexo y grupo etario ----
prev_dm2_prov <- import("datos_limpios/arg_dm2_ge10_prov.rds")

## Prevalencia DM2 y población por región, sexo y grupo etario ----
prev_dm2_reg <- import("datos_limpios/arg_dm2_ge10_reg.rds")


## Defunciones por provincia, sexo y grupo etario ----
defun_dm2_prov <- import("datos_limpios/arg_defun_ge10_prov.rds")

## Defunciones por región, sexo y grupo etario ----
defun_dm2_reg <- import("datos_limpios/arg_defun_ge10_reg.rds")


## Secuelas DM2 por sexo y grupo etario (Qualidiab 2014) ----
comp_dm2 <- import("datos_limpios/fr_comp_DW_ge10.csv")


## Población estándar 2010 ----
pob_est_2010 <- import("datos_limpios/arg_pob_est_2010.rds")


# Explorar datos ---------------------------------------------------------
skim(prev_dm2_prov)

skim(prev_dm2_reg)

skim(defun_dm2_prov)

skim(defun_dm2_reg)


# Función para simulaciones de Monte-Carlo -------------------------------
sim_IU <- function(
  defun_mean,
  ex,
  dm2_prev,
  dm2_prev_se,
  proy_pob,
  fwd,
  nsim = 10000
) {
  # Caso todo cero
  if (all(c(defun_mean, dm2_prev) == 0, na.rm = TRUE)) {
    return(tibble(
      # AVP
      AVP_iu_i = 0,
      AVP = 0,
      AVP_iu_s = 0,

      # AVD
      AVD_iu_i = 0,
      AVD = 0,
      AVD_iu_s = 0,

      # AVAD
      AVAD_iu_i = 0,
      AVAD = 0,
      AVAD_iu_s = 0,

      # Tasa AVP
      AVP_tasa_iu_i = 0,
      AVP_tasa = 0,
      AVP_tasa_iu_s = 0,

      # Tasa AVD
      AVD_tasa_iu_i = 0,
      AVD_tasa = 0,
      AVD_tasa_iu_s = 0,

      # Tasa AVAD
      AVAD_tasa_iu_i = 0,
      AVAD_tasa = 0,
      AVAD_tasa_iu_s = 0
    ))
  }

  # SD seguras
  sd_def <- ifelse(defun_mean > 0, sqrt(defun_mean / 3), 1e-6)
  sd_prev <- ifelse(dm2_prev_se > 0, dm2_prev_se, 1e-6)

  # Simular defunciones
  def_sim <- rtruncnorm(nsim, a = 0, mean = defun_mean, sd = sd_def)

  # Simular prevalencia DM2
  prev_sim <- rtruncnorm(
    nsim,
    a = 0,
    b = 1,
    mean = dm2_prev,
    sd = sd_prev
  )

  # AVP
  AVP_sim <- def_sim * ex

  # AVD
  AVD_sim <- prev_sim * proy_pob * fwd

  # AVAD
  AVAD_sim <- AVP_sim + AVD_sim

  # Tasa AVP
  AVP_tasa_sim <- AVP_sim / proy_pob * 1e5

  # Tasa AVD
  AVD_tasa_sim <- AVD_sim / proy_pob * 1e5

  # Tasa AVAD
  AVAD_tasa_sim <- AVAD_sim / proy_pob * 1e5

  tibble(
    AVP_iu_i = quantile(AVP_sim, .025),
    AVP = quantile(AVP_sim, .05),
    AVP_iu_s = quantile(AVP_sim, .975),

    AVD_iu_i = quantile(AVD_sim, .025),
    AVD = quantile(AVD_sim, .05),
    AVD_iu_s = quantile(AVD_sim, .975),

    AVAD_iu_i = quantile(AVAD_sim, .025),
    AVAD = quantile(AVAD_sim, .05),
    AVAD_iu_s = quantile(AVAD_sim, .975),

    AVP_tasa_iu_i = quantile(AVP_tasa_sim, .025),
    AVP_tasa = quantile(AVP_tasa_sim, .05),
    AVP_tasa_iu_s = quantile(AVP_tasa_sim, .975),

    AVD_tasa_iu_i = quantile(AVD_tasa_sim, .025),
    AVD_tasa = quantile(AVD_tasa_sim, .05),
    AVD_tasa_iu_s = quantile(AVD_tasa_sim, .975),

    AVAD_tasa_iu_i = quantile(AVAD_tasa_sim, .025),
    AVAD_tasa = quantile(AVAD_tasa_sim, .05),
    AVAD_tasa_iu_s = quantile(AVAD_tasa_sim, .975),
  )
}


# Simular AVP, AVD y AVAD por provincia, sexo y grupo etario -------------
set.seed(123)

AVAD_dm2_prov <- prev_dm2_prov |>
  # Añadir datos mortalidad y esperanza de vida
  left_join(defun_dm2_prov) |>

  # Añadir frecuencia de complicaciones DM2 por sexo y grupo etario
  left_join(
    comp_dm2 |>
      # Calcular promedio ponderado de discapacidad (fwd)
      group_by(sexo, grupo_edad10) |>
      summarise(
        fwd = sum(comp_frec * dw, na.rm = TRUE),
        .groups = "drop"
      )
  ) |>

  # Calcular AVP, AVD, AVAD, tasas e intervalos de incertidumbre
  (\(x) {
    bind_cols(
      x,
      pmap_dfr(
        x |>
          select(defun_mean, ex, dm2_prev, dm2_prev_se, proy_pob, fwd),
        sim_IU
      )
    )
  })() |>

  # Redondear variables numéricas
  mutate(across(
    .cols = where(is.numeric),
    .fns = ~ round(.x, 2)
  )) |>

  # Variables caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  # Reordenar columnas
  select(
    anio_enfr:grupo_edad10,
    starts_with("dm2"),
    defun_n:fwd,
    AVP,
    starts_with("AVP_iu"),
    AVD,
    starts_with("AVD_iu"),
    AVAD,
    starts_with("AVAD_iu"),
    AVP_tasa,
    starts_with("AVP_tasa"),
    AVD_tasa,
    starts_with("AVD_tasa"),
    AVAD_tasa,
    starts_with("AVAD_tasa"),
    -lx,
    -Tx
  )


# Simular AVP, AVD y AVAD por región, sexo y grupo etario ----------------
set.seed(123)

AVAD_dm2_reg <- prev_dm2_reg |>
  # Añadir datos mortalidad y esperanza de vida
  left_join(defun_dm2_reg) |>

  # Añadir frecuencia de complicaciones DM2 por sexo y grupo etario
  left_join(
    comp_dm2 |>
      # Calcular promedio ponderado de discapacidad (fwd)
      group_by(sexo, grupo_edad10) |>
      summarise(
        fwd = sum(comp_frec * dw, na.rm = TRUE),
        .groups = "drop"
      )
  ) |>

  # Calcular AVP, AVD, AVAD, tasas e intervalos de incertidumbre
  (\(x) {
    bind_cols(
      x,
      pmap_dfr(
        x |>
          select(defun_mean, ex, dm2_prev, dm2_prev_se, proy_pob, fwd),
        sim_IU
      )
    )
  })() |>

  # Redondear variables numéricas
  mutate(across(
    .cols = where(is.numeric),
    .fns = ~ round(.x, 2)
  )) |>

  # Variables caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  # Reordenar columnas
  select(
    anio_enfr:grupo_edad10,
    starts_with("dm2"),
    defun_n:fwd,
    AVP,
    starts_with("AVP_iu"),
    AVD,
    starts_with("AVD_iu"),
    AVAD,
    starts_with("AVAD_iu"),
    AVP_tasa,
    starts_with("AVP_tasa"),
    AVD_tasa,
    starts_with("AVD_tasa"),
    AVAD_tasa,
    starts_with("AVAD_tasa"),
    -lx,
    -Tx
  )


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = names(AVAD_dm2_prov),

  descripcion = c(
    "Año de realización de la ENFR",
    "Identificador numérico de provincia según clasificación INDEC",
    "Identificador categórico de la provincia",
    "Proyección poblacional para el año de la ENFR",
    "Región geográfica según clasificación DEIS (2021)",
    "Sexo biológico",
    "Grupo de edad decenal",
    "Total estimado de personas con DM2 por autorreporte por provincia, grupo etario y sexo",
    "Error estándar del total estimado de personas con DM2 por autorreporte por provincia, grupo etario y sexo",
    "Prevalencia de DM2 por autorreporte por provincia, grupo etario y sexo",
    "Error estándar del total de la prevalencia de personas con DM2 por autorreporte",
    "Coeficiente de variación de la prevalencia de personas con DM2 por autorreporte",
    "Número de defunciones por DM2 para el trienio correspondiente",
    "Promedio de defunciones por DM2 para el trienio correspondiente",
    "Esperanza de vida a la edad x",
    "Peso de discapacidad ponderado para complicaciones de DM2",
    "Años de vida perdidos por fallecimiento prematuro por DM2",
    "Límite inferior del intervalo de incertidumbre de los AVP por DM2",
    "Límite superior del intervalo de incertidumbre de los AVP por DM2",
    "Años vividos con discapacidad por DM2",
    "Límite inferior del intervalo de incertidumbre de los AVD por DM2",
    "Límite superior del intervalo de incertidumbre de los AVD por DM2",
    "Años de vida ajustados por discapacidad por DM2",
    "Límite inferior del intervalo de incertidumbre de los AVAD por DM2",
    "Límite superior del intervalo de incertidumbre de los AVAD por DM2",
    "Tasa de AVP por DM2 por 100.000 habitantes",
    "Límite inferior del intervalo de incertidumbre de la tasa de AVP por DM2",
    "Límite superior del intervalo de incertidumbre de la tasa de AVP por DM2",
    "Tasa de AVD por DM2 por 100.000 habitantes",
    "Límite inferior del intervalo de incertidumbre de la tasa de AVD por DM2",
    "Límite superior del intervalo de incertidumbre de la tasa de AVD por DM2",
    "Tasa de AVAD por DM2 por 100.000 habitantes",
    "Límite inferior del intervalo de incertidumbre de la tasa de AVAD por DM2",
    "Límite superior del intervalo de incertidumbre de la tasa de AVAD por DM2"
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


# Guardar datos limpios ---------------------------------------------------
## AVAD por provincia
export(AVAD_dm2_prov, file = "datos_limpios/arg_AVAD_ge10_prov.xlsx")

## AVAD por región
export(AVAD_dm2_reg, file = "datos_limpios/arg_AVAD_ge10_reg.xlsx")

## Diccionario de datos
export(data_dict, file = "datos_limpios/dic_arg_AVAD_ge10.xlsx")

# Limpiar environment y desactivar paquetes ------------------------------
rm(list = ls())

pacman::p_unload("all")
