### Funciones auxiliares para limpieza de datos y estimación de AVP, AVD y AVAD
### usando simulaciones de Monte-Carlo
### Autoras:
### - Tamara Ricardo
### - Micaela Gauto
### Fecha de creación: 27-01-2026
# Última modificación: 14-05-2026 09:22

# Resumen indicadores ----------------------------------------------------
resumen_ic <- function(x, nombre) {
  tibble(
    !!nombre := quantile(x, 0.50, na.rm = TRUE),
    !!paste0(nombre, "_iu_l") := quantile(x, 0.025, na.rm = TRUE),
    !!paste0(nombre, "_iu_u") := quantile(x, 0.975, na.rm = TRUE)
  )
}


# Simulaciones Monte-Carlo para AVP --------------------------------------
sim_AVP <- function(
  defun_mean,
  defun_se,
  ex,
  proy_pob,
  nsim = 10000
) {
  ## SD robustos ##
  defun_sd <- dplyr::if_else(defun_mean > 0, defun_se, 1e-6)

  ## Simular defunciones ##
  defun_sim <- truncnorm::rtruncnorm(
    n = nsim,
    a = 0,
    mean = defun_mean,
    sd = defun_sd
  )

  ## Simular AVP ##
  AVP_sim <- defun_sim * ex

  ## Simular tasa AVP ##
  AVP_t_sim <- (AVP_sim / proy_pob) * 1e5

  ## Devolver resultados ##
  list(
    sim = AVP_sim,
    tasa_sim = AVP_t_sim,
    resumen = dplyr::bind_cols(
      resumen_ic(AVP_sim, "AVP"),
      resumen_ic(AVP_t_sim, "AVP_tasa")
    )
  )
}


# Simulaciones Monte-Carlo para AVD --------------------------------------
sim_AVD <- function(
  n,
  n_se,
  fwd,
  proy_pob,
  nsim = 10000
) {
  ## SD robustos ##
  n_sd <- dplyr::if_else(n > 0, n_se, 1e-6)

  ## Simular prevalencia ##
  prev_sim <- truncnorm::rtruncnorm(
    n = nsim,
    a = 0,
    mean = n,
    sd = n_sd
  )

  ## Simular AVD ##
  AVD_sim <- prev_sim * fwd

  ## Simular tasa AVD ##
  AVD_t_sim <- (AVD_sim / proy_pob) * 1e5

  ## Devolver resultados ##
  list(
    sim = AVD_sim,
    tasa_sim = AVD_t_sim,
    resumen = dplyr::bind_cols(
      resumen_ic(AVD_sim, "AVD"),
      resumen_ic(AVD_t_sim, "AVD_tasa")
    )
  )
}


# Simulaciones Monte-Carlo para AVAD -------------------------------------
sim_AVAD <- function(avp, avd) {
  ## Simular AVAD ##
  AVAD_sim <- avp$sim + avd$sim

  ## Simular tasa AVAD ##
  AVAD_t_sim <- avp$tasa_sim + avd$tasa_sim

  ## Devolver resultados ##
  list(
    sim = AVAD_sim,
    tasa_sim = AVAD_t_sim,
    resumen = dplyr::bind_cols(
      resumen_ic(AVAD_sim, "AVAD"),
      resumen_ic(AVAD_t_sim, "AVAD_tasa")
    )
  )
}


# Tasas estandarizadas con IU --------------------------------------------
tasa_est <- function(df, sim_col, nombre, pob_est) {
  # ordenar edades
  df <- df |>
    arrange(grupo_edad_10)

  # pesos
  w <- df |>
    pull({{ pob_est }}) |>
    (\(x) x / sum(x, na.rm = TRUE))()

  # matriz nsim x edad
  mat <- do.call(
    cbind,
    lapply(df[[sim_col]], \(x) x$tasa_sim)
  )

  # tasa estandarizada por réplica
  tasa_std <- as.numeric(mat %*% w)

  # resumen
  tibble(
    !!paste0(nombre, "_tasa_std") := quantile(tasa_std, 0.50, na.rm = TRUE),

    !!paste0(nombre, "_tasa_std_low") := quantile(
      tasa_std,
      0.025,
      na.rm = TRUE
    ),

    !!paste0(nombre, "_tasa_std_upp") := quantile(tasa_std, 0.975, na.rm = TRUE)
  )
}

# ## Simulaciones de Monte-Carlo para AVD por cada complicación ------------
# sim_AVD_comp <- function(
#   dm2_total,
#   dm2_total_se,
#   fwd,
#   proy_pob,
#   nsim = 10000
# ) {
#   # SDs robustos cuando no hay casos
#   dm2_sd <- if_else(dm2_total > 0, dm2_total_se, 1e-6)

#   # Simular casos (truncados en 0)
#   dm2_sim <- rtruncnorm(
#     n = nsim,
#     a = 0,
#     mean = dm2_total,
#     sd = dm2_sd
#   )

#   # AVP, AVD, AVAD
#   AVD_sim <- dm2_sim * fwd

#   # devolver lista con nombres fijos
#   list(
#     AVD_sim = AVD_sim
#   )
# }

# ## AVD por complicación con IU -------------------------------------------
# sim_AVD_IU_ind <- function(
#   dm2_total,
#   dm2_total_se,
#   fwd,
#   proy_pob,
#   nsim = 10000
# ) {
#   sims <- sim_AVD_comp(
#     dm2_total,
#     dm2_total_se,
#     fwd,
#     proy_pob,
#     nsim
#   )

#   tibble(
#     AVD = quantile(sims$AVD_sim, 0.50, na.rm = TRUE),
#     AVD_low = quantile(sims$AVD_sim, 0.025, na.rm = TRUE),
#     AVD_upp = quantile(sims$AVD_sim, 0.975, na.rm = TRUE)
#   )
# }
