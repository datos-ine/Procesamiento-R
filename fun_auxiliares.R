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


# Simulaciones Monte-Carlo para AVD ---------------------------------------
# Por complicación, tipo de complicación y AVD totales
sim_AVD_multi <- function(
    n,
    n_se,
    fwd_vec,
    proy_pob,
    cols_micro,
    cols_macro,
    nsim = 10000
) {
  n_sd <- dplyr::if_else(n > 0, n_se, 1e-6)
  
  prev_sim <- truncnorm::rtruncnorm(
    n    = nsim,
    a    = 0,
    mean = n,
    sd   = n_sd
  )
  
  ## AVD por complicación individual ##
  resultados <- purrr::map(fwd_vec, function(fwd) {
    
    # Si fwd es NA o 0, devuelve ceros directamente sin simular
    if (is.na(fwd) || fwd == 0) {
      AVD_sim   <- rep(0, nsim)
      AVD_t_sim <- rep(0, nsim)
    } else {
      AVD_sim   <- prev_sim * fwd
      AVD_t_sim <- (AVD_sim / proy_pob) * 1e5
    }
    
    list(
      sim      = AVD_sim,
      tasa_sim = AVD_t_sim,
      resumen  = dplyr::bind_cols(
        resumen_ic(AVD_sim,   "AVD"),
        resumen_ic(AVD_t_sim, "AVD_tasa")
      )
    )
  })
  
  ## Función interna para sumar grupos ##
  sumar_grupo <- function(nombres) {
    sims <- purrr::map(nombres, \(nm) {
      s <- resultados[[nm]]$sim
      # Si la simulación es NA (fwd ausente), reemplaza por vector de ceros
      if (all(is.na(s))) rep(0, length(s)) else s
    }) |>
      purrr::reduce(`+`)
    tasa <- (sims / proy_pob) * 1e5
    list(
      sim      = sims,
      tasa_sim = tasa,
      resumen  = dplyr::bind_cols(
        resumen_ic(sims, "AVD"),
        resumen_ic(tasa, "AVD_tasa")
      )
    )
  }
  
  ## Totales derivados ##
  resultados[["total_micro"]] <- sumar_grupo(cols_micro)
  resultados[["total_macro"]] <- sumar_grupo(cols_macro)
  resultados[["total_compl"]] <- sumar_grupo(
    c(cols_micro, cols_macro, "sin_complicaciones")
  )
  
  resultados
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


# # Simulaciones Monte-Carlo para AVD --------------------------------------
# sim_AVD <- function(
    #   n,
#   n_se,
#   fwd,
#   proy_pob,
#   nsim = 10000
# ) {
#   ## SD robustos ##
#   n_sd <- dplyr::if_else(n > 0, n_se, 1e-6)
# 
#   ## Simular prevalencia ##
#   prev_sim <- truncnorm::rtruncnorm(
#     n = nsim,
#     a = 0,
#     mean = n,
#     sd = n_sd
#   )
# 
#   ## Simular AVD ##
#   AVD_sim <- prev_sim * fwd
# 
#   ## Simular tasa AVD ##
#   AVD_t_sim <- (AVD_sim / proy_pob) * 1e5
# 
#   ## Devolver resultados ##
#   list(
#     sim = AVD_sim,
#     tasa_sim = AVD_t_sim,
#     resumen = dplyr::bind_cols(
#       resumen_ic(AVD_sim, "AVD"),
#       resumen_ic(AVD_t_sim, "AVD_tasa")
#     )
#   )
# }
