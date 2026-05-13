### Funciones auxiliares para limpieza de datos y estimación de AVP, AVD y AVAD
### usando simulaciones de Monte-Carlo
### Autoras:
### - Tamara Ricardo
### - Micaela Gauto
# Última modificación: 13-05-2026 15:03

# Limpiar datos ENFR -----------------------------------------------------
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


# Simulaciones Monte-Carlo para AVP, AVD y AVAD --------------------------
sim_AVAD <- function(
  dm2_total,
  dm2_total_se,
  fwd,
  defun_mean,
  defun_se,
  ex,
  proy_pob,
  nsim = 10000
) {
  # SD robustos
  dm2_sd <- dplyr::if_else(dm2_total > 0, dm2_total_se, 1e-6)
  defun_sd <- dplyr::if_else(defun_mean > 0, defun_se, 1e-6)

  # Simulaciones
  dm2_sim <- truncnorm::rtruncnorm(
    n = nsim,
    a = 0,
    mean = dm2_total,
    sd = dm2_sd
  )

  defun_sim <- truncnorm::rtruncnorm(
    n = nsim,
    a = 0,
    mean = defun_mean,
    sd = defun_sd
  )

  # Métricas
  AVP_sim <- defun_sim * ex
  AVD_sim <- dm2_sim * fwd
  AVAD_sim <- AVP_sim + AVD_sim

  # Tasas
  AVP_t_sim <- AVP_sim / proy_pob * 1e5
  AVD_t_sim <- AVD_sim / proy_pob * 1e5
  AVAD_t_sim <- AVAD_sim / proy_pob * 1e5

  sims <- list(
    AVP = AVP_sim,
    AVD = AVD_sim,
    AVAD = AVAD_sim,
    AVP_tasa = AVP_t_sim,
    AVD_tasa = AVD_t_sim,
    AVAD_tasa = AVAD_t_sim
  )

  # función auxiliar
  resumen_ic <- function(x) {
    c(
      est = quantile(x, 0.50, na.rm = TRUE),
      low = quantile(x, 0.025, na.rm = TRUE),
      upp = quantile(x, 0.975, na.rm = TRUE)
    )
  }

  resumen <- purrr::imap_dfc(
    sims,
    \(x, nombre) {
      q <- resumen_ic(x)

      tibble::tibble(
        !!nombre := q["est"],
        !!paste0(nombre, "_low") := q["low"],
        !!paste0(nombre, "_upp") := q["upp"]
      )
    }
  )

  # devolver TODO
  list(
    sim_raw = list(
      AVP_t_sim = AVP_t_sim,
      AVD_t_sim = AVD_t_sim,
      AVAD_t_sim = AVAD_t_sim
    ),
    resumen = resumen
  )
}


# Tasas estandarizadas con IU --------------------------------------------
tasa_est_AVAD <- function(df, pob_est) {
  # ordenar grupos etarios
  df <- df |>
    dplyr::arrange(grupo_edad_10)

  # pesos normalizados
  w <- df |>
    dplyr::pull({{ pob_est }}) |>
    (\(x) x / sum(x, na.rm = TRUE))()

  # matrices nsim x edad
  tasas_mat <- list(
    AVP = do.call(cbind, lapply(df$sim_raw, \(x) x$AVP_t_sim)),
    AVD = do.call(cbind, lapply(df$sim_raw, \(x) x$AVD_t_sim)),
    AVAD = do.call(cbind, lapply(df$sim_raw, \(x) x$AVAD_t_sim))
  )

  # estandarización directa
  tasas_std <- purrr::map(
    tasas_mat,
    \(m) as.numeric(m %*% w)
  )

  # resumen IC
  resumen_ic <- function(x, nombre) {
    tibble::tibble(
      !!paste0(nombre, "_tasa_std") := quantile(x, 0.50, na.rm = TRUE),

      !!paste0(nombre, "_tasa_std_low") := quantile(x, 0.025, na.rm = TRUE),

      !!paste0(nombre, "_tasa_std_upp") := quantile(x, 0.975, na.rm = TRUE)
    )
  }

  purrr::imap_dfc(tasas_std, resumen_ic)
}


## Simulaciones de Monte-Carlo para AVD por cada complicación ------------
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

## AVD por complicación con IU -------------------------------------------
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
