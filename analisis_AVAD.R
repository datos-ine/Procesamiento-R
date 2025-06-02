### Cálculo de AVP, AVD y AVAD para diabetes mellitus tipo 2 (DM2) en Argentina
### para los periodos correspondientes a las cuatro Encuestas Nacionales de
### Factores de Riesgo (2005, 2009, 2013 y 2018).
### Autoras: Micaela Gauto y Tamara Ricardo
### Fecha modificacion:
# Mon Jun  2 13:02:26 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  apyramid,
  gghighlight,
  scico,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
## Prevalencia DM por grupos quinquenales de edad
prev_dm_ge5 <- read_csv("Bases de datos/clean/arg_prev_dm_ge5.csv")

## Prevalencia DM por grupos decenales de edad
prev_dm_ge10 <- read_csv("Bases de datos/clean/arg_prev_dm_ge10.csv")


## Secuelas DM2 (datos temporales)
comp_dm <- read_csv("Bases de datos/clean/comp_dm_temp.csv") |> 
  drop_na()


## AVP por grupos quinquenales de edad
AVP_ge5 <- read_csv("Bases de datos/clean/arg_defun_avp_ge5.csv")


## Proyecciones INDEC 2005-2018 por grupos quinquenales de edad
proy_ge5 <- read_csv("Bases de datos/clean/arg_proy_2005_2018_ge5.csv")



# Calcular AVP y proyecciones por grupo decenal ---------------------------
## AVP
AVP_ge10 <- AVP_ge5 |> 
  # Estimar esperanza vida y AVP por grupo decenal de edad
    group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo) |>
    summarise(defun_n = sum(defun_n, na.rm = TRUE),
              defun_mean = mean(defun_n, na.rm = TRUE),
              ex = weighted.mean(ex, lx, na.rm = TRUE),
              AVP = defun_mean * ex,
              .groups = "drop")

  
# Proyección poblacional
proy_ge10 <- proy_ge5 |> 
  # Estimar población por provincia, sexo y grupo decenal de edad
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(proy_pob = sum(proy_pob, na.rm = TRUE),
         pob_est_2010 = sum(pob_est_2010, na.rm = TRUE),
         .groups = "drop")


# Calcular AVAD -----------------------------------------------------------
## Por provincia, sexo y grupos quinquenales de edad
AVAD_ge5 <- cross_join(prev_dm_ge5, comp_dm) |> 
  # Calcular AVD por cada secuela
  mutate(AVD_ind = dm_total * frec_wandurranga/ (100 * dw)) |> 
  
  # Calcular AVD totales
  group_by(across(-c(sequela:AVD_ind))) |> 
  summarise(AVD = sum(AVD_ind, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir datos defunciones, esperanza de vida y AVP
  left_join(AVP_ge5) |> 
  
  # Añadir proyecciones poblacionales INDEC
  left_join(select(proy_ge5, -anio))  |> 
  
  # Calcular AVAD
  mutate(AVAD = AVD + AVP) |> 
  
  # Calcular tasas específicas y ajustadas AVP, AVD y AVAD
  mutate(across(.cols = c(AVD, AVP, AVAD),
                .fns = list(tasa = ~ 100000 * .x/proy_pob,
                            tasa_est = ~ 100000 * .x/pob_est_2010))) |> 
  
  # Variables caracter a factor
  mutate(across(.cols = where(is.character),  
                .fn = ~ factor(.x))) 
  

## Por provincia, sexo y grupos decenales de edad
AVAD_ge10 <- cross_join(prev_dm_ge10, comp_dm) |> 
  # Calcular AVD por cada secuela
  mutate(AVD_ind = dm_total * frec_wandurranga/ (100 * dw)) |> 
  
  # Calcular AVD totales
  group_by(across(-c(sequela:AVD_ind))) |> 
  summarise(AVD = sum(AVD_ind, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir datos defunciones, esperanza de vida y AVP
  left_join(AVP_ge10) |> 
  
  # Añadir proyecciones poblacionales INDEC
  left_join(proy_ge10)  |> 
  
  # Calcular AVAD
  mutate(AVAD = AVD + AVP) |> 
  
  # Calcular tasas específicas y ajustadas AVP, AVD y AVAD
  mutate(across(.cols = c(AVD, AVP, AVAD),
                .fns = list(tasa = ~ 100000 * .x/proy_pob,
                            tasa_est = ~ 100000 * .x/pob_est_2010))) |> 
  
  # Variables caracter a factor
  mutate(across(.cols = where(is.character),  
                .fn = ~ factor(.x))) 


# Gráficos exploratorios --------------------------------------------------
## Pirámide poblacional por año, grupo quinquenal de edad y sexo
AVAD_ge5 |>
  # Tasa AVAD nacional
  count(anio_enfr, grupo_edad_5, sexo,
        wt = AVAD_tasa, name = "AVAD_tasa")  |> 
  
 # Pirámide
   age_pyramid(age_group = grupo_edad_5,
              split_by = sexo,
              count = AVAD_tasa,
              show_midpoint = FALSE) +
  
  # Dividir por año ENFR
  facet_wrap(~ anio_enfr) +
  
  # Colores
  scale_fill_scico_d(palette = "hawaii") +
  
  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Tasa AVAD") +
  
  # Tema
  theme_minimal()


## Pirámide poblacional por año, grupo decenal de edad y sexo
AVAD_ge10 |>
  # Tasa AVAD nacional
  count(anio_enfr, grupo_edad_10, sexo,
        wt = AVAD_tasa, name = "AVAD_tasa")  |> 
  
  # Pirámide
  age_pyramid(age_group = grupo_edad_10,
              split_by = sexo,
              count = AVAD_tasa,
              show_midpoint = FALSE) +
  
  # Dividir por año ENFR
  facet_wrap(~ anio_enfr) +
  
  # Colores
  scale_fill_scico_d(palette = "hawaii") +
  
  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Tasa AVAD") +
  
  # Tema
  theme_minimal()


## Pirámide poblacional por grupo quinquenal de edad, sexo y región
AVAD_ge5 |>
  # Tasa AVAD nacional
  count(reg_nombre, grupo_edad_5, sexo,
        wt = AVAD_tasa, name = "AVAD_tasa")  |> 
  
  # Pirámide
  age_pyramid(age_group = grupo_edad_5,
              split_by = sexo,
              count = AVAD_tasa,
              show_midpoint = FALSE) +
  
  # Dividir por año ENFR
  facet_wrap(~ reg_nombre) +
  
  # Colores
  scale_fill_scico_d(palette = "hawaii") +
  
  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Tasa AVAD") +
  
  # Tema
  theme_minimal()

# # Calcular intervalos de incertidumbre ------------------------------------
# ## Simular disability weights con distribución normal truncada
# dw_sim <- comp_dm  |> 
#   mutate(sim_id = 1) |> 
#   uncount(weights = 1000, .id = "sim_id") |> 
#   mutate(dw_sim = rnorm(n(), mean = dw, sd = (upper - lower) / 3.92),
#          dw_sim = pmin(pmax(dw_sim, 0), 1)) |> 
#   # Ponderar
#   mutate(weighted_dw = dw_sim * frec_wandurranga) |> 
#   
#   # Calcular DW ponderada total por simulación
#   group_by(sim_id) |> 
#   summarise(dw_total_sim = sum(weighted_dw),
#             .groups = "drop")
# 
# 
# ## AVAD con intervalo de incertidumbre
# avad_sim <- prev_dm_10 |> 
#   # Corrección mínima para prevalencias 0
#   mutate(dm_prev = if_else(dm_prev == 0, 1e-6, dm_prev)) |> 
#   
#   # Simular prevalencia
#   mutate(sim_id = 1) |> 
#   uncount(weights = 1000, .id = "sim_id") |> 
#   mutate(dm_prev_sim = rlnorm(n(), 
#                               meanlog = log(dm_prev), 
#                               sdlog = dm_prev_se / dm_prev)) |> 
#   
#   # Calcular la prevalencia media simulada
#   group_by(sim_id, anio_enfr, prov_nombre, grupo_edad_10, sexo) |> 
#   summarise(dm_prev_sim = mean(dm_prev_sim, na.rm = TRUE),
#             .groups = "drop") |> 
#   
#   # Añadir disability weights simulados
#   left_join(dw_sim) |> 
#   # Calcular AVD simulados
#   mutate(AVD_sim = dm_prev_sim * dw_total_sim) |> 
#   
#   # Añadir AVP
#   left_join(AVP_10) |> 
#   
#   # Calcular AVAD simulados
#   mutate(AVAD_sim = AVD_sim + AVP) |> 
#   
#   # Obtener AVAD y AVD con intervalo de incertidumbre
#   group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo) |> 
#   summarise(across(.cols = c(AVP, AVD_sim, AVAD_sim),
#                    .fns = ~ list(mean(.x), 
#                                  quantile(.x, 0.025),
#                                  quantile(.x, 0.075))))



