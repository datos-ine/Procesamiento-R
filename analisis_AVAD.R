### Cálculo de AVP, AVD y AVAD para diabetes mellitus tipo 2 (DM2) en Argentina
### para los periodos correspondientes a las cuatro Encuestas Nacionales de
### Factores de Riesgo (2005, 2009, 2013 y 2018).
### Autoras: Micaela Gauto y Tamara Ricardo
### Fecha modificacion:


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
prev_dm_5 <- read_csv("Bases de datos/clean/arg_prev_dm_g5.csv")

## Prevalencia DM por grupos decenales de edad
prev_dm_10 <- read_csv("Bases de datos/clean/arg_prev_dm_g10.csv")


## Secuelas DM2 (datos temporales)
comp_dm <- read_csv("Bases de datos/clean/comp_dm_temp.csv") |> 
  drop_na()


## AVP por grupos quinquenales de edad
AVP_5 <- read_csv("Bases de datos/clean/arg_defun_avp_g5.csv")

## AVP por grupos decenales de edad
AVP_10 <- read_csv("Bases de datos/clean/arg_defun_avp_g10.csv")


## Proyecciones INDEC 2005-2018 por grupos quinquenales de edad
proy_pob_5 <- read_csv("Bases de datos/clean/arg_proy_2005_2018_g5.csv")

## Proyecciones INDEC 2005-2018 por grupos decenales de edad
proy_pob_10 <- read_csv("Bases de datos/clean/arg_proy_2005_2018_g10.csv")



# Calcular AVAD -----------------------------------------------------------
## Por provincia, sexo y grupos quinquenales de edad
AVAD_5 <- cross_join(prev_dm_5, comp_dm) |> 
  # Calcular AVD por cada secuela
  mutate(AVD_ind = dm_total * frec_wandurranga/ (100 * dw)) |> 
  
  # Calcular AVD totales
  group_by(across(-c(sequela:AVD_ind))) |> 
  summarise(AVD = sum(AVD_ind, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir datos defunciones, esperanza de vida y AVP
  left_join(AVP_5) |> 
  
  # Añadir proyecciones poblacionales INDEC
  left_join(proy_pob_5 |> 
              select(-anio))  |> 
  
  # Calcular AVAD
  mutate(AVAD = AVD + AVP) |> 
  
  # Calcular tasas específicas AVP, AVD y AVAD
  mutate(across(.cols = c(AVD, AVP, AVAD),
                .fns = ~ 100000 * (.x/proy_pob), 
                .names = "{.col}_tasa"))
  
  
## Por provincia, sexo y grupos decenales de edad
AVAD_10 <- cross_join(prev_dm_10, comp_dm) |> 
  # Calcular AVD por cada secuela
  mutate(AVD_ind = dm_total * frec_wandurranga/ (100 * dw)) |> 
  
  # Calcular AVD totales
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo,
           dm_total, dm_total_cv, dm_prev, dm_prev_cv) |> 
  summarise(AVD = sum(AVD_ind, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir datos defunciones, esperanza de vida y AVP
  left_join(AVP_10) |> 
  
  # Añadir proyecciones poblacionales INDEC
  left_join(proy_pob_10 |> 
              select(-anio))  |> 
  
  # Calcular AVAD
  mutate(AVAD = AVD + AVP) |> 
  
  # Calcular tasas específicas AVP, AVD y AVAD
  mutate(across(.cols = c(AVD, AVP, AVAD),
                .fns = ~ 100000 * (.x/proy_pob), 
                .names = "{.col}_tasa"))


# Guardar datos limpios (opcional) ----------------------------------------
# ## AVAD por provincia, sexo y grupo quinquenal de edad
# write_csv(AVAD_5, "Bases de datos/clean/arg_AVAD_g5.csv")
# 
# ## AVAD por provincia, sexo y grupo decenal de edad
# write_csv(AVAD_10, "Bases de datos/clean/arg_AVAD_g10.csv")

## Limpiar objetos intermedios
rm(AVP_5, AVP_10, comp_dm, prev_dm_5, prev_dm_10)


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



# Análisis datos ----------------------------------------------------------
### Tasas generales AVAD por año, provincia y sexo
tasa_gral <- AVAD_5 |> 
  group_by(anio_enfr, prov_nombre, sexo) |> 
  summarise(across(.cols = c(AVP, AVD, AVAD),
                   .fns = ~ 100000 * sum(.x, na.rm = TRUE)/sum(proy_pob, na.rm = TRUE),
                   .names = "{.col}_gral"),
            .groups = "drop")


### Tasas generales AVAD por año, sexo y grupo etario quinquenal
tasa_nac_5 <- AVAD_5 |> 
  group_by(anio_enfr, sexo, grupo_edad) |> 
  summarise(across(.cols = c(AVP, AVD, AVAD),
                   .fns = ~ 100000 * sum(.x, na.rm = TRUE)/sum(proy_pob, na.rm = TRUE),
                   .names = "{.col}_nac"),
            .groups = "drop")


### Tasas generales AVAD por año, sexo y grupo etario decenal
tasa_nac_10 <- AVAD_10 |> 
  group_by(anio_enfr, sexo, grupo_edad_10) |> 
  summarise(across(.cols = c(AVP, AVD, AVAD),
                   .fns = ~ 100000 * sum(.x, na.rm = TRUE)/sum(proy_pob, na.rm = TRUE),
                   .names = "{.col}_nac"),
            .groups = "drop")


# Gráficos exploratorios --------------------------------------------------
## Tasas nacionales
tasa_nac_5 |> 
  ggplot(aes(x = anio_enfr, y = AVAD_nac, color = sexo)) +
  
  # Capas geométricas
  geom_point() +
  geom_line() +
  facet_wrap(~ grupo_edad) +
  
  # Personalización escalas y ejes
  scale_color_scico_d(palette = "hawaii") +
  scale_x_continuous(breaks = c(2005, 2009, 2013, 2018)) +
  labs(x = "Año ENFR", y = "Tasa AVAD") +
  
  # Personalización tema
  theme_minimal() +
  theme(legend.position = "bottom")

  
