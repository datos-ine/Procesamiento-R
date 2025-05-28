### Cálculo de AVP, AVD y AVAD para diabetes mellitus tipo 2 (DM2) en Argentina
### para los periodos correspondientes a las cuatro Encuestas Nacionales de
### Factores de Riesgo (2005, 2009, 2013 y 2018).
### Autoras: Micaela Gauto y Tamara Ricardo
### Fecha modificacion:


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
## AVP por grupos quinquenales de edad
AVP_5 <- read_csv("Bases de datos/clean/arg_defun_avp_g5.csv")

## AVP por grupos decenales de edad
AVP_10 <- read_csv("Bases de datos/clean/arg_defun_avp_g10.csv")


## Prevalencia DM por grupos quinquenales de edad
prev_dm5 <- read_csv("Bases de datos/clean/arg_prev_dm_g5.csv")

## Prevalencia DM por grupos decenales de edad
prev_dm10 <- read_csv("Bases de datos/clean/arg_prev_dm_g10.csv")


## Proyecciones INDEC 2005-2018
proy_pob <- read_csv("Bases de datos/clean/arg_proy_2005_2018.csv")


## Secuelas DM2 (datos temporales)
comp_dm <- import("Bases de datos/Complicaciones y DW.xlsx",
                  sheet = "DW_DM") |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  
  # Descartar filas con NA
  drop_na()


# Calcular AVAD -----------------------------------------------------------
## Por provincia, sexo y grupos quinquenales de edad
AVAD_5 <- cross_join(prev_dm5, comp_dm) |> 
  # Calcular AVD por cada secuela
  mutate(AVD_ind = dm_total * frecuencia_wandurranga/ (100 * disability_weight)) |> 
  
  # Calcular AVD totales
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad, sexo,
           dm_total, dm_total_cv, dm_prev, dm_prev_cv) |> 
  summarise(AVD = sum(AVD_ind, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir datos AVP
  left_join(AVP_5) |> 
  
  # Calcular AVAD
  mutate(AVAD = AVD + AVP) |> 
  
  # Añadir proyecciones poblacionales INDEC
  left_join(proy_pob |> 
              select(-anio, -grupo_edad_10)) |> 
  
  # Calcular tasas AVP, AVD y AVAD
  mutate(across(.cols = c(AVD, AVP, AVAD),
                .fns = ~ 100000 * .x/proy_pob, 
                .names = "{.col}_tasa"))
  

## Por provincia, sexo y grupos decenales de edad
AVAD_10 <- cross_join(prev_dm10, comp_dm) |> 
  # Calcular AVD por cada secuela
  mutate(AVD_ind = dm_total * frecuencia_wandurranga/ (100 * disability_weight)) |> 
  
  # Calcular AVD totales
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo,
           dm_total, dm_total_cv, dm_prev, dm_prev_cv) |> 
  summarise(AVD = sum(AVD_ind, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir datos AVP
  left_join(AVP_10) |> 
  
  # Calcular AVAD
  mutate(AVAD = AVD + AVP) |> 
  
  # Añadir proyecciones poblacionales INDEC
  left_join(proy_pob |> 
              # Calcular proyección por grupos decenales de edad
              group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo) |> 
              summarise(proy_pob = sum(proy_pob, na.rm = TRUE))) |> 
  
  # Calcular tasas AVP, AVD y AVAD
  mutate(across(.cols = c(AVD, AVP, AVAD),
                .fns = ~ 100000 * .x/proy_pob, 
                .names = "{.col}_tasa"))





