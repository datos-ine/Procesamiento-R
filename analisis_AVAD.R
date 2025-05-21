### Cálculo de AVP, AVD y AVAD para diabetes mellitus tipo 2 (DM2) en Argentina
### para los periodos correspondientes a las cuatro Encuestas Nacionales de
### Factores de Riesgo (2005, 2009, 2013 y 2018).
### Autoras: Micaela Gauto y Tamara Ricardo
### Fecha modificacion:
# Wed May 21 15:01:03 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(rio)
library(janitor)
library(tidyverse)


# Cargar datos ------------------------------------------------------------
## Defunciones por DM2 (DEIS)
defun_dm <- read_csv("Bases de datos/clean/arg_defun_dm_clean.csv")

## Tabla de vida GHO-WHO 2010
esp_vida <- read_csv("Bases de datos/clean/arg_esp_vida_2019_clean.csv")

## Prevalencia DM2 según ENFR por grupos quinquenales de edad
prev_dm5 <- read_csv("Bases de datos/clean/prev_dm_g5.csv")

## Prevalencia DM2 según ENFR por grupos decenales de edad
prev_dm5 <- read_csv("Bases de datos/clean/prev_dm_g10.csv")

## Secuelas DM2 (datos temporales)

## Proyecciones INDEC 2005
pob_05 <- import("Bases de datos/Proyecciones INDEC/proyec_2005.xlsx")

## Proyecciones INDEC 2010-2018
pob_10 <- import("Bases de datos/Proyecciones INDEC/proyec_2010_2018.xlsx")


# Cálculo de los AVP ------------------------------------------------------
# Por grupo quinquenal de edad
AVP_g5 <- defun_dm |> 
  # Defunciones por año, provincia, grupo edad y sexo
  count(anio, anio_enfr, prov_id, prov_nombre, grupo_edad, sexo,
        wt = total) |> 
  
  # Calcular defunciones por trienio ENFR
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad, sexo) |> 
  summarise(defun_n = sum(n, na.rm = TRUE),
            defun_mean = mean(n, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir tabla de vida GHO-WHO
  left_join(esp_vida |> 
              select(grupo_edad, sexo, lx, Tx, ex)) |> 
  
  # Calcular AVP
  mutate(AVP = defun_mean * ex) |> 
  
  # Redondear decimales
  mutate(across(.cols = c(defun_mean:AVP),
                .fns = ~ round(.x, 2)))

  
# Por grupo decenal de edad
AVP_g10 <- defun_dm |> 
  # Defunciones por año, provincia, grupo edad y sexo
  count(anio, anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo,
        wt = total) |> 
  
  # Calcular defunciones por trienio ENFR
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(defun_n = sum(n, na.rm = TRUE),
            defun_mean = mean(n, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir tabla de vida GHO-WHO
  left_join(esp_vida |> 
              # Calcular esperanza de vida para grupos decenales
              group_by(grupo_edad_10, sexo) |> 
              summarise(lx = mean(lx, na.rm = TRUE),
                        Tx = mean(Tx, na.rm = TRUE),
                        ex = mean(ex, na.rm = TRUE))
              ) |> 
  
  # Calcular AVP
  mutate(AVP = defun_mean * ex) |> 
  
  # Redondear decimales
  mutate(across(.cols = c(defun_mean:AVP),
                .fns = ~ round(.x, 2)))
