### Cálculo de las  defunciones a partir de las bases de datos de mortalidad
### publicadas por la Dirección de Estadísticas e Información de Salud (DEIS),
### considerando como causa básica de muerte los códigos E10 a E14 de la Décima
### Revisión de la Clasificación Estadística Internacional de Enfermedades y
### Problemas Relacionados con la Salud (CIE-10). Para cada año de interés, se
### tomar el promedio de defunciones del trienio correspondiente
### Autora: Micaela Gauto 
### Colaboradora: Tamara Ricardo 
### Fecha de modificación:
# Tue May 13 15:57:36 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(tidyverse)


# Cargar datos crudos -----------------------------------------------------
## Defunciones 2004
def04 <- read_delim("Bases de datos/DEIS/DE_2004.csv", 
                    col_select = c(prov_nombre = Jurisdicción,
                                   sexo = Sexo,
                                   grupo_edad_quin = `Grupo de edad`,
                                   causa = `Causa de muerte (CIE-10)`,
                                   total = Total))

## Defunciones 2005, 2006, 2008-10, 2012-14 y 2017-19
def_raw <- 
  # Listar los csv para cada año de interés
  list.files(path = "Bases de datos/DEIS/",
             pattern = "^defweb.*\\.csv$",
             full.names = TRUE) |> 
  
  # Crear columna para el año
  set_names(nm = c(2005:2006, 2008:2010, 
                   2012:2014, 2017:2019)) |> 
  
  # Unir archivos
  map(~ read_csv(.x, 
                 locale = locale(encoding = "WINDOWS-1252"),
                 col_select = c(prov_res = PROVRES,
                                sexo = SEXO,
                                causa = CAUSA,
                                grupo_edad_quin = GRUPEDAD,
                                total = CUENTA))) |> 
  list_rbind(names_to = "anio")


## Base esperanza de vida filtrada (tabla del WHO-GHE)
esp_vida_raw <- read_csv("Bases de datos/WHO_GHO/esp_vida_full.csv",
                         skip = 1)


# Etiquetas provincias INDEC
prov <- read_csv("Bases de datos/cod_pcias_arg.csv")


# Limpieza datos ----------------------------------------------------------
## Defunciones 2004
def04_clean <- def04 |> 
  # Añadir columna para el año
  mutate(anio = "2004") |> 
  
  # Categorías no relevantes a NA
  mutate(across(.cols = c(prov_nombre, sexo, grupo_edad_quin),
                .fns = ~if_else(.x %in% c("Lugar no especificado",
                                         "Otro país",
                                         "Desconocido",
                                         "Indeterminado",
                                         "01.0 a 6 días",
                                         "02.7 a 27 días",
                                         "03.28 días a 11 meses",
                                         "04.1 año",
                                         "05.2 años",
                                         "06.3 años",
                                         "07.4 años",
                                         "08.5 a 9",
                                         "09.10 a 14",
                                         "25.Sín especificar",
                                         "ERROR"),
                               NA, .x))) |> 
  
  # Eliminar filas con NA's
  drop_na() |> 
  
  # Cambiar etiqueta CABA
  mutate(prov_nombre = fct_recode(prov_nombre,
                               "CABA" = "Ciudad Aut. de Buenos Aires")) |> 
  
  # Unir grupos edad
  mutate(grupo_edad_quin = fct_collapse(grupo_edad_quin,
                                        "23.80 y más" = c("23.80 a 84", "24.85 y más"))) |> 
  
  # Añadir identificador numérico provincia
  left_join(prov)
  

## Serie defunciones
def_clean <- def_raw |> 
  # Filtrar NA's provincia
  filter(!between(prov_res, "98", "99")) |> 
  
  # Añadir etiquetas provincia
  mutate(prov_res = as.numeric(prov_res)) |> 
  left_join(prov) |> 
  
  # Filtrar NA's sexo
  filter(between(sexo, 1, 2)) |> 
  
  # Modificar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Varón",
                                  "Mujer")
  )) |> 
  
  # Filtrar grupos de edad
  filter(!grepl("Menor|1 a 9|10 a 14|Sin", grupo_edad_quin)) |>
  
  # Unir con datos 2004
  bind_rows(def04_clean) |> 
  
  # Modificar etiquetas grupo etario
  mutate(grupo_edad_quin = str_sub(grupo_edad_quin, start = 4) |> 
           factor()) |> 
  
  # Crear grupo edad ENFR
  mutate(grupo_edad_enfr = fct_collapse(grupo_edad_quin,
                                        "18 a 24" = c("15 a 19", "20 a 24"),
                                        "25 a 34" = c("25 a 29", "30 a 34"),
                                        "35 a 49" = c("35 a 39", "40 a 44", "45 a 49"),
                                        "50 a 64" = c("50 a 54", "55 a 59", "60 a 64"),
                                        "65+" = c("65 a 69", "70 a 74", 
                                                  "75 a 79", "80 y más")),
         .after = grupo_edad_quin) |> 
  
  # Filtrar causas de muerte por DM
  filter(causa %in% paste0("E", 10:14)) |>
  
  # Descartar columnas innecesarias
  select(-causa) 


## Esperanza de vida----
esp_vida <- esp_vida_raw |> 
  # Extraer primeras dos letras del estimador
  mutate(Indicator = str_sub(Indicator, start = 1, end = 2)) |>
  
  # Crear grupo de edad quinquenal
  mutate(grupo_edad_quin = fct_collapse(`Age Group`,
                                        "80 y más" = c("80-84 years", "85+ years")) |> 
           factor(labels = levels(def_clean$grupo_edad_quin))
  ) |> 
  
  # Crear grupo edad ENFR
  mutate(grupo_edad_enfr = fct_collapse(`Age Group`,
                                        "18 a 24" = c("15-19  years", "20-24 years"),
                                        "25 a 34" = c("25-29 years", "30-34 years"),
                                        "35 a 49" = c("35-39 years", "40-44 years", 
                                                      "45-49 years"),
                                        "50 a 64" = c("50-54 years", "55-59 years", 
                                                      "60-64 years"),
                                        "65+" = c("65-69 years", "70-74 years", 
                                                  "75-79 years", "80-84 years", "85+ years")),
         .after = grupo_edad_quin) |> 
  
  # Reorganizar datos a formato largo según sexo
  pivot_longer(cols = c("Male", "Female"), 
               names_to = "sexo") |>
  
  # Etiquetas para sexo
  mutate(sexo = factor(sexo, 
                       labels = c("Mujer", "Varón"))
  ) |>
  
  # Volver a formato ancho según tipo de indicador
  pivot_wider(names_from = Indicator, 
              values_from = value)


# Calcular AVP ------------------------------------------------------------
## Serie quinquenal
AVP_quin <- def_clean |> 
  
  # Completar filas categorías faltantes (sin muertes)
  complete(anio, nesting(prov_res, prov_nombre), sexo, grupo_edad_quin,
           fill = list(total = 0)) |> 
  
  # Crear serie defunciones
  count(anio, prov_res, prov_nombre, sexo, grupo_edad_quin, 
        wt = total, 
        name = "defun_dm") |> 
  
  # Añadir año ENFR
  mutate(anio_enfr = case_when(
    anio %in% c(2004:2006) ~ 2005,
    anio %in% c(2008:2010) ~ 2009,
    anio %in% c(2012:2014) ~ 2013,
    TRUE ~ 2018), 
    .after = anio ) |> 
  
  # Calcular muertes por provincia, sexo, grupo etario y año ENFR
  group_by(anio_enfr, prov_nombre, grupo_edad_quin, sexo) |> 
  summarise(n_defun_tri = sum(defun_dm, na.rm = TRUE),
            mean_defun_tri = mean(defun_dm, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir datos esperanza de vida
  left_join(esp_vida |>
              # Calcular esperanza de vida por grupo quinquenal
              group_by(grupo_edad_quin, sexo) |>
              summarise(
                esp_vida = sum(Tx, na.rm = TRUE) / sum(lx, na.rm = TRUE),
                .groups = "drop"
              )) |> 
  
  # Calcular AVP
  mutate(AVP = mean_defun_tri * esp_vida)


## Serie ENFR
AVP_enfr <- def_clean |> 
  
  # Completar filas categorías faltantes (sin muertes)
  complete(anio, nesting(prov_res, prov_nombre), sexo, grupo_edad_enfr,
           fill = list(total = 0)) |> 
  
  # Crear serie defunciones
  count(anio, prov_res, prov_nombre, sexo, grupo_edad_enfr, 
        wt = total, 
        name = "defun_dm") |> 
  
  # Añadir año ENFR
  mutate(anio_enfr = case_when(
    anio %in% c(2005:2006) ~ 2005,
    anio %in% c(2008:2010) ~ 2009,
    anio %in% c(2012:2014) ~ 2013,
    TRUE ~ 2018), 
    .after = anio ) |> 
  
  # Calcular muertes por provincia, sexo, grupo etario y año ENFR
  group_by(anio_enfr, prov_nombre, grupo_edad_enfr, sexo) |> 
  summarise(n_defun_tri = sum(defun_dm, na.rm = TRUE),
            mean_defun_tri = mean(defun_dm, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir datos esperanza de vida
  left_join(esp_vida |>
              # Calcular esperanza de vida por grupo quinquenal
              group_by(grupo_edad_enfr, sexo) |>
              summarise(
                esp_vida = sum(Tx, na.rm = TRUE) / sum(lx, na.rm = TRUE),
                .groups = "drop"
              )) |> 
  
  # Calcular AVP
  mutate(AVP = mean_defun_tri * esp_vida)


# Guardar datos limpios ---------------------------------------------------
write_csv(AVP_quin, file = "Bases de datos/clean/AVP_serie_quinquenal.csv")

write_csv(AVP_enfr, file = "Bases de datos/clean/AVP_serie_enfr.csv")

# Limpiar environment
rm(list = ls())

