### Cálculo de las  defunciones a partir de las bases de datos de mortalidad
### publicadas por la Dirección de Estadísticas e Información de Salud (DEIS),
### considerando como causa básica de muerte los códigos E10 a E14 de la Décima
### Revisión de la Clasificación Estadística Internacional de Enfermedades y
### Problemas Relacionados con la Salud (CIE-10). Para cada año de interés, se
### tomar el promedio de defunciones del trienio correspondiente
### Autora: Micaela Gauto 
### Colaboradora: Tamara Ricardo 
### Fecha de modificación:
# Mon May 12 11:33:28 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(tidyverse)


# Cargar datos crudos -----------------------------------------------------
## Unir series defunciones
def_raw <- 
  # Listar los csv para cada año de interés
  list.files(path = "Bases de datos/DEIS/", # Acá están las bases de los años que usamos
                    pattern = "*.csv",
                    full.names = TRUE) |> 
  
  # Crear columna para el año
  set_names(nm = c(2005:2006, 2008:2010, 
                   2012:2014, 2017:2019)) |> # Modificar cuando tengamos 2004
  
  # Unir archivos
  map(~ read_csv(.x, locale = locale(encoding = "WINDOWS-1252"))) |> 
  list_rbind(names_to = "anio")
  

## Base de esperanza de vida filtrada (tabla del WHO-GHE)
esp_vida_raw <- read_csv("Bases de datos/arg_2019_espvida_filtrada.csv",
                         skip = 1) 


# Limpieza de datos -------------------------------------------------------
## Series defunciones-----
def_clean <- def_raw |> 
  # Estandarizar nombres columnas (quitar mayúsculas, espacios y acentos)
  clean_names() |> 
  rename(prov_res = provres,
         grupo_edad = grupedad) |> 
  
  # Filtrar NA's provincia
  filter(!between(prov_res, "98", "99")) |> 
  
  # Filtrar NA's sexo
  filter(between(sexo, 1, 2)) |> 
  
  # Modificar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Masculino",
                                  "Femenino")
  )) |> 
  
  # Modificar etiquetas provincia
  mutate(prov_res_cat = factor(
    prov_res,
    labels = c("CABA", 
               "Buenos Aires", 
               "Catamarca",
               "Córdoba",
               "Corrientes",
               "Chaco",
               "Chubut",
               "Entre Ríos",
               "Formosa",
               "Jujuy",
               "La Pampa",
               "La Rioja",
               "Mendoza",
               "Misiones",
               "Neuquén",
               "Río Negro",
               "Salta",
               "San Juan",
               "San Luis",
               "Santa Cruz",
               "Santa Fe",
               "Santiago del Estero",
               "Tucumán",
               "Tierra del Fuego")
  ), .after = prov_res) |> 
  
  # Filtrar grupos de edad por >20 años
  filter(!grepl("Menor|1 a 9|10 a 14|15 a 19|Sin", grupo_edad)) |>
  
  # Modificar etiquetas grupo etario
  mutate(grupo_edad = str_sub(grupo_edad, start = 4) |> factor()) |> 
  
  # Filtrar causas de muerte por DM
  filter(causa %in% paste0("E", 10:14)) |>

  # Descartar columnas innecesarias
  select(-causa, -mat) 


## Esperanza de vida----
esp_vida <- esp_vida_raw |> 
  # Estandarizar nombres de variables
  rename(grupo_edad = "Age Group",
         Masculino = Male,
         Femenino = Female) |> 
  
  # Formato long
  pivot_longer(cols = c("Masculino", "Femenino"),
               names_to = "sexo",
               values_to = "esp_vida") |> 
  
  # Filtrar menores de 20 años y mayores de 85
  filter(!grupo_edad %in% c("15-19  years", "85+ years")) |> 
  
  # Etiquetas grupos etarios
  mutate(grupo_edad = factor(grupo_edad,
                             labels = levels(def_clean$grupo_edad)))
  

### Explorar datos limpios
nlevels(datos$anio |>  factor()) *        # Cantidad de niveles año

nlevels(datos$prov_res |>  factor()) *    # Cantidad de niveles provincia

nlevels(datos$grupo_edad |>  factor()) *  # Cantidad de niveles grupo etario

nlevels(datos$sexo |>  factor())          # Cantidad de niveles sexo

tabyl(datos$sexo)

tabyl(datos$prov_res_cat)

tabyl(datos$grupo_edad)


# Serie de defunciones para período de estudio ----------------------------
serie_def <- def_clean |> 
  
  # Completar filas categorías faltantes (sin muertes)
  complete(anio, nesting(prov_res, prov_res_cat), sexo, grupo_edad,
           fill = list(cuenta = 0)) |> 
  
  # Conteo de muertes
  count(anio, prov_res, prov_res_cat, sexo, grupo_edad, 
        wt = cuenta, 
        name = "defun_dm")


# Cálculo AVP a partir de las defunciones ---------------------------------
AVP_serie <- serie_def |> 
  # Añade año encuesta
  mutate(anio_enfr = case_when(
    anio %in% c(2005:2006) ~ 2005,
    anio %in% c(2008:2010) ~ 2009,
    anio %in% c(2012:2014) ~ 2013,
    TRUE ~ 2018
  )) |> 
  
  # Calcula muertes
  group_by(prov_res_cat, sexo, grupo_edad, anio_enfr) |> 
  summarise(sum_defun = sum(defun_dm, na.rm = TRUE),
            mean_defun = mean(defun_dm, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Une con datos esperanza de vida
  inner_join(esp_vida |> select(-Indicator)) |> 
  
  # Calcular AVP
  mutate(AVP = mean_defun * esp_vida)


# Guardar datos limpios ---------------------------------------------------
write_csv(AVP_serie, file = "Bases de datos/clean/AVP_serie.csv")

# Limpiar environment
rm(list = ls())