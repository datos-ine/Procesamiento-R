### Limpieza y procesamiento de las bases de datos de mortalidad publicadas por
### la Dirección de Estadísticas e Información de Salud (DEIS), considerando
### como causa básica de muerte los códigos E10 a E14 de la Décima Revisión de
### la Clasificación Estadística Internacional de Enfermedades y Problemas
### Relacionados con la Salud (CIE-10).
### Limpieza y procesamiento de las tablas de vida publicadas para Argentina en
### el año 2019 por la GHO-WHO, considerando grupos de edad quinquenales y cada
### 10 años para población de 20 años y más según sexo.
### Autoras: Micaela Gauto y Tamara Ricardo
### Última modificación:
# Wed May 21 18:20:28 2025 ------------------------------



# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(tidyverse)


# Cargar datos crudos -----------------------------------------------------
## Etiquetas provincias INDEC
id_prov <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_edad <- read_csv("Bases de datos/grupos_edad.csv") |> 
  mutate_all(.funs = ~ factor(.x))


## Defunciones 2004
def04_raw <- read_csv("Bases de datos/DEIS/DE_2004.csv", 
                      col_select = c(prov_nombre = Jurisdicción,
                                     sexo = Sexo,
                                     grupo_edad = `Grupo de edad`,
                                     causa = `Causa de muerte (CIE-10)`,
                                     total = Total))


## Defunciones 2005, 2006, 2008-10, 2012-14 y 2017-19
def_raw <- # Listar los csv para cada año de interés
  list.files(path = "Bases de datos/DEIS/",
             pattern = "^defweb.*\\.csv$",
             full.names = TRUE) |> 
  
  # Crear columna para el año
  set_names(nm = c(2005, 2006, 2008:2010, 
                   2012:2014, 2017:2019)) |> 
  
  # Leer archivos csv y unir
  map(~ read_csv(.x, 
                 locale = locale(encoding = "WINDOWS-1252"),
                 col_select = c(prov_id = PROVRES,
                                sexo = SEXO,
                                causa = CAUSA,
                                grupo_edad = GRUPEDAD,
                                total = CUENTA))) |> 
  list_rbind(names_to = "anio")

## Base esperanza de vida GHO-WHO para Argentina
esp_vida_raw <- read_csv2("Bases de datos/WHO_GHO/argentina_tabla de vida_GHO.csv",
                          skip = 1)


# Limpiar serie defunciones -----------------------------------------------
## Serie 2004
def04 <- def04_raw |> 
  # Crear columna para el año
  mutate(anio = "2004") |> 
  
  # Filtrar datos faltantes provincia
  filter(!prov_nombre %in% c("Lugar no especificado", "Otro país")) |> 
  
  # Filtrar datos faltantes sexo
  filter(sexo %in% c("Varón", "Mujer")) |> 
  
  # Filtrar grupos de edad fuera del rango de interés(20-80+)
  filter(!str_detect(grupo_edad, 
                     "01|02|03|04|05|06|07|08|09|10|25|ERROR")) |> 
  
  # Modificar etiqueta provincia para CABA
  mutate(prov_nombre = fct_recode(prov_nombre,
                                  "CABA" = "Ciudad Aut. de Buenos Aires")) |> 
  
  # Añadir identificador numérico de provincias
  left_join(id_prov)


## Series 2005-2019
def <- def_raw |> 
  # Modificar estructura prov_id
  mutate(prov_id = as.numeric(prov_id)) |> 
  
  # Filtrar datos faltantes provincia
  filter(!between(prov_id, 98, 99)) |> 
  
  # Filtrar datos faltantes sexo
  filter(between(sexo, 1, 2)) |> 
  
  # Filtrar grupos de edad fuera del rango de interés
  filter(!str_detect(grupo_edad, "01|02|03|04|99")) |> 
  
  # Modificar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Varón",
                                  "Mujer"))) |> 
  
  # Añadir etiquetas provincia
  left_join(id_prov)


## Unir series defunciones
def_join <- def04 |> 
  bind_rows(def) |> 
  
  # Filtrar causas de muerte por DM
  filter(causa %in% paste0("E", 10:14)) |>
  
  # Crear etiqueta año ENFR
  mutate(anio_enfr = case_when(
    between(anio, "2004", "2006") ~ "2005",
    between(anio, "2008", "2010") ~ "2009",
    between(anio, "2012", "2014") ~ "2013",
    between(anio, "2017", "2019") ~ "2018"
  )) |> 
  
  # Quitar los tres primeros caracteres grupo edad
  mutate(grupo_edad = fct_relabel(grupo_edad, ~ str_sub(.x, 4))) |> 
  
  # Reagrupar niveles grupo edad
  mutate(grupo_edad = fct_collapse(grupo_edad,
                                   "80+" = c("80 y más", "80 a 84", "85 y más"))
  ) |> 
  
  # Crear variable para grupo edad cada 10 años
  left_join(grupos_edad) |> 
  
  # Añadir filas faltantes (sin muertes por DM)
  complete(nesting(anio, anio_enfr), 
           nesting(prov_id, prov_nombre),
           nesting(grupo_edad, grupo_edad_10),
           sexo,
           fill = list(total = 0)) |> 
  
  # Descartar columnas innecesarias
  select(-causa)


# Explorar datos defunciones ----------------------------------------------
tabyl(def_join$prov_nombre)

tabyl(def_join$sexo)

tabyl(def_join$grupo_edad)

tabyl(def_join$grupo_edad_10)


# Limpiar datos esperanza de vida -----------------------------------------
esp_vida <- esp_vida_raw |> 
  # Estandarizar nombres de columna
  clean_names() |> 
  
  # Seleccionar datos para 2019
  select(indicator,
         grupo_edad = age_group,
         "Varón" = male_4,
         "Mujer" = female_5) |> 
  
  # Extraer primeras dos letras del estimador
  mutate(indicator = str_sub(indicator, start = 1, end = 2)) |>
  
  # Crear columna para sexo
  pivot_longer(cols = c("Varón", "Mujer"), 
               names_to = "sexo") |>
  
  # Crear columnas para cada indicador
  pivot_wider(names_from = indicator,
              values_from = value) |> 
  
  # Filtrar menores de 20 años y mayores de 85 años
  filter(!str_detect(grupo_edad, "<1|1-4|5-9|10-14|15-19|85")) |> 
  
  # Cambiar las etiquetar grupo edad quinquenal
  mutate(grupo_edad = fct_relabel(grupo_edad, ~ levels(grupos_edad$grupo_edad))
  ) |> 
  
  # Añadir grupo edad cada 10 años
  left_join(grupos_edad) |> 
  select(grupo_edad, grupo_edad_10, everything())


# Guardar datos esperanza de vida -----------------------------------------
## Defunciones por DM
write_csv(def_join, "Bases de datos/clean/arg_defun_dm.csv")

## Esperanza de vida 2019
write_csv(esp_vida, "Bases de datos/clean/arg_esp_vida_2019.csv")


## Limpiar environment
rm(list = ls())