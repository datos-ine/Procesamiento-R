### Cálculo de las  defunciones a partir de las bases de datos de mortalidad
### publicadas por la Dirección de Estadísticas e Información de Salud (DEIS),
### considerando como causa básica de muerte los códigos E10 a E14 de la Décima
### Revisión de la Clasificación Estadística Internacional de Enfermedades y
### Problemas Relacionados con la Salud (CIE-10). Para cada año de interés, se
### tomar el promedio de defunciones del trienio correspondiente
### Autora: Micaela Gauto 
### Colaboradora: Tamara Ricardo 
### Fecha de modificación:
# Thu May 15 13:49:08 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(tidyverse)


# Cargar datos crudos -----------------------------------------------------
## Etiquetas provincias INDEC
prov <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_edad <- read_csv("Bases de datos/grupos_edad.csv") |> 
  mutate_all(.funs = ~ factor(.x))

## Defunciones 2004
def04 <- read_csv("Bases de datos/DEIS/DE_2004.csv", 
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
  set_names(nm = c("2005", "2006", "2008":"2010", 
                   "2012":"2014", "2017":"2019")) |> 
  
  # Leer archivos csv y unir
  map(~ read_csv(.x, 
                 locale = locale(encoding = "WINDOWS-1252"),
                 col_select = c(prov_res = PROVRES,
                                sexo = SEXO,
                                causa = CAUSA,
                                grupo_edad = GRUPEDAD,
                                total = CUENTA))) |> 
  list_rbind(names_to = "anio")


## Base esperanza de vida WHO-GHE
esp_vida_raw <- read_csv("Bases de datos/WHO_GHO/arg_esp_vida_2019.csv",
                         skip = 1)


# Limpiar serie defunciones -----------------------------------------------
def_clean <- def04 |> 
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
  left_join(prov) |> 
  
  # Unir serie de defunciones 2005-19
  bind_rows(
    def_raw |> 
      # Filtrar datos faltantes provincia
      filter(!between(prov_res, "98", "99")) |> 
      
      # Filtrar datos faltantes sexo
      filter(between(sexo, 1, 2)) |> 
      
      # Filtrar grupos de edad fuera del rango de interés
      filter(!str_detect(grupo_edad, "01|02|03|04|99")) |> 
      
      # Añadir etiquetas provincia
      mutate(prov_res = as.numeric(prov_res)) |> 
      left_join(prov) |> 
      
      # Modificar etiquetas sexo
      mutate(sexo = factor(sexo,
                           labels = c("Varón",
                                      "Mujer")))
  ) |> 
  # Filtrar causas de muerte por DM
  filter(causa %in% paste0("E", 10:14)) |>
  
  # Crear etiqueta año ENFR
  mutate(anio_enfr = fct_collapse(anio,
                                  "2005" = c("2004":"2006"),
                                  "2009" = c("2008":"2010"),
                                  "2013" = c("2012":"2014"),
                                  "2018" = c("2017":"2019"))) |> 
  
  # Modificar etiquetas grupo edad quinquenal
  mutate(grupo_edad_5 = fct_relabel(grupo_edad, ~ str_sub(.x, start = 4)) |> 
           fct_collapse("80 y más" = c("80 a 84", "85 y más"))
         )|> 
  
  # Crear variable para grupo edad cada 10 años
  left_join(grupos_edad)


### Explorar serie defunciones
tabyl(def_clean$prov_nombre)

tabyl(def_clean$sexo)

tabyl(def_clean$grupo_edad_5)

tabyl(def_clean$grupo_edad_10)


# Limpiar tabla esperanza de vida -----------------------------------------
esp_vida_clean <- esp_vida_raw |> 
  # Estandarizar nombres de columna
  clean_names() |> 
  rename(grupo_edad = age_group) |> 
  
  # Filtrar grupo 15-19 años
  filter(grupo_edad != "15-19  years") |> 
  
  # Extraer primeras dos letras del estimador
  mutate(indicator = str_sub(indicator, start = 1, end = 2)) |>
  
  # Reorganizar datos a formato long según sexo
  pivot_longer(cols = c("male", "female"), 
               names_to = "sexo") |> 
  
  # Volver a formato wide según tipo de indicador
  pivot_wider(names_from = indicator, 
              values_from = value) |> 
  
  # Modificar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Varón",
                                  "Mujer"))) |> 
  
  # Modificar niveles grupo edad
  mutate(grupo_edad = fct_collapse(grupo_edad,
                                   "80 y más" = c("80-84 years", "85+ years"))
         ) |> 
  
  # Modificar etiquetas grupo edad
  mutate(grupo_edad_5 = fct_relabel(grupo_edad,
                                    ~ levels(grupos_edad$grupo_edad_5))) |> 
  
  # Crear variable para grupo edad cada 10 años
  left_join(grupos_edad)
  

# Calcular AVP ------------------------------------------------------------
# AVP por grupo edad quinquenal
AVP_g5 <- def_clean |> 
  
  # Completar filas categorías faltantes (sin muertes)
  complete(nesting(anio, anio_enfr), nesting(prov_res, prov_nombre), 
           sexo, grupo_edad_5,
           fill = list(total = 0)) |> 
  
  # Crear serie defunciones
  count(anio, anio_enfr, prov_nombre, sexo, grupo_edad_5, 
        wt = total, 
        name = "def_dm") |> 
  
  # Calcular total y promedio defunciones por año ENFR, provincia, sexo y edad
  group_by(anio_enfr, prov_nombre, grupo_edad_5, sexo) |> 
  summarise(
    # Conteo defunciones para el trienio
    def_tri = sum(def_dm, na.rm = TRUE),
    
    # Promedio defunciones para el trienio
    mean_def_tri = mean(def_dm, na.rm = TRUE),
    .groups = "drop") |> 
  
  # Añadir esperanza de vida
  left_join(esp_vida_clean |> 
              # Calcular esperanza de vida por grupo de edad quinquenal
              group_by(grupo_edad_5, sexo) |> 
              summarise(esp_vida = sum(Tx, na.rm = TRUE) / sum(lx, na.rm = TRUE),
                        .groups = "drop") 
              ) |> 
  
  # Calcular AVP
  mutate(AVP = mean_def_tri * esp_vida)
  

# AVP por grupo edad cada 10 años
AVP_g10 <- def_clean |> 
  
  # Completar filas categorías faltantes (sin muertes)
  complete(nesting(anio, anio_enfr), nesting(prov_res, prov_nombre),
           sexo, grupo_edad_10,
           fill = list(total = 0)) |> 
  
  # Crear serie defunciones
  count(anio, anio_enfr, prov_nombre, sexo, grupo_edad_10, 
        wt = total, 
        name = "def_dm") |> 
  
  # Calcular total y promedio defunciones por año ENFR, provincia, sexo y edad
  group_by(anio_enfr, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(
    # Conteo defunciones para el trienio
    def_tri = sum(def_dm, na.rm = TRUE),
    
    # Promedio defunciones para el trienio
    mean_def_tri = mean(def_dm, na.rm = TRUE),
    .groups = "drop") |> 
  
  # Añadir esperanza de vida
  left_join(esp_vida_clean |> 
              # Calcular esperanza de vida por grupo de edad quinquenal
              group_by(grupo_edad_10, sexo) |> 
              summarise(esp_vida = sum(Tx, na.rm = TRUE) / sum(lx, na.rm = TRUE),
                        .groups = "drop") 
  ) |> 
  
  # Calcular AVP
  mutate(AVP = mean_def_tri * esp_vida)


# Guardar datos limpios ---------------------------------------------------
write_csv(AVP_g5, file = "Bases de datos/clean/AVP_serie_5.csv")

write_csv(AVP_g10, file = "Bases de datos/clean/AVP_serie_10.csv")

# Limpiar environment
rm(list = ls())
