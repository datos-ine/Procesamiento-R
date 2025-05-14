### Cálculo de las  defunciones a partir de las bases de datos de mortalidad
### publicadas por la Dirección de Estadísticas e Información de Salud (DEIS),
### considerando como causa básica de muerte los códigos E10 a E14 de la Décima
### Revisión de la Clasificación Estadística Internacional de Enfermedades y
### Problemas Relacionados con la Salud (CIE-10). Para cada año de interés, se
### tomar el promedio de defunciones del trienio correspondiente
### Autora: Micaela Gauto 
### Colaboradora: Tamara Ricardo 
### Fecha de modificación:
# Wed May 14 09:42:39 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(tidyverse)


# Cargar datos crudos -----------------------------------------------------
## Etiquetas provincias INDEC
prov <- read_csv("Bases de datos/cod_pcias_arg.csv")

## Defunciones 2004
def04 <- read_csv("Bases de datos/DEIS/DE_2004.csv", 
                    col_select = c(prov_nombre = Jurisdicción,
                                   sexo = Sexo,
                                   grupo_edad_quin = `Grupo de edad`,
                                   causa = `Causa de muerte (CIE-10)`,
                                   total = Total))


## Defunciones 2005, 2006, 2008-10, 2012-14 y 2017-19
def_raw <- # Listar los csv para cada año de interés
  list.files(path = "Bases de datos/DEIS/",
             pattern = "^defweb.*\\.csv$",
             full.names = TRUE) |> 
  
  # Crear columna para el año
  set_names(nm = c(2005:2006, 2008:2010, 
                   2012:2014, 2017:2019)) |> 
  
  # Leer archivos csv y unir
  map(~ read_csv(.x, 
                 locale = locale(encoding = "WINDOWS-1252"),
                 col_select = c(prov_res = PROVRES,
                                sexo = SEXO,
                                causa = CAUSA,
                                grupo_edad_quin = GRUPEDAD,
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
  
  # Filtrar grupos de edad fuera del rango de interés
  filter(!str_detect(grupo_edad_quin, "01|02|03|04|05|06|07|08|09|25|ERROR")) |> 
  
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
      filter(!str_detect(grupo_edad_quin, "01|02|03|99")) |> 
      
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
  
  # Modificar etiquetas grupo edad quinquenal
  mutate(grupo_edad_quin = fct_relabel(grupo_edad_quin, ~ str_sub(.x, start = 4)) |> 
           fct_collapse("80 y más" = c("80 a 84", "85 y más"))
         ) |> 
  
  # Crear grupo edad ENFR
  mutate(grupo_edad_enfr = fct_collapse(grupo_edad_quin,
                                        "18 a 24" = c("15 a 19", "20 a 24"),
                                        "25 a 34" = c("25 a 29", "30 a 34"),
                                        "35 a 49" = c("35 a 39", "40 a 44", "45 a 49"),
                                        "50 a 64" = c("50 a 54", "55 a 59", "60 a 64"),
                                        "65+" = c("65 a 69", "70 a 74", 
                                                  "75 a 79", "80 y más")),
         .after = grupo_edad_quin) |> 
  
  # Crear etiqueta año ENFR
  mutate(anio_enfr = case_when(
    anio %in% c(2004:2006) ~ 2005,
    anio %in% c(2008:2010) ~ 2009,
    anio %in% c(2012:2014) ~ 2013,
    TRUE ~ 2018), 
    .after = anio ) |> 

  # Descartar columnas innecesarias
  select(-causa) 
  

### Explorar serie defunciones
tabyl(def_clean$prov_nombre)

tabyl(def_clean$sexo)

tabyl(def_clean$grupo_edad_quin)

tabyl(def_clean$grupo_edad_enfr)


# Limpiar tabla esperanza de vida -----------------------------------------
esp_vida_clean <- esp_vida_raw |> 
  # Estandarizar nombres de columna
  clean_names() |> 
  rename(grupo_edad_quin = age_group) |> 
  
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
  
  # Modificar niveles grupo edad quinquenal
  mutate(grupo_edad_quin = fct_collapse(grupo_edad_quin,
                                        "80 y más" = c("80-84 years", "85+ years")) |> 
           fct_relabel(~ levels(def_clean$grupo_edad_quin))
           ) |> 
  
  # Añadir grupo edad ENFR
  left_join(def_clean |> 
              select(grupo_edad_quin, grupo_edad_enfr) |> 
              distinct())
  
  
# Calcular AVP ------------------------------------------------------------
# AVP por grupo edad quinquenal
AVP_quin <- def_clean |> 
  
  # Completar filas categorías faltantes (sin muertes)
  complete(nesting(anio, anio_enfr), nesting(prov_res, prov_nombre), 
           sexo, grupo_edad_quin,
           fill = list(total = 0)) |> 
  
  # Crear serie defunciones
  count(anio, anio_enfr, prov_nombre, sexo, grupo_edad_quin, 
        wt = total, 
        name = "def_dm") |> 
  
  # Calcular total y promedio defunciones por año ENFR, provincia, sexo y edad
  group_by(anio_enfr, prov_nombre, grupo_edad_quin, sexo) |> 
  summarise(
    # Conteo defunciones para el trienio
    def_tri = sum(def_dm, na.rm = TRUE),
    
    # Promedio defunciones para el trienio
    mean_def_tri = mean(def_dm, na.rm = TRUE),
    .groups = "drop") |> 
  
  # Añadir esperanza de vida
  left_join(esp_vida_clean |> 
              # Calcular esperanza de vida por grupo de edad quinquenal
              group_by(grupo_edad_quin, sexo) |> 
              summarise(esp_vida = sum(Tx, na.rm = TRUE) / sum(lx, na.rm = TRUE),
                        .groups = "drop") 
              ) |> 
  
  # Calcular AVP
  mutate(AVP = mean_def_tri * esp_vida)
  

# AVP por grupo edad ENFR
AVP_enfr <- def_clean |> 
  
  # Completar filas categorías faltantes (sin muertes)
  complete(nesting(anio, anio_enfr), nesting(prov_res, prov_nombre),
           sexo, grupo_edad_enfr,
           fill = list(total = 0)) |> 
  
  # Crear serie defunciones
  count(anio, anio_enfr, prov_nombre, sexo, grupo_edad_enfr, 
        wt = total, 
        name = "def_dm") |> 
  
  # Calcular total y promedio defunciones por año ENFR, provincia, sexo y edad
  group_by(anio_enfr, prov_nombre, grupo_edad_enfr, sexo) |> 
  summarise(
    # Conteo defunciones para el trienio
    def_tri = sum(def_dm, na.rm = TRUE),
    
    # Promedio defunciones para el trienio
    mean_def_tri = mean(def_dm, na.rm = TRUE),
    .groups = "drop") |> 
  
  # Añadir esperanza de vida
  left_join(esp_vida_clean |> 
              # Calcular esperanza de vida por grupo de edad quinquenal
              group_by(grupo_edad_enfr, sexo) |> 
              summarise(esp_vida = sum(Tx, na.rm = TRUE) / sum(lx, na.rm = TRUE),
                        .groups = "drop") 
  ) |> 
  
  # Calcular AVP
  mutate(AVP = mean_def_tri * esp_vida)

# Guardar datos limpios ---------------------------------------------------
write_csv(AVP_quin, file = "Bases de datos/clean/AVP_serie_quinquenal.csv")

write_csv(AVP_enfr, file = "Bases de datos/clean/AVP_serie_enfr.csv")

# Limpiar environment
rm(list = ls())
