### Cálculo de las  defunciones a partir de las bases de datos de mortalidad
### publicadas por la Dirección de Estadísticas e Información de Salud (DEIS),
### considerando como causa básica de muerte los códigos E10 a E14 de la Décima
### Revisión de la Clasificación Estadística Internacional de Enfermedades y
### Problemas Relacionados con la Salud (CIE-10). Para cada año de interés, se
### tomar el promedio de defunciones del trienio correspondiente
### Autora: Micaela Gauto 
### Colaboradora: Tamara Ricardo 
### Fecha de modificación:
# Tue May 20 11:15:38 2025 ------------------------------


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


## Base esperanza de vida WHO-GHE
esp_vida_raw <- read_csv("Bases de datos/WHO_GHO/arg_esp_vida_2019.csv",
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
  left_join(grupos_edad)


### Explorar serie defunciones
tabyl(def_join$prov_nombre)

tabyl(def_join$sexo)

tabyl(def_join$grupo_edad)

tabyl(def_join$grupo_edad_10)

### Limpiar objetos intermedios
rm(def04_raw, def04, def_raw, def)


# Limpiar tabla esperanza de vida -----------------------------------------
esp_vida <- esp_vida_raw |> 
  # Estandarizar nombres de columna
  clean_names() |> 
  rename(grupo_edad = age_group) |> 
  
  # Filtrar grupo 15-19 años
  filter(grupo_edad != "15-19  years") |> 
  
  # Extraer primeras dos letras del estimador
  mutate(indicator = str_sub(indicator, start = 1, end = 2)) |>
  
  # Crear columna para sexo
  pivot_longer(cols = c("male", "female"), 
               names_to = "sexo") |>
  
  # Crear columnas para indicadores
  pivot_wider(names_from = indicator,
              values_from = value) |> 
  
  # Modificar etiquetas sexo
  mutate(sexo = factor(sexo, labels = c("Varón",
                                        "Mujer"))) |> 
  
  # Agrupar niveles grupo edad
  mutate(grupo_edad = fct_collapse(grupo_edad,
                                   "80+" = c("80-84 years", "85+ years"))) |> 
  
  # Modificar etiquetas grupo edad
  mutate(grupo_edad = fct_relabel(grupo_edad,
                                  ~ levels(grupos_edad$grupo_edad))) |> 
  
  # Crear variable para grupo edad cada 10 años
  left_join(grupos_edad)
  

# Calcular AVP ------------------------------------------------------------
## AVP por grupo edad quinquenal
AVP_g5 <- def_join |> 
  # Completar filas faltantes (sin defunciones)
  complete(nesting(anio, anio_enfr), nesting(prov_id, prov_nombre),
           sexo, grupo_edad,
           fill = list(total = 0)) |> 
  
  # Conteo defunciones por año, provincia, sexo y grupo etario
  count(anio, anio_enfr, prov_id, prov_nombre, grupo_edad, sexo,
        wt = total,
        name = "defun_dm") |> 
  
  # Calcular defunciones totales y promedio por trienio ENFR
  group_by(anio_enfr, prov_nombre, grupo_edad, sexo) |> 
  summarise(defun_tri = sum(defun_dm, na.rm = TRUE),
            defun_mean_tri = mean(defun_dm, na.rm = TRUE) |>  round(2),
            .groups = "drop") |> 
  
  # Añadir datos esperanza de vida
  left_join(esp_vida |> 
              # Esperanza de vida por grupo edad quinquenal
              group_by(grupo_edad, sexo) |> 
              summarise(esp_vida = sum(Tx)/sum(lx),
                        .groups = "drop")
  ) |> 
  
  # Calcular AVP
  mutate(AVP = defun_mean_tri * esp_vida)
  

## AVP por grupo edad cada 10 años
AVP_g10 <- def_join |> 
  # Completar filas faltantes (sin defunciones)
  complete(nesting(anio, anio_enfr), nesting(prov_id, prov_nombre),
           sexo, grupo_edad_10,
           fill = list(total = 0)) |> 
  
  # Conteo defunciones por año, provincia, sexo y grupo etario
  count(anio, anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo,
        wt = total,
        name = "defun_dm") |> 
  
  # Calcular defunciones totales y promedio por trienio ENFR
  group_by(anio_enfr, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(defun_tri = sum(defun_dm, na.rm = TRUE),
            defun_mean_tri = mean(defun_dm, na.rm = TRUE) |>  round(2),
            .groups = "drop") |> 
  
  # Añadir datos esperanza de vida
  left_join(esp_vida |> 
              # Esperanza de vida por grupo edad quinquenal
              group_by(grupo_edad_10, sexo) |> 
              summarise(esp_vida = sum(Tx)/sum(lx),
                        .groups = "drop")
  ) |> 
  
  # Calcular AVP
  mutate(AVP = defun_mean_tri * esp_vida)


# Explorar datos limpios --------------------------------------------------
## Diferencias en la mortalidad por DM según sexo
# Grupos quinquenales
mod1 <- lm(defun_mean_tri ~ sexo, data = AVP_g5) 

summary(mod1) # p = 0.521

# Grupos cada 10 años
mod2 <- lm(defun_mean_tri ~ sexo, data = AVP_g10) 

summary(mod2) # p = 0.585 


## Diferencias en la mortalidad por DM según sexo y grupo etario
# Grupos quinquenales
mod3 <- lm(defun_mean_tri ~ sexo * grupo_edad, data = AVP_g5) 

summary(mod3) # Solo hay diferencias significativas para el grupo 80+ (p = 0.001)


# Grupos cada 10 años
mod4 <- lm(defun_mean_tri ~ sexo * grupo_edad_10, data = AVP_g10) 

summary(mod4) # Solo hay diferencias significativas para el grupo 80+ (p = 0.044)


# Guardar datos limpios ---------------------------------------------------
write_csv(AVP_g5, file = "Bases de datos/clean/AVP_serie_5.csv")

write_csv(AVP_g10, file = "Bases de datos/clean/AVP_serie_10.csv")

# Limpiar environment
rm(list = ls())
