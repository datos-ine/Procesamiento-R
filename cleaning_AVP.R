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
# Mon May 26 13:58:12 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(rio)
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
def04_raw <- read_csv("Bases de datos/DEIS/DE_2004.csv")

## Defunciones 2005, 2006, 2008-10, 2012-14 y 2017-19
def_raw <- # Crear lista de archivos csv de interés
  list.files(path = "Bases de datos/DEIS/",
             pattern = "^defweb.",
             full.names = TRUE) |> 
  
  # Crear columna para el año
  set_names(nm = c(2005, 2006, 2008:2010, 
                   2012:2014, 2017:2019)) |> 
  
  # Leer archivos csv y unir
  map(read_csv, locale = locale(encoding = "WINDOWS-1252")) |> 
  list_rbind(names_to = "anio")


## Base esperanza de vida GHO-WHO para Argentina
esp_vida_raw <- read_csv2("Bases de datos/WHO_GHO/argentina_tabla de vida_GHO.csv",
                          skip = 1)


### Explorar datos crudos
glimpse(def04_raw)

glimpse(def_raw)

glimpse(esp_vida_raw)

tabyl(def04_raw$Jurisdicción)

tabyl(def04_raw$Sexo)

tabyl(def04_raw$`Grupo de edad`)

tabyl(def_raw$PROVRES)

tabyl(def_raw$SEXO)

tabyl(def_raw$GRUPEDAD)


# Limpiar serie defunciones -----------------------------------------------
## Serie 2004
def04 <- def04_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  rename(prov_nombre = jurisdiccion,
         grupo_edad = grupo_de_edad,
         causa = causa_de_muerte_cie_10) |> 
  
  # Crear columna para el año
  mutate(anio = "2004") |> 
  
  # Filtrar datos faltantes provincia
  filter(!prov_nombre %in% c("Lugar no especificado", "Otro país")) |> 
  
  # Filtrar datos faltantes sexo
  filter(sexo %in% c("Varón", "Mujer")) |> 
  
  # Filtrar grupos de edad fuera del rango de interés(20-80+)
  filter(between(grupo_edad, "11.20 a 24", "24.85 y más")) |> 
  
  # Modificar etiqueta provincia para CABA
  mutate(prov_nombre = fct_recode(prov_nombre,
                                  "CABA" = "Ciudad Aut. de Buenos Aires")) |> 
  
  # Añadir identificador numérico provincias
  left_join(id_prov)


## Serie 2005-2019
def <- def_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  rename(prov_id = provres,
         grupo_edad = grupedad,
         total = cuenta) |> 
  
  # Filtrar datos faltantes provincia
  mutate(prov_id = as.numeric(prov_id)) |> 
  filter(!between(prov_id, 98, 99)) |> 
  
  # Filtrar datos faltantes sexo
  filter(between(sexo, 1, 2)) |> 
  
  # Filtrar grupos de edad fuera del rango de interés
  filter(between(grupo_edad, "05_20 a 24", "17_80 y más")) |> 
  
  # Modificar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Varón",
                                  "Mujer"))) |> 
  
  # Añadir etiquetas provincia
  left_join(id_prov)
  

## Unir series defunciones
def_join <- bind_rows(def04, def) |> 
  # Filtrar causas de muerte por DM
  filter(causa %in% paste0("E", 10:14)) |> 
  
  # Crear etiqueta año ENFR
  mutate(anio_enfr = case_when(
    between(anio, "2004", "2006") ~ "2005",
    between(anio, "2008", "2010") ~ "2009",
    between(anio, "2012", "2014") ~ "2013",
    between(anio, "2017", "2019") ~ "2018")
    ) |> 
  
  # Quitar tres primeros caracteres de la etiqueta del grupo etario
  mutate(grupo_edad = str_sub(grupo_edad, 4)) |> 
  
  # Reagrupar niveles grupo etario
  mutate(grupo_edad = fct_collapse(grupo_edad,
                                 "80+" = c("80 a 84", "80 y más", "85 y más"))
         ) |> 
  
  # Crear grupos decenales de edad
  left_join(grupos_edad) |> 
  
  # Añadir filas faltantes (combinaciones sin defunciones)
  complete(nesting(anio, anio_enfr), 
           nesting(prov_id, prov_nombre),
           nesting(grupo_edad, grupo_edad_10),
           sexo,
           fill = list(total = 0)) |> 
  
  # Agrupar defunciones
  count(anio, anio_enfr, prov_id, prov_nombre, grupo_edad, grupo_edad_10, sexo,
        wt = total)


# Limpiar base esperanza de vida ------------------------------------------
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
  mutate(grupo_edad = fct_relabel(grupo_edad, 
                                  ~ levels(grupos_edad$grupo_edad))
  ) |> 
  
  # Añadir grupo edad decenal
  left_join(grupos_edad)


# Explorar datos ----------------------------------------------------------
tabyl(def04$prov_nombre)

tabyl(def04$grupo_edad)

tabyl(def$prov_id)

tabyl(def$grupo_edad)

tabyl(def_join$prov_nombre)

tabyl(def_join$grupo_edad)

tabyl(esp_vida$grupo_edad)


# Calcular AVP ------------------------------------------------------------
## Grupos quinquenales de edad
AVP_5 <- def_join |> 
  # Defunciones por trienio ENFR
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad, sexo) |> 
  summarise(defun_n = sum(n, na.rm = TRUE),
            defun_mean = mean(n, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir esperanza de vida
  left_join(esp_vida |>  
              select(grupo_edad, sexo, lx, Tx, ex)) |> 
  
  # Calcular AVP
  mutate(AVP = defun_mean * ex) |> 
  
  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, 2))) |> 
  
  # Variables categóricas a factor
  mutate(across(.cols = c(anio_enfr:sexo),
                .fns = ~ factor(.x)))


## Grupos decenales de edad
AVP_10 <- def_join |> 
  # Defunciones por trienio ENFR
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(defun_n = sum(n, na.rm = TRUE),
            defun_mean = mean(n, na.rm = TRUE),
            .groups = "drop") |> 
  
  # Añadir esperanza de vida
  left_join(esp_vida |>  
              # Calcular esperanza de vida para grupos decenales
              group_by(grupo_edad_10, sexo) |> 
              summarise(lx = mean(lx, na.rm = TRUE),
                        Tx = mean(Tx, na.rm = TRUE),
                        ex = mean(ex, na.rm = TRUE),
                        .groups = "drop")) |> 
  
  # Calcular AVP
  mutate(AVP = defun_mean * ex) |> 
  
  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, 2))) |> 
  
  # Variables categóricas a factor
  mutate(across(.cols = c(anio_enfr:sexo),
                .fns = ~ factor(.x)))


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = c("anio_enfr", "prov_id", "prov_nombre", 
               "grupo_edad", "grupo_edad_10", "sexo",
               "defun_n", "defun_mean", "lx", "Tx", "ex", "AVP"),
  
  descripcion = c(
    "Año de realización ENFR",
    "Identificador numérico provincia",
    "Nombre de provincia",
    "Grupo de edad quinquenal",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Número de defunciones para el trienio correspondiente",
    "Promedio de defunciones para el trienio correspondiente",
    "Cantidad de personas vivas a la edad x",
    "Años-persona vividos por encima de la edad x",
    "Esperanza de vida",
    "Años de vida perdidos por muerte prematura"),
  
  tipo_var = c(rep("factor", 6), rep("numeric", 6)),
  
  niveles = list(c(2005, 2009, 2013, 2018),
                 levels(id_prov$prov_id |>  factor()),
                 levels(id_prov$prov_nombre),
                 levels(grupos_edad$grupo_edad),
                 levels(grupos_edad$grupo_edad_10),
                 c("Varón", "Mujer"),
                 NA, NA, NA, NA, NA, NA) |> 
    as.character()
) |> 
  
  mutate(niveles = str_remove_all(niveles, '^c\\(|\\)$|"'))


# Guardar datos limpios ---------------------------------------------------
## AVP por grupo quinquenal de edad
write_csv(AVP_5, file = "Bases de datos/clean/arg_defun_avp_g5.csv")

## AVP por grupo decenal de edad
write_csv(AVP_10, file = "Bases de datos/clean/arg_defun_avp_g10.csv")

## Diccionario de datos
export(data_dict, file = "Bases de datos/clean/dic_arg_defun_avp.xlsx")


## Limpiar environment
rm(list = ls())
