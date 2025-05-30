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
# Fri May 30 14:02:29 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  tidyverse
)


# Cargar datos crudos -----------------------------------------------------
## Etiquetas provincias INDEC
id_provincias <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_etarios <- read_csv("Bases de datos/grupos_etarios.csv")


## Defunciones 2004
def04_raw <- read_csv("Bases de datos/DEIS/DE_2004.csv")

## Defunciones 2005, 2006, 2008-10, 2012-14 y 2017-19
def05_19_raw <- # Crear lista de archivos csv de interés
  list.files(path = "Bases de datos/DEIS/",
             pattern = "^defweb.",
             full.names = TRUE) |> 
  
  # Crear columna para el año
  set_names(nm = c(2005, 2006, 2008:2010, 
                   2012:2014, 2017:2019)) |> 
  
  # Leer archivos csv
  map(read_csv, locale = locale(encoding = "WINDOWS-1252")) 


## Base esperanza de vida GHO-WHO para Argentina
esp_vida_raw <- read_csv2("Bases de datos/WHO_GHO/argentina_tabla de vida_GHO.csv",
                          skip = 1)


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
  left_join(id_provincias)


## Serie 2005-2019
def05_19 <- def05_19_raw |> 
  # Unir archivos csv individuales
  list_rbind(names_to = "anio") |> 
  
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
  mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) |> 
  
  # Añadir etiquetas provincia
  left_join(id_provincias)


### Unir serie defunciones
def_join <- bind_rows(def04, def05_19) |> 
  # Filtrar causas de muerte por DM
  filter(causa %in% paste0("E", 10:14)) |>
  
  # Crear etiqueta año ENFR
  mutate(anio_enfr = case_when(
    between(anio, "2004", "2006") ~ "2005",
    between(anio, "2008", "2010") ~ "2009",
    between(anio, "2012", "2014") ~ "2013",
    between(anio, "2017", "2019") ~ "2018"
    )) |> 
  
  # Añadir clasificaciones grupo etario
  left_join(grupos_etarios) |> 
  
  # Añadir filas faltantes (combinaciones sin defunciones)
  complete(nesting(anio, anio_enfr), 
           nesting(prov_id, prov_nombre),
           nesting(grupo_edad_5, grupo_edad_10),
           sexo,
           fill = list(total = 0)) |> 
  
  # Conteo defunciones
  count(anio, anio_enfr, prov_id, prov_nombre, 
        grupo_edad_5, grupo_edad_10,sexo,
        wt = total) |> 
  
  # Calcular defunciones por trienio ENFR
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_5, grupo_edad_10, sexo) |> 
  summarise(defun_n = sum(n, na.rm = TRUE),
            defun_mean = mean(n, na.rm = TRUE),
            .groups = "drop")
  


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
  
  # Añadir clasificaciones grupo etario
  left_join(grupos_etarios) 


# Explorar datos ----------------------------------------------------------
### Explorar datos
tabyl(def04$prov_nombre)

tabyl(def04$sexo)

tabyl(def04$grupo_edad)

tabyl(def05_19$prov_nombre)

tabyl(def05_19$sexo)

tabyl(def05_19$grupo_edad)

tabyl(def_join$prov_nombre)

tabyl(def_join$sexo)

tabyl(def_join$grupo_edad_5)

tabyl(def_join$grupo_edad_10)


tabyl(esp_vida$grupo_edad_5)

tabyl(esp_vida$grupo_edad_10)



# Calcular AVP ------------------------------------------------------------
AVP_ge5 <- def_join |>
  
  # Añadir esperanza de vida
  left_join(esp_vida |> 
              # Descartar columnas innecesarias
              select(grupo_edad_5, grupo_edad_10, sexo, lx, Tx, ex)) |> 
  
  # Calcular AVP
  mutate(AVP = defun_mean * ex) |> 
  
  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, 2)))
  

# ## Por año ENFR, provincia, grupo decenal de edad y sexo
# AVP_ge10 <- AVP_ge5 |> 
#   # Estimar esperanza vida y AVP por grupo decenal de edad
#   group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo) |> 
#   summarise(defun_n = sum(defun_n, na.rm = TRUE),
#             defun_mean = mean(defun_n, na.rm = TRUE),
#             ex = weighted.mean(ex, lx, na.rm = TRUE),
#             AVP = defun_mean * ex,
#             .groups = "drop")
 


# Guardar datos limpios ---------------------------------------------------
write_csv(AVP_ge5, file = "Bases de datos/clean/arg_defun_avp_ge5.csv")




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
    "Esperanza de vida a la edad x",
    "Años de vida perdidos por muerte prematura"),
  
  tipo_var = c(rep("factor", 6), rep("numeric", 6)),
  
  valores = list(c(2005, 2009, 2013, 2018),
                 levels(id_provincias$prov_id |>  factor()),
                 levels(id_provincias$prov_nombre),
                 levels(grupos_etarios$grupo_edad_5 |>  factor()),
                 levels(grupos_etarios$grupo_edad_10 |>  factor()),
                 c("Varón", "Mujer"),
                 "0-Inf", "0-Inf", "0-Inf", "0-Inf", "0-Inf", "0-Inf") |> 
    as.character()
) |> 
  
  mutate(valores = str_remove_all(valores, '^c\\(|\\)$|"'))

## Guardar diccionario de datos
export(data_dict, file = "Bases de datos/clean/dic_arg_defun_avp.xlsx")


## Limpiar environment
rm(list = ls())
