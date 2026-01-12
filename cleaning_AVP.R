### Limpieza y procesamiento de las bases de datos de mortalidad publicadas por
### la Dirección de Estadísticas e Información de Salud (DEIS), considerando
### como causa básica de muerte los códigos E10 a E14 de la Décima Revisión de
### la Clasificación Estadística Internacional de Enfermedades y Problemas
### Relacionados con la Salud (CIE-10).
### Limpieza y procesamiento de las tablas de vida publicadas para Argentina en
### el año 2019 por la GHO-WHO, considerando grupos de edad quinquenales y cada
### 10 años para población de 30 años y más según sexo.
### Autoras: Micaela Gauto y Tamara Ricardo
### Última modificación:
# 2025-12-30


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  geoAr,
  tidyverse
)


# Cargar datos crudos -----------------------------------------------------
## Etiquetas provincias INDEC
id_provincias <- get_provincias() |>
  # Renombrar columnas
  select(
    prov_id = id,
    prov_nombre = nombre
  ) |>

  # Cambiar etiqueta CABA
  mutate(prov_nombre = case_when(
    prov_id == "02" ~ "CABA",
    prov_id == "94" ~ "Tierra del Fuego",
    .default = prov_nombre
  ))

## Defunciones 2004
def04_raw <- read_csv("Bases de datos/DEIS/DE_2004.csv")

## Defunciones 2005, 2006, 2008-10, 2012-14 y 2017-19
def05_19_raw <- # Crear lista de archivos csv de interés
  list.files(
    path = "Bases de datos/DEIS/",
    pattern = "^defweb.",
    full.names = TRUE
  ) |>

  # Crear columna para el año
  set_names(nm = c(2005, 2006, 2008:2010, 2012:2014, 2017:2019)) |>

  # Leer archivos csv
  map(read_csv, locale = locale(encoding = "WINDOWS-1252"))


## Base esperanza de vida GHO-WHO para Argentina
esp_vida_raw <- read_csv2(
  "Bases de datos/WHO_GHO/argentina_tabla de vida_GHO.csv",
  skip = 1
)


# Limpiar serie defunciones -----------------------------------------------
## Serie 2004
def04 <- def04_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>
  rename(
    prov_nombre = jurisdiccion,
    grupo_edad = grupo_de_edad,
    causa = causa_de_muerte_cie_10
  ) |>

  # Crear columna para el año
  mutate(anio = "2004") |>

  # Filtrar datos faltantes provincia
  filter(!prov_nombre %in% c("Lugar no especificado", "Otro país")) |>

  # Filtrar datos faltantes sexo
  filter(sexo %in% c("Varón", "Mujer")) |>

  # # Filtrar grupos de edad fuera del rango de interés(35-80+)
  # filter(between(grupo_edad, "14.35", "24.85 y más")) |>
  
  # Filtrar grupos de edad fuera del rango de interés(30-80+)
  filter(between(grupo_edad, "13.30", "24.85 y más")) |>
  
  # Cambiar etiqueta CABA
  mutate(prov_nombre = if_else(str_detect(prov_nombre, "Ciudad"), "CABA", prov_nombre)) |> 
  
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
   filter(!between(prov_id, "98", "99")) |>

  # Filtrar datos faltantes sexo
  filter(between(sexo, 1, 2)) |>

  # Filtrar grupos de edad fuera del rango de interés (35-80+)
  #filter(between(grupo_edad, "08_35 a 39", "17_80 y más")) |>
  
  # Filtrar grupos de edad fuera del rango de interés (30-80+)
  filter(between(grupo_edad, "07_30 a 34", "17_80 y más")) |>
  
  # Modificar etiquetas sexo
  mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) |>

  # Añadir etiquetas provincia
  left_join(id_provincias)


### Unir serie defunciones
def_join <- bind_rows(def04, def05_19) |>
  
  # Filtrar causas de muerte por DM
  # filter(causa %in% paste0("E", 10:14)) |> 
  filter(causa == "E11" | causa == "E14") %>%  # Se toman las causas E11 (DM2) y los E14 de 30 o más (filtro de edad previo)

  # Crear etiqueta año ENFR
  mutate(
    anio_enfr = case_when(
      between(anio, "2004", "2006") ~ "2005",
      between(anio, "2008", "2010") ~ "2009",
      between(anio, "2012", "2014") ~ "2013",
      between(anio, "2017", "2019") ~ "2018"
    )) |>

  # Añadir clasificaciones grupo etario
  
  ## Grupos quinquenales
  mutate(grupo_edad_5 = case_when(
    str_detect(grupo_edad, "30 a 34") ~ "30-34", #se agrega grupo 30-34 años
    str_detect(grupo_edad, "35 a 39") ~ "35-39",
    str_detect(grupo_edad, "40 a 44") ~ "40-44",
    str_detect(grupo_edad, "45 a 49") ~ "45-49",
    str_detect(grupo_edad, "50 a 54") ~ "50-54",
    str_detect(grupo_edad, "55 a 59") ~ "55-59",
    str_detect(grupo_edad, "60 a 64") ~ "60-64",
    str_detect(grupo_edad, "65 a 69") ~ "65-69",
    str_detect(grupo_edad, "70 a 74") ~ "70-74",
    str_detect(grupo_edad, "75 a 79") ~ "75-79",
    .default = "80+"
  ),
  
  ## Grupos decenales
  grupo_edad_10 = case_when(
    str_detect(grupo_edad, "30 a 34") ~ "30-39", 
    str_detect(grupo_edad, "35 a 39") ~ "30-39",
    str_detect(grupo_edad, "40 a 44") ~ "40-49",
    str_detect(grupo_edad, "45 a 49") ~ "40-49",
    str_detect(grupo_edad, "50 a 54") ~ "50-59",
    str_detect(grupo_edad, "55 a 59") ~ "50-59",
    str_detect(grupo_edad, "60 a 64") ~ "60-69",
    str_detect(grupo_edad, "65 a 69") ~ "60-69",
    str_detect(grupo_edad, "70 a 74") ~ "70-79",
    str_detect(grupo_edad, "75 a 79") ~ "70-79",
    .default = "80+"
  ),
  
  # Agrupamiento en regiones según ENFR 2018
  region = case_when(
    prov_nombre == "CABA" | prov_nombre == "Buenos Aires" | prov_nombre == "Santa Fe" |
      prov_nombre == "Córdoba" | prov_nombre == "Entre Ríos" | prov_nombre == "La Pampa" ~ "Centro",
    prov_nombre == "Jujuy" | prov_nombre == "Salta" | prov_nombre == "Tucumán" | prov_nombre == "Catamarca" |
      prov_nombre == "La Rioja" | prov_nombre == "Santiago del Estero" ~ "Noroeste",
    prov_nombre == "Chaco" | prov_nombre == "Formosa" | prov_nombre == "Misiones" |
      prov_nombre == "Corrientes" ~ "Noreste",
    prov_nombre == "San Luis" | prov_nombre == "San Juan" | prov_nombre == "Mendoza" ~ "Cuyo",
    prov_nombre == "Neuquén" | prov_nombre == "Río Negro" | prov_nombre == "Chubut" |
      prov_nombre == "Santa Cruz" | prov_nombre == "Tierra del Fuego" ~ "Patagonia",
    .default = "Otro"
  )) |> 

  # Añadir filas faltantes (combinaciones sin defunciones)
  complete(
    nesting(anio, anio_enfr),
    nesting(region, prov_id, prov_nombre),
    nesting(grupo_edad_5, grupo_edad_10),
    sexo,
    fill = list(total = 0)
  )   

### Conteo defunciones por grupos decenales
def_join_10 <- def_join %>% 
  
  # Conteo defunciones
  count(
    anio,
    anio_enfr,
    prov_id,
    prov_nombre,
    grupo_edad_10,
    sexo,
    wt = total
  ) |>

  # Calcular defunciones por trienio ENFR
  group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo) |>
  summarise(
    defun_n = sum(n, na.rm = TRUE),
    defun_mean = mean(n, na.rm = TRUE),
    .groups = "drop"
  )

### Conteo defunciones por grupos decenales y región
def_join_10_region <- def_join %>% 
  
  # Conteo defunciones
  count(
    anio,
    anio_enfr,
    region,
    grupo_edad_10,
    sexo,
    wt = total
  ) |>
  
  # Calcular defunciones por trienio ENFR
  group_by(anio_enfr, region, grupo_edad_10, sexo) |>
  summarise(
    defun_n = sum(n, na.rm = TRUE),
    defun_mean = mean(n, na.rm = TRUE),
    .groups = "drop"
  )


# Limpiar base esperanza de vida ------------------------------------------

## Esperanza de vida en grupos quinquenales
# esp_vida_5 <- esp_vida_raw |>
#   # Estandarizar nombres de columna
#   clean_names() |>
# 
#   # Seleccionar datos para 2019
#   select(
#     indicator,
#     grupo_edad = age_group,
#     "Varón" = male_4,
#     "Mujer" = female_5
#   ) |>
# 
#   # Extraer primeras dos letras del estimador
#   mutate(indicator = str_sub(indicator, start = 1, end = 2)) |>
# 
#   # Crear columna para sexo
#   pivot_longer(cols = c("Varón", "Mujer"), names_to = "sexo") |>
# 
#   # Crear columnas para cada indicador
#   pivot_wider(names_from = indicator, values_from = value) |>
# 
#   # # Filtrar menores de 35 años y mayores de 85 años
#   # filter(
#   #   !str_detect(grupo_edad, "<1|1-4|5-9|10-14|15-19|20-24|25-29|30-34|85")
#   # ) |>
#   
#   # Filtrar menores de 30 años
#   filter(
#     !str_detect(grupo_edad, "<1|1-4|5-9|10-14|15-19|20-24|25-29|85")
#   ) |>
# 
#   # Cambiar etiquetas grupo etario
#   mutate(
#     grupo_edad = fct_relabel(grupo_edad, ~ levels(factor(def_join$grupo_edad_5))) #corrección
#   )

## Esperanza de vida en grupos decenales
esp_vida_10 <- esp_vida_raw |>
  # Estandarizar nombres de columna
  clean_names() |>
  
  # Seleccionar datos para 2019
  select(
    indicator,
    grupo_edad = age_group,
    "Varón" = male_4,
    "Mujer" = female_5
  ) |>
  
  # Extraer primeras dos letras del estimador
  mutate(indicator = str_sub(indicator, start = 1, end = 2)) |>
  
  # Crear columna para sexo
  pivot_longer(cols = c("Varón", "Mujer"), names_to = "sexo") |>
  
  # Crear columnas para cada indicador
  pivot_wider(names_from = indicator, values_from = value) |>
  
  # # Filtrar menores de 35 años y mayores de 85 años
  # filter(
  #   !str_detect(grupo_edad, "<1|1-4|5-9|10-14|15-19|20-24|25-29|30-34|85")
  # ) |>
  
  # Filtrar menores de 30 años y quedarme con grupos decenales
  filter(
    !str_detect(grupo_edad, "<1|1-4|5-9|10-14|15-19|20-24|25-29|35-39|45-49|55-59|65-69|75-79|85")
  ) |>
  
  # Cambiar etiquetas grupo etario
  mutate(
    grupo_edad = fct_relabel(grupo_edad, ~ levels(factor(def_join$grupo_edad_10))) #corrección
  )


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


tabyl(esp_vida_5$grupo_edad)

tabyl(esp_vida_10$grupo_edad)


# Calcular AVP ------------------------------------------------------------

## AVP por grupos decenales
AVP_ge10 <- def_join_10 |>

  # Añadir esperanza de vida
  left_join(
    esp_vida_10 |>
      # Descartar columnas innecesarias
      select(grupo_edad, sexo, lx, Tx, ex),
    by = join_by(grupo_edad_10 == grupo_edad, sexo == sexo)
  ) |>

  # Calcular AVP
  mutate(AVP = defun_mean * ex) |>

  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2)))

## AVP por grupos decenales y región
AVP_ge10_region <- def_join_10_region |>
  
  # Añadir esperanza de vida
  left_join(
    esp_vida_10 |>
      # Descartar columnas innecesarias
      select(grupo_edad, sexo, lx, Tx, ex),
    by = join_by(grupo_edad_10 == grupo_edad, sexo == sexo)
  ) |>
  
  # Calcular AVP
  mutate(AVP = defun_mean * ex) |>
  
  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric), .fns = ~ round(.x, 2)))

# Guardar datos limpios ---------------------------------------------------
write_csv(AVP_ge10, file = "Bases de datos/clean/arg_defun_avp_30.csv")

write_csv(AVP_ge10_region, file = "Bases de datos/clean/arg_defun_avp_30_reg.csv")


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = c(
    "anio_enfr",
    "prov_id",
    "prov_nombre",
    "grupo_edad_10",
    "sexo",
    "defun_n",
    "defun_mean",
    "lx",
    "Tx",
    "ex",
    "AVP"
  ),

  descripcion = c(
    "Año de realización ENFR",
    "Identificador numérico de provincia",
    "Identificador categórico de provincia",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Número de defunciones para el trienio correspondiente",
    "Promedio de defunciones para el trienio correspondiente",
    "Cantidad de personas vivas a la edad x",
    "Años-persona vividos por encima de la edad x",
    "Esperanza de vida a la edad x",
    "Años de vida perdidos por muerte prematura"
  ),

  tipo_var = c(rep("factor", 5), rep("numeric", 6)),

  valor = list(
    c(2005, 2009, 2013, 2018),
    levels(def_join_10$prov_id |> factor()),
    levels(def_join_10$prov_nombre |> factor()),
    levels(def_join_10$grupo_edad_10 |> factor()),
    c("Varón", "Mujer"),
    "0-Inf",
    "0-Inf",
    "0-Inf",
    "0-Inf",
    "0-Inf",
    "0-Inf"
  ) |>
    as.character() |>
    str_remove_all('^c\\(|\\)$|"')
)

data_dict_reg <- tibble(
  variable = c(
    "anio_enfr",
    "region",
    "grupo_edad_10",
    "sexo",
    "defun_n",
    "defun_mean",
    "lx",
    "Tx",
    "ex",
    "AVP"
  ),
  
  descripcion = c(
    "Año de realización ENFR",
    "Identificador categórico de región",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Número de defunciones para el trienio correspondiente",
    "Promedio de defunciones para el trienio correspondiente",
    "Cantidad de personas vivas a la edad x",
    "Años-persona vividos por encima de la edad x",
    "Esperanza de vida a la edad x",
    "Años de vida perdidos por muerte prematura"
  ),
  
  tipo_var = c(rep("factor", 4), rep("numeric", 6)),
  
  valor = list(
    c(2005, 2009, 2013, 2018),
    levels(def_join_10_region$region |> factor()),
    levels(def_join_10$grupo_edad_10 |> factor()),
    c("Varón", "Mujer"),
    "0-Inf",
    "0-Inf",
    "0-Inf",
    "0-Inf",
    "0-Inf",
    "0-Inf"
  ) |>
    as.character() |>
    str_remove_all('^c\\(|\\)$|"')
)


## Guardar diccionario de datos
export(data_dict, file = "Bases de datos/clean/dic_arg_defun_avp.xlsx")

export(data_dict_reg, file = "Bases de datos/clean/dic_arg_defun_avp_reg.xlsx")


## Limpiar environment y desactivar paquetes
rm(list = ls())

pacman::p_unload("all")
