### Cálculo de las  defunciones a partir de las bases de datos de mortalidad
### publicadas por la Dirección de Estadísticas e Información de Salud (DEIS),
### considerando como causa básica de muerte los códigos E10 a E14 de la Décima
### Revisión de la Clasificación Estadística Internacional de Enfermedades y
### Problemas Relacionados con la Salud (CIE-10). Para cada año de interés, se
### tomar el promedio de defunciones del trienio correspondiente
### Autora: Micaela Gauto 
### Colaboradora: Tamara Ricardo 
### Fecha de modificación:
# Fri May  9 10:44:21 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(tidyverse)

# Cargar datos crudos -----------------------------------------------------
# Unir series separadas
datos_raw <- 
  # Listar los csv para cada año de interés
  list.files(path = "Bases de datos/Defunciones/",
                    pattern = ".*(05|06|08|09|10|12|13|14|17|18|19).*\\.csv$",
                    full.names = TRUE) |> 
  
  # Crear columna para el año
  set_names(nm = c(2005:2006, 2008:2010, 2012:2014, 2017:2019)) |> 
  
  # Unir archivos
  map(~ read_csv(.x, locale = locale(encoding = "WINDOWS-1252"))) |> 
  list_rbind(names_to = "anio")
  
  
# Explorar datos crudos ---------------------------------------------------
glimpse(datos_raw)

tabyl(datos_raw$PROVRES)

tabyl(datos_raw$SEXO)

tabyl(datos_raw$CAUSA)

tabyl(datos_raw$MAT)

tabyl(datos_raw$GRUPEDAD)

# Limpieza de datos -------------------------------------------------------
datos <- datos_raw |> 
  # Estandarizar nombres columnas (quitar mayúsculas, espacios y acentos)
  clean_names() |> 
  rename(prov_res = provres,
         grupo_edad = grupedad) |> 
  
  # Filtrar NAs provincia
  filter(!between(prov_res, "98", "99")) |> 
  
  # Filtrar NAs sexo
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
  
  # Modificar etiquetas grupo etario
  mutate(grupo_edad = str_sub(grupo_edad, start = 4)) |> 
  
  # Filtrar grupos de edad
  filter(!grepl("Menor|1 a 9|10 a 14|Sin", grupo_edad)) |>

  # Filtrar causas de muerte por DM
  filter(causa %in% paste0("E", 10:14)) |>

  # Descartar columnas innecesarias
  select(-causa, -mat)


### Explorar datos limpios
nlevels(datos$anio |>  factor()) *        # Cantidad de niveles año

nlevels(datos$prov_res |>  factor()) *    # Cantidad de niveles provincia

nlevels(datos$grupo_edad |>  factor()) *  # Cantidad de niveles grupo etario

nlevels(datos$sexo |>  factor())          # Cantidad de niveles sexo

tabyl(datos$sexo)

tabyl(datos$prov_res_cat)

tabyl(datos$grupo_edad)


# Agrupamiento del recuento de muertes ------------------------------------
serie_def <- datos |> 
  
  # Completar filas categorías faltantes (sin muertes)
  complete(anio, nesting(prov_res, prov_res_cat), sexo, grupo_edad,
           fill = list(cuenta = 0)) |> 
  
  # Conteo de muertes
  count(anio, prov_res, prov_res_cat, sexo, grupo_edad, 
        wt = cuenta, 
        name = "defun_dm")



# Guardar datos limpios ---------------------------------------------------
write_csv(serie_def, file = "Bases de datos/clean/serie_def_dm.csv") 

# Carga de datos
# def05 <- read_csv("Bases de datos/Defunciones/defweb05.csv", 
#                      locale = locale(encoding = "WINDOWS-1252"))
# def05 <- def05 %>% 
#   mutate(AÑO = 2005)
# 
# def06 <- read_csv("Bases de datos/Defunciones/defweb06.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def06 <- def06 %>% 
#   mutate(AÑO = 2006)
# 
# def08 <- read_csv("Bases de datos/Defunciones/defweb08.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def08 <- def08 %>% 
#   mutate(AÑO = 2008)
# 
# def09 <- read_csv("Bases de datos/Defunciones/defweb09.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def09 <- def09 %>% 
#   mutate(AÑO = 2009)
# 
# def10 <- read_csv("Bases de datos/Defunciones/defweb10.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def10 <- def10 %>% 
#   mutate(AÑO = 2010)
# 
# def12 <- read_csv("Bases de datos/Defunciones/defweb12.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def12 <- def12 %>% 
#   mutate(AÑO = 2012)
# 
# def13 <- read_csv("Bases de datos/Defunciones/defweb13.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def13 <- def13 %>% 
#   mutate(AÑO = 2013)
# 
# def14 <- read_csv("Bases de datos/Defunciones/defweb14.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def14 <- def14 %>% 
#   mutate(AÑO = 2014)
# 
# def17 <- read_csv("Bases de datos/Defunciones/defweb17.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def17 <- def17 %>% 
#   mutate(AÑO = 2017)
# 
# def18 <- read_csv("Bases de datos/Defunciones/defweb18.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def18 <- def18 %>% 
#   mutate(AÑO = 2018)
# 
# def19 <- read_csv("Bases de datos/Defunciones/defweb19.csv", 
#                   locale = locale(encoding = "WINDOWS-1252"))
# def19 <- def19 %>% 
#   mutate(AÑO = 2019)
# 
# def_tot <- def05 %>% bind_rows(def06, def08, def09, def10, def12, def13,
#                                  def14, def17, def18, def19)
#
### Selección de grupos de edad (15-19 hasta 80 y más), 
# de causas de muerte por DM,
# de provincias (elimina "98 = otro país"/"99 = no especificado"),
# de sexo (elimina "9 = sin especificar")
# 
# serie_def <- def_tot %>% 
#   select(-MAT) %>% #elimino columna de muertes maternas
#   filter(
#     grepl("04_|05_|06_|07_|08_|09_|10_|11_|12_|13_|14_|15_|16_|17_", GRUPEDAD) &
#       grepl("E10|E11|E12|E131|E14", CAUSA) &
#       (PROVRES != 98 & PROVRES != 99) &
#       (SEXO == 1 | SEXO == 2))
#
#
# serie_def <- serie_def %>% 
#   group_by(AÑO, PROVRES, SEXO, GRUPEDAD) %>% 
#   summarise(CUENTA = sum(CUENTA))
# 
# 
# ### Renombramiento de categorías de variables: PROVRES y SEXO
# 
# serie_def <- serie_def %>% 
#   mutate(PROV_DESC = case_when(
#     PROVRES == "02"	~ "CABA",
#     PROVRES == "06"	~ "Buenos Aires",
#     PROVRES == "10"	~ "Catamarca",
#     PROVRES == "14"	~ "Córdoba",
#     PROVRES == "18"	~ "Corrientes",
#     PROVRES == "22"	~ "Chaco",
#     PROVRES == "26"	~ "Chubut",
#     PROVRES == "30"	~ "Entre Ríos",
#     PROVRES == "34"	~ "Formosa",
#     PROVRES == "38"	~ "Jujuy",
#     PROVRES == "42"	~ "La Pampa",
#     PROVRES == "46"	~ "La Rioja",
#     PROVRES == "50"	~ "Mendoza", 
#     PROVRES == "54"	~ "Misiones",
#     PROVRES == "58"	~ "Neuquén",
#     PROVRES == "62"	~ "Río Negro",
#     PROVRES == "66"	~ "Salta", 
#     PROVRES == "70"	~ "San Juan",
#     PROVRES == "74"	~ "San Luis",
#     PROVRES == "78"	~ "Santa Cruz",
#     PROVRES == "82"	~ "Santa Fe",
#     PROVRES == "86"	~ "Santiago del Estero",
#     PROVRES == "90"	~ "Tucumán",
#     PROVRES == "94"	~ "Tierra del Fuego",
#     .default = PROVRES
#   ),
#   SEXO_DESC = case_when(
#     SEXO == 1 ~ "Masculino",
#     SEXO == 2 ~ "Femenino",
#     .default = "Otro"
#   ))
#
### Reordenamiento de variables y armado de base final
# 
# serie_def <- serie_def %>% 
#   select(AÑO, PROVRES, PROV_DESC, SEXO, SEXO_DESC, GRUPEDAD, CUENTA)
# 
# save(serie_def,
#      file = "serie_def.RData")
