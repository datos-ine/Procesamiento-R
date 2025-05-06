### Carga de paquetes ----
library(tidyverse)
library(readxl)
library(readr)

### Carga de bases de datos ----

def05 <- read_csv("Bases de datos/Defunciones/defweb05.csv", 
                     locale = locale(encoding = "WINDOWS-1252"))
def05 <- def05 %>% 
  mutate(AÑO = 2005)

def06 <- read_csv("Bases de datos/Defunciones/defweb06.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def06 <- def06 %>% 
  mutate(AÑO = 2006)

def08 <- read_csv("Bases de datos/Defunciones/defweb08.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def08 <- def08 %>% 
  mutate(AÑO = 2008)

def09 <- read_csv("Bases de datos/Defunciones/defweb09.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def09 <- def09 %>% 
  mutate(AÑO = 2009)

def10 <- read_csv("Bases de datos/Defunciones/defweb10.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def10 <- def10 %>% 
  mutate(AÑO = 2010)

def12 <- read_csv("Bases de datos/Defunciones/defweb12.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def12 <- def12 %>% 
  mutate(AÑO = 2012)

def13 <- read_csv("Bases de datos/Defunciones/defweb13.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def13 <- def13 %>% 
  mutate(AÑO = 2013)

def14 <- read_csv("Bases de datos/Defunciones/defweb14.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def14 <- def14 %>% 
  mutate(AÑO = 2014)

def17 <- read_csv("Bases de datos/Defunciones/defweb17.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def17 <- def17 %>% 
  mutate(AÑO = 2017)

def18 <- read_csv("Bases de datos/Defunciones/defweb18.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def18 <- def18 %>% 
  mutate(AÑO = 2018)

def19 <- read_csv("Bases de datos/Defunciones/defweb19.csv", 
                  locale = locale(encoding = "WINDOWS-1252"))
def19 <- def19 %>% 
  mutate(AÑO = 2019)

def_tot <- def05 %>% bind_rows(def06, def08, def09, def10, def12, def13,
                                 def14, def17, def18, def19)

### Selección de grupos de edad (15-19 hasta 80 y más), 
# de causas de muerte por DM,
# de provincias (elimina "98 = otro país"/"99 = no especificado"),
# de sexo (elimina "9 = sin especificar")

serie_def <- def_tot %>% 
  select(-MAT) %>% #elimino columna de muertes maternas
  filter(
    grepl("04_|05_|06_|07_|08_|09_|10_|11_|12_|13_|14_|15_|16_|17_", GRUPEDAD) &
      grepl("E10|E11|E12|E131|E14", CAUSA) &
      (PROVRES != 98 & PROVRES != 99) &
      (SEXO == 1 | SEXO == 2))

### Agrupamiento del recuento de muertes

serie_def <- serie_def %>% 
  group_by(AÑO, PROVRES, SEXO, GRUPEDAD) %>% 
  summarise(CUENTA = sum(CUENTA))

### Renombramiento de categorías de variables: PROVRES y SEXO

serie_def <- serie_def %>% 
  mutate(PROV_DESC = case_when(
    PROVRES == "02"	~ "CABA",
    PROVRES == "06"	~ "Buenos Aires",
    PROVRES == "10"	~ "Catamarca",
    PROVRES == "14"	~ "Córdoba",
    PROVRES == "18"	~ "Corrientes",
    PROVRES == "22"	~ "Chaco",
    PROVRES == "26"	~ "Chubut",
    PROVRES == "30"	~ "Entre Ríos",
    PROVRES == "34"	~ "Formosa",
    PROVRES == "38"	~ "Jujuy",
    PROVRES == "42"	~ "La Pampa",
    PROVRES == "46"	~ "La Rioja",
    PROVRES == "50"	~ "Mendoza", 
    PROVRES == "54"	~ "Misiones",
    PROVRES == "58"	~ "Neuquén",
    PROVRES == "62"	~ "Río Negro",
    PROVRES == "66"	~ "Salta", 
    PROVRES == "70"	~ "San Juan",
    PROVRES == "74"	~ "San Luis",
    PROVRES == "78"	~ "Santa Cruz",
    PROVRES == "82"	~ "Santa Fe",
    PROVRES == "86"	~ "Santiago del Estero",
    PROVRES == "90"	~ "Tucumán",
    PROVRES == "94"	~ "Tierra del Fuego",
    .default = PROVRES
  ),
  SEXO_DESC = case_when(
    SEXO == 1 ~ "Masculino",
    SEXO == 2 ~ "Femenino",
    .default = "Otro"
  ))

### Reordenamiento de variables y armado de base final

serie_def <- serie_def %>% 
  select(AÑO, PROVRES, PROV_DESC, SEXO, SEXO_DESC, GRUPEDAD, CUENTA)

save(serie_def,
     file = "serie_def.RData")
