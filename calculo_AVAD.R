#### Cálculo de AVADs para DM en Argentina - años 2005, 2009, 2013 y 2018
#### AVAD = AVP (años de vida perdidos por muerte prematura) + AVD (años vividos con discapacidad)

### Cálculo de AVP -------------------------------------------------------------
## AVP = cantidad de muertes por DM por grupo de edad * esperanza de vida según grupo de edad

## Carga de paquetes
library(tidyverse)
library(readxl)
library(readr)

### Carga y limpieza de bases de datos ----

## Base de muertes por DM ----
# Serie 2005-2022 por jurisdicción, sexo y grupo de edad quinquenal - DEIS
load("serie_def.RData")

## Base de esperanza de vida (tabla del WHO-GHE) ----
espvida <- read_delim("Bases de datos/arg2019_espvida.csv", 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                                                                    grouping_mark = "."), trim_ws = TRUE)
# Armonización de nombres de variable "GRUPEDAD"
espvida <- espvida %>% mutate(GRUPEDAD = as.factor(case_when(
  GRUPEDAD == "1-4 years" ~ "01_1 a 4",
  GRUPEDAD == "5-9 years" ~ "02_5 a 9",
  GRUPEDAD == "10-14 years" ~ "03_10 a 14",
  GRUPEDAD == "15-19  years" ~ "04_15 a 19",
  GRUPEDAD == "20-24 years" ~ "05_20 a 24",
  GRUPEDAD == "25-29 years" ~ "06_25 a 29",
  GRUPEDAD == "30-34 years" ~ "07_30 a 34",
  GRUPEDAD == "35-39 years" ~ "08_35 a 39",
  GRUPEDAD == "40-44 years" ~ "09_40 a 44",
  GRUPEDAD == "45-49 years" ~ "10_45 a 49",
  GRUPEDAD == "50-54 years" ~ "11_50 a 54",
  GRUPEDAD == "55-59 years" ~ "12_55 a 59",
  GRUPEDAD == "60-64 years" ~ "13_60 a 64",
  GRUPEDAD == "65-69 years" ~ "14_65 a 69",
  GRUPEDAD == "70-74 years" ~ "15_70 a 74",
  GRUPEDAD == "75-79 years" ~ "16_75 a 79",
  GRUPEDAD == "80+ years" ~ "17_80 y más",
  .default = GRUPEDAD))
  )
    
# Filtro de grupos de edad de interés (desde 15-19) y selección de columnas
espvida <- espvida %>% 
  filter(
    grepl("04_|05_|06_|07_|08_|09_|10_|11_|12_|13_|14_|15_|16_|17_", GRUPEDAD)) %>% 
  select(-INDICADOR, -`Ambos sexos`)

# Pivoteo y creación de variable "SEXO"
espvida <- espvida %>% 
  pivot_longer(cols = 2:3,
               names_to = "SEXO_DESC",
               values_to = "ESP_VIDA")


## Cálculo de promedio fallecidos por trienio según año de ENFR y cálculo de AVAP ----

# AVP 2005 = falle 2005/06 --> no tengo falle del 2004
AVP_2005 <- serie_def %>% 
  filter(AÑO == 2005 | AÑO == 2006) %>% 
  group_by(PROVRES, PROV_DESC, SEXO, SEXO_DESC, GRUPEDAD) %>% 
  summarise(MEDIA_FALLE = mean(CUENTA)) %>% 
  left_join(espvida, by = join_by(GRUPEDAD == GRUPEDAD, SEXO_DESC == SEXO_DESC)) %>% 
  mutate(AVP = MEDIA_FALLE*ESP_VIDA,
         AÑO = 2005)

# AVP 2009 = falle 2008/09/10
AVP_2009 <- serie_def %>% 
  filter(AÑO == 2008 | AÑO == 2009 | AÑO == 2010) %>% 
  group_by(PROVRES, PROV_DESC, SEXO, SEXO_DESC, GRUPEDAD) %>% 
  summarise(MEDIA_FALLE = mean(CUENTA)) %>% 
  left_join(espvida, by = join_by(GRUPEDAD == GRUPEDAD, SEXO_DESC == SEXO_DESC)) %>% 
  mutate(AVP = MEDIA_FALLE*ESP_VIDA,
         AÑO = 2009)

# AVP 2013 = falle 2012/13/14
AVP_2013 <- serie_def %>% 
  filter(AÑO == 2012 | AÑO == 2013 | AÑO == 2014) %>% 
  group_by(PROVRES, PROV_DESC, SEXO, SEXO_DESC, GRUPEDAD) %>% 
  summarise(MEDIA_FALLE = mean(CUENTA)) %>% 
  left_join(espvida, by = join_by(GRUPEDAD == GRUPEDAD, SEXO_DESC == SEXO_DESC)) %>% 
  mutate(AVP = MEDIA_FALLE*ESP_VIDA,
         AÑO = 2013)

# AVP 2018 = falle 2017/18/19
AVP_2018 <- serie_def %>% 
  filter(AÑO == 2017 | AÑO == 2018 | AÑO == 2019) %>% 
  group_by(PROVRES, PROV_DESC, SEXO, SEXO_DESC, GRUPEDAD) %>% 
  summarise(MEDIA_FALLE = mean(CUENTA)) %>% 
  left_join(espvida, by = join_by(GRUPEDAD == GRUPEDAD, SEXO_DESC == SEXO_DESC)) %>% 
  mutate(AVP = MEDIA_FALLE*ESP_VIDA,
         AÑO = 2018)

## Serie de AVP ----
AVP_serie <- bind_rows(AVP_2005, AVP_2009, AVP_2013, AVP_2018) 

rm(list=setdiff(ls(), c("espvida", "serie_def", "AVP_serie")))

## Grupos teóricos ----
# Debería obtener 2688 por los grupos teóricos según combinación de categorías de las variables:
# - Año = 4 categorías
# - Prov = 24 categorías
# - Sexo = 2 categorías
# - Grupo de edad = 14 categorías

# Obtengo base con 2159 filas: debe haber grupos sin fallecidos, no se puede calcular AVP.

### A RESOLVER:
# - Promedio muertes en trienio alrededor de ENFR? Para ENFR 2005 no tengo datos de falle 2004.
# - Prevalencia de DM para cálculo de AVAD inicia en 18 años, para falle tengo grupos quinquenales


### Cálculo de AVD -------------------------------------------------------------
## AVD = suma[prev de secuela de DM * peso de la secuela]

## ENFR: Carga y limpieza de bases de datos ----

ENFR_2005 <- read_delim("Bases de datos/ENFR_bases/ENFR 2005 - Base usuario.txt", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)
ENFR_2005 <- ENFR_2005 %>% mutate(AÑO = 2005)

ENFR_2009 <- read_delim("Bases de datos/ENFR_bases/ENFR 2009 - Base usuario.txt", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)
ENFR_2009 <- ENFR_2009 %>% mutate(AÑO = 2009)

ENFR_2013 <- read_delim("Bases de datos/ENFR_bases/ENFR 2013 - Base usuario.txt", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)
ENFR_2013 <- ENFR_2013 %>% mutate(AÑO = 2013)

ENFR_2018 <- read_delim("Bases de datos/ENFR_bases/ENFR 2018 - Base usuario.txt", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)
ENFR_2018 <- ENFR_2018 %>% mutate(AÑO = 2018)

## ENFR: Selección de variables ----
# Las variables corresponden a respuestas de una persona seleccionada por hogar (no necesariamente el jefe de hogar)
# No hay grupos de edad, se muestran los datos por edad individual

# ENFR 2005
DM_05 <- ENFR_2005 %>% 
  select(AÑO, PROV, PONDERACION, 
         CHCH04, #sexo
         CHCH05,  #edad en años cumplidos
         CIDI01) #diagnóstico de DM por autorreporte
         
DM_05 <- DM_05 %>% rename(
  COD_PROV = PROV, 
  E_SEXO = CHCH04,
  E_EDAD = CHCH05,
  E_DM = CIDI01)

# ENFR 2009
DM_09 <- ENFR_2009 %>% 
  select(AÑO, PRVNC, PONDERACION, 
         BHCH04, #sexo
         BHCH05,  #edad en años cumplidos
         BIDI01) #diagnóstico de DM por autorreporte

DM_09 <- DM_09 %>% rename(
  COD_PROV = PRVNC, 
  E_SEXO = BHCH04,
  E_EDAD = BHCH05,
  E_DM = BIDI01)

# ENFR 2013
DM_13 <- ENFR_2013 %>% 
  select(AÑO, COD_PROVINCIA, PONDERACION, 
         BHCH04, #sexo
         BHCH05,  #edad en años cumplidos
         BIDI01) #diagnóstico de DM por autorreporte

DM_13 <- DM_13 %>% rename(
  COD_PROV = COD_PROVINCIA, 
  E_SEXO = BHCH04,
  E_EDAD = BHCH05,
  E_DM = BIDI01)

# ENFR 2018
DM_18 <- ENFR_2018 %>% 
  select(AÑO, cod_provincia, 
         wf1p, #factor de expansión Paso 1 (prov y aglom)
         bhch03, #sexo
         bhch04,  #edad en años cumplidos
         bidi01) #diagnóstico de DM por autorreporte

DM_18 <- DM_18 %>% rename(
  COD_PROV = cod_provincia,
  PONDERACION = wf1p,
  E_SEXO = bhch03,
  E_EDAD = bhch04,
  E_DM = bidi01)

## ENFR: Agregado de descriptores ----

ENFR_DM <- list(DM_05, DM_09, DM_13, DM_18) #armo lista para para guardar resultado del loop

for (i in 1:length(ENFR_DM)) {
  ENFR_DM[[i]] <- ENFR_DM[[i]] %>%
    mutate(PROV_DESC = case_when(
      COD_PROV == 2 ~ "CABA",
      COD_PROV == 6 ~ "Buenos Aires",
      COD_PROV == 10 ~ "Catamarca",
      COD_PROV == 14 ~ "Córdoba",
      COD_PROV == 18 ~ "Corrientes",
      COD_PROV == 22 ~ "Chaco",
      COD_PROV == 26 ~ "Chubut",
      COD_PROV == 30 ~ "Entre Ríos",
      COD_PROV == 34 ~ "Formosa",
      COD_PROV == 38 ~ "Jujuy",
      COD_PROV == 42 ~ "La Pampa",
      COD_PROV == 46 ~ "La Rioja",
      COD_PROV == 50 ~ "Mendoza",
      COD_PROV == 54 ~ "Misiones",
      COD_PROV == 58 ~ "Neuquén",
      COD_PROV == 62 ~ "Río Negro",
      COD_PROV == 66 ~ "Salta",
      COD_PROV == 70 ~ "San Juan",
      COD_PROV == 74 ~ "San Luis",
      COD_PROV == 78 ~ "Santa Cruz",
      COD_PROV == 82 ~ "Santa Fe",
      COD_PROV == 86 ~ "Santiago del Estero",
      COD_PROV == 90 ~ "Tucumán",
      COD_PROV == 94 ~ "Tierra del Fuego",
      .default = NA),
      SEXO_DESC = case_when(
        E_SEXO == 1 ~ "Varones",
        E_SEXO == 2 ~ "Mujeres",
        .default = NA
      ),
      DM_auto = case_when(
        E_DM == 1 ~ "SI",
        E_DM == 2 ~ "NO",
        E_DM == 9 | E_DM == 99 ~ "NS/NC",
        .default = NA),
      GRUPEDAD = 
        as.factor(
          case_when(
            E_EDAD >= 18 & E_EDAD < 20 ~ "04_15 a 19", #OJO QUE PREV DM ES DE 18-19
            E_EDAD >= 20 & E_EDAD < 25 ~ "05_20 a 24",
            E_EDAD >= 25 & E_EDAD < 30 ~ "06_25 a 29",
            E_EDAD >= 30 & E_EDAD < 35 ~ "07_30 a 34",
            E_EDAD >= 35 & E_EDAD < 40 ~ "08_35 a 39",
            E_EDAD >= 40 & E_EDAD < 45 ~ "09_40 a 44",
            E_EDAD >= 45 & E_EDAD < 50 ~ "10_45 a 49",
            E_EDAD >= 50 & E_EDAD < 55 ~ "11_50 a 54",
            E_EDAD >= 55 & E_EDAD < 60 ~ "12_55 a 59",
            E_EDAD >= 60 & E_EDAD < 65 ~ "13_60 a 64",
            E_EDAD >= 65 & E_EDAD < 70 ~ "14_65 a 69",
            E_EDAD >= 70 & E_EDAD < 75 ~ "15_70 a 74",
            E_EDAD >= 75 & E_EDAD < 80 ~ "16_75 a 79",
            E_EDAD >= 80 ~ "17_80 y más")
            ))
}


for (i in 1:length(ENFR_DM)) { #reasigno resultado del loop a las bases DM
  nombres_DM <- c("DM_05", "DM_09", "DM_13", "DM_18")
  assign(nombres_DM[[i]], ENFR_DM[[i]])
}

## Serie de DM ----
DM_serie <- rbind(DM_05, DM_09, DM_13, DM_18)

rm(list=setdiff(ls(), c("espvida", "serie_def", "AVP_serie", "DM_serie")))


## Prevalencia de DM por año, jurisdicción y grupo de edad ----

# Primero agrupo por año, provincia, sexo, grupo de edad y condición de DM,
# luego calculo prevalencia de DM en cada grupo

prev_DM <- DM_serie %>% 
  group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, DM_auto) %>% 
  summarise(recuento = sum(PONDERACION)) %>% 
  pivot_wider(names_from = DM_auto, values_from = recuento, values_fill = 0) %>% 
  mutate(prop_DM = round(SI/(SI+NO+`NS/NC`)*100, digits = 2),
                   total_DM = SI+NO+`NS/NC`) %>%
  select(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, SI, total_DM, prop_DM)

# Grupos teóricos = 2688; 
# Obtengo base de prevalencia de DM con 2688 filas, todos los grupos posibles con dato

## Frecuencia y pesos de secuelas de DM ----

# Para cada grupo de edad, a partir de prev de DM saco proporción de personas con cada secuela y luego multiplico por DW
# Se diferencian secuelas por sexo --> FALTA ACTUALIZAR INFO, QUEDA POR AHORA IGUAL POR SEXO
# Asumo misma proporción de secuela en cada grupo de edad, jurisdicción y año.

DW <- read_excel("Bases de datos/Complicaciones y DW.xlsx", 
                 sheet = "DW_DM")

## Agrego columnas con grupos de edad para join posterior con base de prevalencia de DM
# Agrego columnas con cada grupo y luego pivoteo para obtener una fila por grupo de edad para cada secuela

DW <- DW %>% 
  filter(!is.na(`Frecuencia (Wandurranga)`)) %>% 
  mutate("04_15 a 19" = "04_15 a 19",
        "05_20 a 24" = "05_20 a 24",
        "06_25 a 29" = "06_25 a 29",
        "07_30 a 34" = "07_30 a 34",
        "08_35 a 39" = "08_35 a 39",
        "09_40 a 44" = "09_40 a 44",
        "10_45 a 49" = "10_45 a 49",
        "11_50 a 54" = "11_50 a 54",
        "12_55 a 59" = "12_55 a 59",
        "13_60 a 64" = "13_60 a 64",
        "14_65 a 69" = "14_65 a 69",
        "15_70 a 74" = "15_70 a 74",
        "16_75 a 79" = "16_75 a 79",
        "16_75 a 79" = "16_75 a 79",
        "17_80 y más" = "17_80 y más"
        )

DW <- DW %>% 
  pivot_longer(cols = c(5:18),
               names_to = "GRUPO",
               values_to = "GRUPEDAD") %>% 
  select(-Sequela, -GRUPO)


## Cálculo de AVD ----

# Unión de tabla con prevalencia de DM y tabla con frec y pesos de secuelas
prev_DM_pesos <- prev_DM %>% 
  left_join(DW, by = join_by(GRUPEDAD == GRUPEDAD)) #base se multiplica x 4 porque hay 4 secuelas para cada grupo

# Primero calculo AVD aportado por cada secuela
prev_DM_pesos <- prev_DM_pesos %>% 
  mutate(AVD_indiv = #para obtener cantidad de años vividos multiplico cantidad de personas con DM, frecuencia de secuela y DW
           SI * `Frecuencia (Wandurranga)` / 100 * `Disability Weight`) 

# Sumo AVD de secuelas para obtener AVD total por grupo de edad
AVD_serie <- prev_DM_pesos %>% 
  group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, prop_DM) %>% #agrupo por estas dos variables para que me aparezcan ambas en la tabla (de todos modos para cada grupo de edad hay 1 prop_DM)
  summarise(AVD = sum(AVD_indiv)) #tiene que tener misma cantidad de filas que tabla con prev_DM

# Grupos teóricos = 2688
# Obtengo base de AVD con 2688 filas, todos los grupos posibles con dato


## Prueba de compatibilidad de bases AVP / AVD previa al macheo ----

# Transformo variables a factor y reordeno columnas en cada base
AVP_serie <- AVP_serie %>% 
  mutate(PROVRES = as.factor(PROVRES),
         PROV_DESC = as.factor(PROV_DESC),
         AÑO = as.factor(AÑO),
         SEXO = as.factor(SEXO),
         SEXO_DESC = as.factor(case_when(
           SEXO_DESC == "Masculino" ~ "Varones",
           SEXO_DESC == "Femenino" ~ "Mujeres")),
         GRUPEDAD = as.factor(GRUPEDAD)) %>% 
  select(AÑO, PROVRES, PROV_DESC, SEXO, SEXO_DESC, GRUPEDAD, AVP)
# AVP = 2159 filas

AVD_serie <- AVD_serie %>% 
  mutate(AÑO = as.factor(AÑO),
         PROV_DESC = as.factor(PROV_DESC),
         SEXO_DESC = as.factor(SEXO_DESC),
         GRUPEDAD = as.factor(GRUPEDAD))
# AVD = 2688 filas

# Base de AVD tiene más filas que AVP: no todos los grupos cuentan con fallecidos para calcular AVP
# Agrupo y macheo para ver qué categoría no tiene dato de AVP

prueba_AVP <- AVP_serie %>% 
  group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD) %>% 
  summarise(n_AVP=n())

prueba_AVD <- AVD_serie %>% 
  group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD) %>% 
  summarise(n_AVD=n())

prueba <- prueba_AVD %>% 
  left_join(prueba_AVP, by = join_by(AÑO == AÑO,
                                     PROV_DESC == PROV_DESC,
                                     SEXO_DESC == SEXO_DESC,
                                     GRUPEDAD == GRUPEDAD)) %>% 
  filter(is.na(n_AVP)) 

# Hay 529 categorías que no tienen AVP, es decir, no cuentan con fallecidos por DM, no se puede calcular AVP

rm(list=setdiff(ls(), c("espvida", "serie_def", "AVP_serie", "DM_serie", "AVD_serie")))


### Cálculo de AVAD ------------------------------------------------------------
## AVAD = AVP + AVD

AVAD_serie <- AVD_serie %>% #uso de base la serie de AVD porque es la más completa
  left_join(AVP_serie, 
            by = join_by(AÑO == AÑO, 
                         PROV_DESC == PROV_DESC, 
                         SEXO_DESC == SEXO_DESC,
                         GRUPEDAD == GRUPEDAD)) %>%
    mutate(
      AVP = case_when(
        is.na(AVP) ~ 0, #asigno valor 0 en filas que no tienen dato de AVP
        .default = AVP),
      AVAD = (AVD + AVP)) %>% 
  select(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, prop_DM, AVD, AVP, AVAD) #reordeno columnas


### Aporte de AVP y de AVD sobre AVAD totales por grupo de edad, sexo y año ----

prop_AVAD <- AVAD_serie %>% 
  group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD) %>% 
  summarise(AVP_acum = sum(AVP),
            AVD_acum = sum(AVD),
            AVAD_acum = sum(AVAD)) %>% 
  mutate(
    prop_AVP = round((AVP_acum / AVAD_acum * 100), 2),
    prop_AVD = round((AVD_acum / AVAD_acum * 100), 2)) %>% 
  pivot_longer(cols = c(prop_AVP, prop_AVD), names_to = "INDIC", values_to = "PROP") %>% 
  select(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, INDIC, PROP)


### Tasas de AVAD --------------------------------------------------------------

## Tasas brutas y específicas por grupo de edad, sexo y provincia ----

# No tengo proyecciones 2009, censo 1991/2001 tiene proyecciones cada 5 años, se usa 2010.

# Carga de población por año, sexo y jurisdicción
pobl_05 <- read_excel("Bases de datos/Proyecciones INDEC/proyec_2005.xlsx") #2005 (de proyecciones 2001-2015)
pobl_10_18 <- read_excel("Bases de datos/Proyecciones INDEC/proyec_2010_2018.xlsx") #2010, 2013, 2018 (de proyecciones 2010-2040)

pobl_05 <- pobl_05 %>% 
  mutate(AÑO = 2005) %>% 
  rename("Ambos sexos" = "AMBOS SEXOS",
         "Varones" = "VARONES", "Mujeres" = "MUJERES") %>% 
  select(GRUPEDAD, `Ambos sexos`, Varones, Mujeres, PROV_DESC, AÑO)

serie_pobl <- rbind(pobl_05, pobl_10_18)

# Armonización de variable "GRUPEDAD"
serie_pobl <- serie_pobl %>% 
  mutate(GRUPEDAD = 
           as.factor(
             case_when(
               GRUPEDAD == "0-4" ~ "01_1 a 4", #equiparo 0-4 de poblaciones con 1-4
               GRUPEDAD == "5-9" ~ "02_5 a 9",
               GRUPEDAD == "10-14" ~ "03_10 a 14",
               GRUPEDAD == "15-19" ~ "04_15 a 19",
               GRUPEDAD == "20-24" ~ "05_20 a 24",
               GRUPEDAD == "25-29" ~ "06_25 a 29",
               GRUPEDAD == "30-34" ~ "07_30 a 34",
               GRUPEDAD == "35-39" ~ "08_35 a 39",
               GRUPEDAD == "40-44" ~ "09_40 a 44",
               GRUPEDAD == "45-49" ~ "10_45 a 49",
               GRUPEDAD == "50-54" ~ "11_50 a 54",
               GRUPEDAD == "55-59" ~ "12_55 a 59",
               GRUPEDAD == "60-64" ~ "13_60 a 64",
               GRUPEDAD == "65-69" ~ "14_65 a 69",
               GRUPEDAD == "70-74" ~ "15_70 a 74",
               GRUPEDAD == "75-79" ~ "16_75 a 79",
               GRUPEDAD == "80-84" | GRUPEDAD == "85-89" | GRUPEDAD == "90-94" | 
                 GRUPEDAD == "95-99" | GRUPEDAD == "100 y más" ~ "17_80 y más",
               GRUPEDAD == "80ymas" ~ "17_80 y más",
               .default = GRUPEDAD))
             )

# Armado de base de población para unir con series de AVAD
pobl_join <- serie_pobl %>%
  select(-`Ambos sexos`) %>% #excluyo recuento total, asumo que si sumo sexos obtengo total (que no hay otra categoría)
  filter(grepl("04_|05_|06_|07_|08_|09_|10_|11_|12_|13_|14_|15_|16_|17_", GRUPEDAD)) %>% #excluyo filas de grupos de edad no necesarios
  pivot_longer(cols = c(Mujeres, Varones),
               names_to = "SEXO_DESC", values_to = "RECUENTO_pobl") %>% 
  group_by(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD) %>% 
  summarise(RECUENTO_pobl = sum(RECUENTO_pobl)) %>% 
  mutate(AÑO = as.factor(AÑO))


## Cálculo de tasas brutas de AVAD (generales y específicas)
tasas_esp_AVAD <- AVAD_serie %>% 
  select(AÑO, PROV_DESC, SEXO_DESC, GRUPEDAD, AVP, AVD, AVAD) %>% 
  mutate(AÑO_join = as.factor(case_when(
    AÑO == "2009" ~ "2010", #para prueba de cálculo de tasas, ver si se puede conseguir pobl de 2009
    .default = AÑO
  ))) %>% 
  left_join(pobl_join, by = join_by(AÑO_join == AÑO, PROV_DESC == PROV_DESC, SEXO_DESC == SEXO_DESC,
                                      GRUPEDAD == GRUPEDAD)) %>% 
  select(-AÑO_join)

tasas_esp_AVAD <- tasas_esp_AVAD %>% 
  mutate(tasa_AVP = round((AVP / RECUENTO_pobl * 100000),2),
         tasa_AVD = round((AVD / RECUENTO_pobl * 100000),2),
         tasa_AVAD = round((AVAD / RECUENTO_pobl * 100000),2))

tasas_gral_AVAD <- tasas_esp_AVAD %>% 
  group_by(AÑO, PROV_DESC, SEXO_DESC) %>% 
  summarise(pobl_tot = sum(RECUENTO_pobl),
            AVP_sum = sum(AVP),
            AVD_sum = sum(AVD),
            AVAD_sum = sum(AVAD),
            AVP_gral = AVP_sum/pobl_tot*100000,
            AVD_gral = AVD_sum/pobl_tot*100000,
            AVAD_gral = AVAD_sum/pobl_tot*100000) %>% 
  select(AÑO, PROV_DESC, SEXO_DESC, AVP_gral, AVD_gral, AVAD_gral)

tasas_nac <- tasas_esp_AVAD %>% 
  group_by(AÑO, SEXO_DESC, GRUPEDAD) %>% 
  summarise(pobl_nac = sum(RECUENTO_pobl),
            AVP_nac = sum(AVP),
            AVD_nac = sum(AVD),
            AVAD_nac = sum(AVAD),
            tasa_AVP_nac = AVP_nac/pobl_nac*100000,
            tasa_AVD_nac = AVD_nac/pobl_nac*100000,
            tasa_AVAD_nac = AVAD_nac/pobl_nac*100000) %>% 
  select(AÑO, SEXO_DESC, GRUPEDAD, AVP_nac, AVD_nac, AVAD_nac, tasa_AVP_nac, tasa_AVD_nac, tasa_AVAD_nac)
  
  
rm(list=setdiff(ls(), c("AVP_serie", "DM_serie", "AVD_serie", "AVAD_serie", "prop_AVAD",
                        "tasas_esp_AVAD", "tasas_gral_AVAD", "tasas_nac", "pobl_join")))


## Tasas ajustadas de AVAD ----

# Recorte de población de referencia: población total Argentina 2010, por sexo (último dato censal en el período de interés)
pobl_ref <- pobl_join %>% 
  filter(AÑO == 2010)

# Unión a base con tasas específicas para cálculo de tasas ajustadas (método directo)
base_aj <- tasas_esp_AVAD %>% 
  select(AÑO, GRUPEDAD, PROV_DESC, SEXO_DESC, tasa_AVP, tasa_AVD, tasa_AVAD) %>% 
  left_join(pobl_ref, by = join_by(GRUPEDAD == GRUPEDAD, PROV_DESC == PROV_DESC,
                                   SEXO_DESC == SEXO_DESC)) %>% 
  select(-AÑO.y) %>% 
  rename(AÑO = AÑO.x)

# Cálculo de tasas ajustadas
tasas_aj_AVAD <- base_aj %>% 
  mutate(AVP_ref = tasa_AVP*RECUENTO_pobl/100000,
         AVD_ref = tasa_AVD*RECUENTO_pobl/100000,
         AVAD_ref = tasa_AVAD*RECUENTO_pobl/100000) %>% 
  group_by(AÑO, PROV_DESC, SEXO_DESC) %>% 
  summarise(pobl_ref = sum(RECUENTO_pobl),
            AVP_sum = sum(AVP_ref),
            AVD_sum = sum(AVD_ref),
            AVAD_sum = sum(AVAD_ref),
            AVP_aj = AVP_sum/pobl_ref*100000,
            AVD_aj = AVD_sum/pobl_ref*100000,
            AVAD_aj = AVAD_sum/pobl_ref*100000) %>% 
  select(AÑO, PROV_DESC, SEXO_DESC, AVP_aj, AVD_aj, AVAD_aj)

# Tasas ajustadas nacionales
tasas_aj_nac <- tasas_nac %>% 
  select(AÑO, SEXO_DESC, GRUPEDAD, tasa_AVP_nac, tasa_AVD_nac, tasa_AVAD_nac) %>% 
  left_join((pobl_ref %>% 
               group_by(AÑO, SEXO_DESC, GRUPEDAD) %>% 
               summarise(pobl_nac = sum(RECUENTO_pobl))), 
            by = join_by(SEXO_DESC == SEXO_DESC, GRUPEDAD == GRUPEDAD)) %>% 
  select(-AÑO.y) %>% 
  rename(AÑO = AÑO.x)

tasas_aj_nac <- tasas_aj_nac %>% 
  mutate(AVP_ref = tasa_AVP_nac*pobl_nac/100000,
         AVD_ref = tasa_AVD_nac*pobl_nac/100000,
         AVAD_ref = tasa_AVAD_nac*pobl_nac/100000) %>% 
  group_by(AÑO, SEXO_DESC) %>% 
  summarise(pobl_ref = sum(pobl_nac),
            AVP_sum = sum(AVP_ref),
            AVD_sum = sum(AVD_ref),
            AVAD_sum = sum(AVAD_ref),
            AVP_aj = AVP_sum/pobl_ref*100000,
            AVD_aj = AVD_sum/pobl_ref*100000,
            AVAD_aj = AVAD_sum/pobl_ref*100000) %>% 
  select(AÑO, SEXO_DESC, AVP_sum, AVD_sum, AVAD_sum, AVP_aj, AVD_aj, AVAD_aj)


## Guardo archivos como RData ----
save(AVAD_serie, #cantidad de AVAD por año, provincia, sexo y grupo de edad
     file = "AVAD_serie.RData")

save(prop_AVAD, #proporción de AVP y AVD sobre AVAD por año, provincia, sexo y grupo de edad
     file = "prop_AVAD.RData")

save(tasas_esp_AVAD, #tasas específicas según grupo de edad de AVAD, AVP y AVD por año, provincia y sexo
     file = "tasas_eso_AVAD.RData")

save(tasas_gral_AVAD, #tasas brutas generales de AVAD, AVP y AVD por año, provincia y sexo
     file = "tasas_gral_AVAD.RData")

save(tasas_aj_AVAD, #tasas ajustadas de AVAD, AVP y AVD por año, provincia y sexo
     file = "tasas_aj_AVAD.RData")

save(tasas_nac, #recuento de AVAD, AVP y AVD y sus tasas específicas según grupo de edad, por año y sexo para el total país
     file = "tasas_nac.RData") 

save(tasas_aj_nac, #tasas ajustadas de AVAD, AVP y AVD por año y sexo para el total país
     file = "tasas_aj_nac.RData")

