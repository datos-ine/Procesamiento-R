# Exploración base de datos QUALIDIAB -------------------------------------


# Carga de paquetes -------------------------------------------------------
pacman::p_load(
  readxl,
  tidyverse,
  skimr,
  gtsummary,    # resumen estadístico y tests
  rstatix,      # resumen estadístico y pruebas estadísticas
  janitor,      # añadir totales y porcentajes a las tablas
  scales,       # convertir fácilmente proporciones en porcentajes  
  flextable     # convertir tablas en imágenes bonitas
)

# Carga de datos ----------------------------------------------------------

qualidiab_2014 <- read_excel("Bases de datos/fichas_pacientes_QUALIDIAB_solo_ARG_2014.xlsx")

glimpse(qualidiab_2014)

# variables <- names(qualidiab_2014)
# variables_df <- data.frame(var = variables,
#                            y = 1)
# write_csv2(variables_df, "variables_qualidiab.csv")

skim(qualidiab_2014)



# Limpieza de base --------------------------------------------------------

qualidiab_2014 <- qualidiab_2014 %>% 
  
  # # Selección de variables
  # select(id_udc, #id de unidad de reporte
  #        paciente_id, clave, #id de paciente
  #        fecha_de_nacimiento, 
  #        sexo, #0: femenino; 1: masculino
  #        obito, 
  #        pais, 
  #        registro_fecha, 
  #        starts_with(c("cobertura", "complicaciones", "ojos"))) %>% #cobertura 0/1; complicaciones 0/1
   
  # Edición de variables
  mutate(across(.cols = starts_with(c("cobertura", "complicaciones", "ojos")),
                .fns = ~ as_factor(case_when(
                  .x == 0 ~ "No",
                  .x == 1 ~ "Sí",
                  .default = "Sin dato"))),
         sexo = as_factor(case_when(
           sexo == 0 ~ "Femenino",
           sexo == 1 ~ "Masculino"
           )),
         obito = as_factor(obito),
         fecha_de_nacimiento = as_date(fecha_de_nacimiento),
         registro_fecha = as_date(registro_fecha),
         edad = floor(interval(fecha_de_nacimiento, registro_fecha) / years(1)),
         grupedad = as_factor(case_when(
           edad >= 0 & edad < 5 ~ "01_0 a 4",
           edad >= 5 & edad < 10 ~ "02_5 a 9",
           edad >= 10 & edad < 15 ~ "03_10 a 14",
           edad >= 15 & edad < 20 ~ "04_15 a 19",
           edad >= 20 & edad < 25 ~ "05_20 a 24",
           edad >= 25 & edad < 30 ~ "06_25 a 29",
           edad >= 30 & edad < 35 ~ "07_30 a 34",
           edad >= 35 & edad < 40 ~ "08_35 a 39",
           edad >= 40 & edad < 45 ~ "09_40 a 44",
           edad >= 45 & edad < 50 ~ "10_45 a 49",
           edad >= 50 & edad < 55 ~ "11_50 a 54",
           edad >= 55 & edad < 60 ~ "12_55 a 59",
           edad >= 60 & edad < 65 ~ "13_60 a 64",
           edad >= 65 & edad < 70 ~ "14_65 a 69",
           edad >= 70 & edad < 75 ~ "15_70 a 74",
           edad >= 75 & edad < 80 ~ "16_75 a 79",
           edad >= 80 ~ "17_80 y más",
           .default = NA)))

# Reordenamiento de niveles de la variable "grupedad"
qualidiab_2014 <- qualidiab_2014 %>% 
  mutate(grupedad = factor(grupedad, levels = c("01_0 a 4",
                           "02_5 a 9",
                           "03_10 a 14",
                           "04_15 a 19",
                           "05_20 a 24",
                           "06_25 a 29",
                           "07_30 a 34",
                           "08_35 a 39",
                           "09_40 a 44",
                           "10_45 a 49",
                           "11_50 a 54",
                           "12_55 a 59",
                           "13_60 a 64",
                           "14_65 a 69",
                           "15_70 a 74",
                           "16_75 a 79",
                           "17_80 y más")))
                           
# Análisis exploratorio ---------------------------------------------------

## Distribución de casos según tipo de diabetes ----
qualidiab_2014 %>% 
  count(antecedentes_dm2, antecedentes_dm1, antecedentes_dg) %>% 
  view()

# En el 2014, de los 1365 registros, hay 1262 casos con DM2, 80 personas con DM1, 2 con diabetes gestacional y 21 personas sin DM1, DM2 o DG.
# Nos deberíamos quedar sólo con los que tienen DM2?


## Distribución de casos según sexo y edad ----
qualidiab_2014 %>% 
  count(sexo, grupedad) %>% 
  view()

qualidiab_2014 %>% 
  ggplot(aes(x = grupedad)) +
  geom_histogram(stat = "count") +
  labs(x = "Grupos de edad",
       y = "Recuento") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~sexo) 


## Distribución de casos según unidad de reporte ----
qualidiab_2014 %>% 
  tabyl(id_udc) %>% 
  adorn_pct_formatting() %>% 
  view()
# Variable para clasificar por región --> falta criterio de clasificación
# Evaluar si vale la pena esta desagregación


## Frecuencia de comorbilidades seleccionadas según sexo ----
qualidiab_2014 %>% 
  select(c(sexo, edad, ojos_retinopatia_no_proliferativa, ojos_retinopatia_proliferativa, complicaciones_ceguera,
           complicaciones_nefropatia, complicaciones_dialisis_transplante,
           complicaciones_neuropatia_periferica, complicaciones_amputacion, complicaciones_disfuncion_erectil)) %>% 
  tbl_summary(
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",   
                     all_categorical() ~ "{n} / {N} ({p}%)"),               
    digits = all_continuous() ~ 1,                         
    type   = all_categorical() ~ "categorical",
    by = sexo, 
    label  = list(
      sexo = "Sexo",
      edad ~ "Edad", 
      ojos_retinopatia_no_proliferativa ~ "Retinopatía no proliferativa", 
      ojos_retinopatia_proliferativa ~ "Retinopatía proliferativa", 
      complicaciones_ceguera ~ "Ceguera",
      complicaciones_nefropatia ~ "Nefropatía", 
      complicaciones_dialisis_transplante ~ "Diálisis/Transplante",
      complicaciones_neuropatia_periferica ~ "Neuropatía periférica", 
      complicaciones_amputacion ~ "Amputación", 
      complicaciones_disfuncion_erectil ~ "Disfrunción eréctil"),
    missing_text = "Sin dato"                               
  ) %>% 
  modify_header(label ~ "**Variable**") %>% 
  add_p() %>% 
  as_flex_table() 
  # flextable::autofit() %>%                      
  # flextable::save_as_docx(path = "tabla2.docx") 
