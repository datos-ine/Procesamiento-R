# Complicaciones de DM ----------------------------------------------------

# Cálculo de frecuencias de complicaciones asociadas a DM y sus pesos de discapacidad (DW)
# correspondientes, en función de lo acordado con referentes de la Red Qualidiab.

# Resumen de criterios consensuados:
# * Se utilizarán los registros de DM2. Para esto hay que corregir aquellos sin dato en función del tratamiento: si tiene un antidiabético oral es DM2, si sólo tiene insulina y es mayor de 70, DM2, sino DM1.
# * Esto implica corregir las prevalencias de DM de la ENFR asumiendo que el 90% de las personas tienen DM2.
# * Se utilizarán todas las complicaciones registradas, salvo las aclaraciones detalladas a continuación.
# * En caso de existir más de un DW para la complicación se utilizará el promedio, salvo para el caso de retinopatías detallado a continuación.
# * Se calcularán frecuencias por sexo y grupos de edad. Jorge sugiere probar con grupos decenales o 4-5 grandes grupos.
# * Las frecuencias se calcularán a nivel nacional y se asumirá la misma distribución de frecuencias para cada región geográfica.
# * Complicaciones macrovasculares: 
#   - Se desestiman las siguientes complicaciones registradas: HVI, AIT e hipotensión ortostática.
#   - Stent se usará como proxy de IAM en caso de que el paciente no lo registre.
#   - Revascularización de miembros inferiores se usará como proxy de neuropatía periférica en caso de que el paciente no lo registre.
#   - CRM (cirugía de revascularización miocárdica) a confirmar por Jorge Elgart si se usará como proxy de IAM en caso de que el paciente no lo registre.
# * Complicaciones microvasculares:
#   - Disfunción eréctil: no tiene DW propio pero es una complicación de neuropatía periférica, se puede asumir el mismo DW.
#   - Retinopatía: para diferenciar la no proliferativa (más leve) de la proliferativa se opta por usar el promedio de DW para la "Retinopatía No proliferativa" y el DW severo para "Retinopatía proliferativa".
#   - Nefropatía: si usamos esta complicación diferenciada de "Diálisis/Transplante" se debería usar el promedio de DW para los estadíos 3-4 de enf renal crónica, sumando el estadío 5 a "Diálisis/Transplante".
#   Como alternativa, pueden combinarse ambas complicaciones y usar el promedio de los DW para estadíos 3-4-5 de ERC. Evaluar si hay diferencias para decidir.

### Autora: Micaela Gauto
### Fecha modificación:
# 2025-12-29


# Carga de paquetes -------------------------------------------------------
pacman::p_load(
  rio,
  readxl,
  tidyverse,
  skimr,
  gtsummary,    # resumen estadístico y tests
  rstatix,      # resumen estadístico y pruebas estadísticas
  janitor,      # añadir totales y porcentajes a las tablas
  scales,       # convertir fácilmente proporciones en porcentajes  
  flextable, # convertir tablas en imágenes bonitas
  writexl
)

# Carga de datos ----------------------------------------------------------

## Base Qualidiab 2014
qualidiab_2014 <- read_excel("Bases de datos/fichas_pacientes_QUALIDIAB_solo_ARG_2014.xlsx")


## Base de pesos de discapacidad para complicaciones de Qualidiab 
# (corregida para "Retinopatía proliferativa", "Disfunción eréctil" y "Nefropatía" (combinada con diálisis/tx) según criterios consensuados)
DW_GBD <- read_excel("Bases de datos/DW_GBD.xlsx")


# Limpieza de base --------------------------------------------------------

## Edición de variables Qualidiab ----
qualidiab_2014 <- qualidiab_2014 %>%   
  mutate(across(.cols = starts_with(c("cobertura", "complicaciones", "ojos")),
                .fns = ~ as_factor(case_when(
                  .x == 0 ~ "No",
                  .x == 1 ~ "Sí",
                  .default = "Sin dato"))),
         
         sexo = as_factor(case_when(
           sexo == 0 ~ "Mujer",
           sexo == 1 ~ "Varón"
           )),
         
         obito = as_factor(obito),
         fecha_de_nacimiento = as_date(fecha_de_nacimiento),
         registro_fecha = as_date(registro_fecha),
         edad = floor(interval(fecha_de_nacimiento, registro_fecha) / years(1)),
         
         # grupo_edad_5 = as_factor(case_when(
         #   edad >= 0 & edad < 5 ~ "01_0 a 4",
         #   edad >= 5 & edad < 10 ~ "02_5 a 9",
         #   edad >= 10 & edad < 15 ~ "03_10 a 14",
         #   edad >= 15 & edad < 20 ~ "04_15 a 19",
         #   edad >= 20 & edad < 25 ~ "05_20 a 24",
         #   edad >= 25 & edad < 30 ~ "06_25 a 29",
         #   edad >= 30 & edad < 35 ~ "07_30 a 34",
         #   edad >= 35 & edad < 40 ~ "08_35 a 39",
         #   edad >= 40 & edad < 45 ~ "09_40 a 44",
         #   edad >= 45 & edad < 50 ~ "10_45 a 49",
         #   edad >= 50 & edad < 55 ~ "11_50 a 54",
         #   edad >= 55 & edad < 60 ~ "12_55 a 59",
         #   edad >= 60 & edad < 65 ~ "13_60 a 64",
         #   edad >= 65 & edad < 70 ~ "14_65 a 69",
         #   edad >= 70 & edad < 75 ~ "15_70 a 74",
         #   edad >= 75 & edad < 80 ~ "16_75 a 79",
         #   edad >= 80 ~ "17_80 y más",
         #   .default = NA)),
         
         grupo_edad_10 = as_factor(case_when(
           edad >= 0 & edad < 10 ~ "0-9",
           edad >= 10 & edad < 20 ~ "10-19",
           edad >= 20 & edad < 30 ~ "20-29",
           edad >= 30 & edad < 40 ~ "30-39",
           edad >= 40 & edad < 50 ~ "40-49",
           edad >= 50 & edad < 60 ~ "50-59",
           edad >= 60 & edad < 70 ~ "60-69",
           edad >= 70 & edad < 80 ~ "70-79",
           edad >= 80 ~ "80+",
           .default = NA)),
         
         tiempo_dx = (edad - edad_al_diagnostico_de_la_diabetes),
         tiempo_dx = case_when(
           tiempo_dx < 0 ~ NA,
           .default = tiempo_dx
         ))

## Reordenamiento de niveles de la variable "grupo_edad" ----
qualidiab_2014 <- qualidiab_2014 %>% 
  mutate(
    # grupo_edad_5 = factor(grupo_edad_5, levels = c(
    #   "01_0 a 4",
    #   "02_5 a 9",
    #   "03_10 a 14",
    #   "04_15 a 19",
    #   "05_20 a 24",
    #   "06_25 a 29",
    #   "07_30 a 34",
    #   "08_35 a 39",
    #   "09_40 a 44",
    #   "10_45 a 49",
    #   "11_50 a 54",
    #   "12_55 a 59",
    #   "13_60 a 64",
    #   "14_65 a 69",
    #   "15_70 a 74",
    #   "16_75 a 79",
    #   "17_80 y más")),
    
    grupo_edad_10 = factor(grupo_edad_10, levels = c(
      "0-9",
      "10-19",
      "20-29",
      "30-39",
      "40-49",
      "50-59",
      "60-69",
      "70-79",
      "80+")))
    
## Corrección de complicaciones que tienen proxys ----
qualidiab_2014 <- qualidiab_2014 %>% 
  mutate(
    
    #Stent y CRM se usarán como proxy de IAM (confirmar CRM).
    complicaciones_iam_c = case_when(
      complicaciones_stent == "Sí" & complicaciones_iam == "No" ~ "Sí",
      complicaciones_stent == "Sí" & complicaciones_iam == "Sin dato" ~ "Sí",
      complicaciones_crm == "Sí" & complicaciones_iam == "No" ~ "Sí",
      complicaciones_crm == "Sí" & complicaciones_iam == "Sin dato" ~ "Sí",
      .default = complicaciones_iam
    ),
    
    #Revascularización se usará como proxy de neuropatía periférica.
    complicaciones_neuropatia_periferica_c = case_when(
      complicaciones_revascularizacion == "Sí" & complicaciones_neuropatia_periferica == "No" ~ "Sí",
      complicaciones_revascularizacion == "Sí" & complicaciones_neuropatia_periferica == "Sin dato" ~ "Sí",
      .default = complicaciones_neuropatia_periferica
    ),
    
    #Nefropatía -> 2 opciones
    #1. Diálisis/Transplante se usará como proxy de nefropatía (opción si usamos categoría combinada) -> Avanzamos con esta
    complicaciones_nefropatia_c = case_when(
      complicaciones_dialisis_transplante == "Sí" & complicaciones_nefropatia == "No" ~ "Sí",
      complicaciones_dialisis_transplante == "Sí" & complicaciones_nefropatia == "Sin dato" ~ "Sí",
      .default = complicaciones_nefropatia
    ),
    #2. Saco registro de nefropatía para los que tienen TX para no contar doble (opción si uso categorías por separado)
    complicaciones_nefropatia_c2 = case_when(
      complicaciones_dialisis_transplante == "Sí" & complicaciones_nefropatia == "Sí" ~ "No",
      .default = complicaciones_nefropatia
    ))


## Identificación de personas con diabetes sin complicaciones (usando complicaciones corregidas) ----

qualidiab_2014 <- qualidiab_2014 %>% 
  mutate(complic = # ausencia de cualquiera de las complicaciones referidas en la ficha (macro y microvasculares)
           case_when( 
             (complicaciones_ceguera == "No" | complicaciones_ceguera == "Sin dato") & 
               #complicaciones_dialisis_transplante == "No" & 
               (complicaciones_neuropatia_periferica_c == "No" | complicaciones_neuropatia_periferica_c == "Sin dato") &
               (complicaciones_nefropatia_c == "No" | complicaciones_nefropatia_c == "Sin dato") & 
               #complicaciones_hipo_ta_ortostatica == "No" & 
               (complicaciones_disfuncion_erectil == "No" | complicaciones_disfuncion_erectil == "Sin dato") & 
               (complicaciones_iam_c == "No" | complicaciones_iam_c == "Sin dato") & 
               (complicaciones_acv == "No"  | complicaciones_acv == "Sin dato") & 
               (complicaciones_claudicacion_miembros_inferiores == "No" | complicaciones_claudicacion_miembros_inferiores == "Sin dato") &
               #complicaciones_revascularizacion == "No" & 
               #complicaciones_hvi == "No" & 
               (complicaciones_ic == "No" | complicaciones_ic == "Sin dato") & 
               #complicaciones_ait == "No" &
               (complicaciones_amputacion == "No" | complicaciones_amputacion == "Sin dato") & 
               #complicaciones_crm == "No" & 
               #complicaciones_stent == "No" & 
               (ojos_retinopatia_no_proliferativa == "No" | ojos_retinopatia_no_proliferativa == "Sin dato") & 
               (ojos_retinopatia_proliferativa == "No" | ojos_retinopatia_proliferativa == "Sin dato")  ~ "Sin complicaciones",
             
      
    complicaciones_ceguera == "Sí" | 
      #complicaciones_dialisis_transplante == "Sí" | 
      complicaciones_neuropatia_periferica_c == "Sí" |
      complicaciones_nefropatia_c == "Sí" | 
      #complicaciones_hipo_ta_ortostatica == "Sí" | 
      complicaciones_disfuncion_erectil == "Sí" | 
      complicaciones_iam_c == "Sí" | 
      complicaciones_acv == "Sí" | 
      complicaciones_claudicacion_miembros_inferiores == "Sí" |
      #complicaciones_revascularizacion == "Sí" | 
      #complicaciones_hvi == "Sí" | 
      complicaciones_ic == "Sí" | 
      #complicaciones_ait == "Sí" |
      complicaciones_amputacion == "Sí" | 
      #complicaciones_crm == "Sí" | 
      #complicaciones_stent == "Sí" | 
      ojos_retinopatia_no_proliferativa == "Sí" | 
      ojos_retinopatia_proliferativa == "Sí" ~ "Alguna complicación"),
    
    complic_micro = # ausencia de complicaciones microvasculares en el estudio
      case_when(
      (complicaciones_ceguera == "No" | complicaciones_ceguera == "Sin dato") & 
        #complicaciones_dialisis_transplante == "No" & 
        (complicaciones_neuropatia_periferica_c == "No" | complicaciones_neuropatia_periferica_c == "Sin dato") &
        (complicaciones_nefropatia_c == "No" | complicaciones_nefropatia_c == "Sin dato") & 
        (complicaciones_disfuncion_erectil == "No"  | complicaciones_disfuncion_erectil == "Sin dato") & 
        (complicaciones_amputacion == "No" | complicaciones_amputacion == "Sin dato") & 
        (ojos_retinopatia_no_proliferativa == "No" | ojos_retinopatia_no_proliferativa == "Sin dato") & 
        (ojos_retinopatia_proliferativa == "No" | ojos_retinopatia_proliferativa == "Sin dato") ~ "Sin complicaciones",
      
      complicaciones_ceguera == "Sí" | 
        #complicaciones_dialisis_transplante == "Sí" | 
        complicaciones_neuropatia_periferica_c == "Sí" |
        complicaciones_nefropatia_c == "Sí" | 
        complicaciones_disfuncion_erectil == "Sí" | 
        complicaciones_amputacion == "Sí" | 
        ojos_retinopatia_no_proliferativa == "Sí" | 
        ojos_retinopatia_proliferativa == "Sí" ~ "Alguna complicación"),
    
    complic_macro = # ausencia de complicaciones macrovasculares en el estudio
      case_when( 
      #complicaciones_hipo_ta_ortostatica == "No" & 
        (complicaciones_iam_c == "No" | complicaciones_iam_c == "Sin dato") & 
        (complicaciones_acv == "No" | complicaciones_acv == "Sin dato") & 
        (complicaciones_claudicacion_miembros_inferiores == "No" | complicaciones_claudicacion_miembros_inferiores == "Sin dato") &
        #complicaciones_revascularizacion == "No" & 
        #complicaciones_hvi == "No" & 
        #complicaciones_ait == "No" &
        #complicaciones_crm == "No" & 
        #complicaciones_stent == "No" &
        (complicaciones_ic == "No" | complicaciones_ic == "Sin dato") ~ "Sin complicaciones",
      
      #complicaciones_hipo_ta_ortostatica == "Sí" | 
        complicaciones_iam_c == "Sí" | 
        complicaciones_acv == "Sí" | 
        complicaciones_claudicacion_miembros_inferiores == "Sí" |
        #complicaciones_revascularizacion == "Sí" | 
        #complicaciones_hvi == "Sí" | 
        #complicaciones_ait == "Sí" |
        #complicaciones_crm == "Sí" | 
        #complicaciones_stent == "Sí" |
        complicaciones_ic == "Sí" ~ "Alguna complicación"))
    
## Corrección de diagnósticos y filtrado de DM2 ----
qualidiab_2014 <- qualidiab_2014 %>% 
  
  mutate(tto_ado = case_when(
    tratamiento_sulfonilureas_valor != is.na(tratamiento_sulfonilureas_valor) |
      tratamiento_metformina_valor != is.na(tratamiento_metformina_valor) |
      tratamiento_glitazonas != is.na(tratamiento_glitazonas) |
      tratamiento_idpp4_valor != is.na(tratamiento_idpp4_valor) |
      tratamiento_arglp1_valor != is.na(tratamiento_arglp1_valor) |
      tratamiento_dm_otros != is.na(tratamiento_dm_otros) |
      tratamiento_dm_otros_cual != is.na(tratamiento_dm_otros_cual) ~ "antidiab_oral_si",
    
    is.na(tratamiento_sulfonilureas_valor) &
      is.na(tratamiento_metformina_valor) &
      is.na(tratamiento_glitazonas) &
      is.na(tratamiento_idpp4_valor) &
      is.na(tratamiento_arglp1_valor) &
      is.na(tratamiento_dm_otros) &
      is.na(tratamiento_dm_otros_cual) ~ "antidiab_oral_no"),
    
    tto_insu = case_when(
      tratamiento_nph != is.na(tratamiento_nph) |
        tratamiento_cristalina != is.na(tratamiento_cristalina) |
        tratamiento_analogos_lento != is.na(tratamiento_analogos_lento) |
        tratamiento_analogos_rapido != is.na(tratamiento_analogos_rapido) |
        tratamiento_numero_inyecciones_por_dia != is.na(tratamiento_numero_inyecciones_por_dia) ~ "insu_si",
      
      is.na(tratamiento_nph) &
        is.na(tratamiento_cristalina) &
        is.na(tratamiento_analogos_lento) &
        is.na(tratamiento_analogos_rapido) &
        is.na(tratamiento_numero_inyecciones_por_dia) ~ "insu_no"))

qualidiab_2014 <- qualidiab_2014 %>% 
  mutate(diagnostico_dm = case_when(
    antecedentes_dm2 == 1 & antecedentes_dm1 == 0 & antecedentes_dg == 0 ~ "dm2",
    antecedentes_dm2 == 0 & antecedentes_dm1 == 1 & antecedentes_dg == 0 ~ "dm1",
    antecedentes_dm2 == 0 & antecedentes_dm1 == 0 & antecedentes_dg == 1 ~ "dg",
    antecedentes_dm2 == 1 & antecedentes_dm1 == 1 & antecedentes_dg == 0 & 
      tto_ado == "antidiab_oral_si" ~ "dm2",
    antecedentes_dm2 == 0 & antecedentes_dm1 == 0 & antecedentes_dg == 0 & 
      tto_ado == "antidiab_oral_si" ~ "dm2",
    antecedentes_dm2 == 0 & antecedentes_dm1 == 0 & antecedentes_dg == 0 & 
      tto_ado == "antidiab_oral_no" & tto_insu == "insu_si" & edad < 70 ~ "dm1", # si sólo se trata con insulina y es menor de 70 = DM1
    antecedentes_dm2 == 0 & antecedentes_dm1 == 0 & antecedentes_dg == 0 & 
      tto_ado == "antidiab_oral_no" & tto_insu == "insu_si" & edad >= 70 ~ "dm2", # si sólo se trata con insulina y es tiene 70+ = DM2 insulinodep
    .default = "otro"
  ))

# Chequeo recategorización
qualidiab_2014 %>% 
  group_by(diagnostico_dm, antecedentes_dm2, antecedentes_dm1, antecedentes_dg) %>% 
  summarise(n=n()) %>% 
  view()

## En la nueva categorización de DM2 hay: 1261 registros que en la base tenían DM2, 16 registros nuevos por tratamiento y 1 registro con doble antecedente que queda como DM2 por tratamiento.
## Los que en la recategorización son "otros" corresponden a personas con pre-diabetes.

### Filtrado de DM2 ----
qualidiab_2014_dm2 <- qualidiab_2014 %>% 
  filter(diagnostico_dm == "dm2")


### Filtrado de base de DW según secuelas a utilizar ----

DW_GBD_recorte <- DW_GBD %>% 
  group_by(`tipo_complicación crónica`, `complicación crónica_Qualidiab`) %>% 
  summarise(DW_promedio = sum(DW_secuela)/length(DW_secuela)) %>% 

  filter(DW_promedio != is.na(DW_promedio))

# ### Agrego nefropatía separada de diálisis/transplante
# nefro_sep <- DW_GBD %>% 
#   filter(`complicación crónica_Qualidiab` == "Nefropatía" & 
#            (str_detect(secuela_GBD, "Stage 3") | str_detect(secuela_GBD, "Stage 4"))) %>% 
#   group_by(`tipo_complicación crónica`, `complicación crónica_Qualidiab`) %>% 
#   summarise(DW_promedio = sum(DW_secuela)/length(DW_secuela)) %>%
#   mutate(`complicación crónica_Qualidiab` = case_when(
#     `complicación crónica_Qualidiab` == "Nefropatía" ~ "Nefropatía_sep",
#     .default = `complicación crónica_Qualidiab`
#   ))

# DW_GBD_recorte <- DW_GBD_recorte %>%
#   bind_rows(nefro_sep)


# Cálculo de frecuencias por sexo y grupos de edad ------------------------

## Grupos de edad decenales ----
complic_frecyDW_10 <- qualidiab_2014_dm2 %>% 
  
  select(c(sexo, grupo_edad_10, 
           complic, 
           complicaciones_iam_c, 
           complicaciones_ic,
           complicaciones_acv,
           complicaciones_claudicacion_miembros_inferiores,
           ojos_retinopatia_no_proliferativa,
           ojos_retinopatia_proliferativa,
           complicaciones_ceguera, 
           complicaciones_nefropatia_c, #si se usa nefropatía y diálisis/transplante por separado, cambiar
           complicaciones_neuropatia_periferica_c, 
           complicaciones_amputacion,
           complicaciones_disfuncion_erectil)) %>% 
  
  pivot_longer(cols = 3:14, names_to = "complicacion", values_to = "presencia") %>%    
  
  mutate(complicacion = case_when(
    complicacion == "complic" ~ "Sin complicaciones",
    complicacion == "complicaciones_iam_c" ~ "IAM",
    complicacion == "complicaciones_ic" ~ "IC",
    complicacion == "complicaciones_acv" ~ "ACV",
    complicacion == "complicaciones_claudicacion_miembros_inferiores" ~ "Claudicación miembros inferiores",
    complicacion == "ojos_retinopatia_no_proliferativa" ~ "Retinopatía no proliferativa",
    complicacion == "ojos_retinopatia_proliferativa" ~ "Retinopatía proliferativa",
    complicacion == "complicaciones_ceguera" ~ "Ceguera", 
    complicacion == "complicaciones_nefropatia_c" ~ "Nefropatía", 
    complicacion == "complicaciones_neuropatia_periferica_c" ~ "Neuropatía periférica",
    complicacion == "complicaciones_amputacion" ~ "Amputación",
    complicacion == "complicaciones_disfuncion_erectil" ~ "Disfunción eréctil"),   
    
    complicacion = factor(complicacion, levels = c(     
      "Sin complicaciones",
      "IAM",
      "IC",
      "ACV",
      "Claudicación miembros inferiores",
      "Retinopatía no proliferativa",
      "Retinopatía proliferativa",
      "Ceguera", 
      "Nefropatía", 
      "Neuropatía periférica", 
      "Amputación",
      "Disfunción eréctil"))) %>% 
  
  group_by(sexo, grupo_edad_10, complicacion, presencia) %>% 
  summarise(recuento = n()) %>% 
  ungroup() %>% 
  group_by(sexo, grupo_edad_10, complicacion) %>% 
  mutate(frec_compli = recuento/sum(recuento)) %>% 
  
  filter(presencia == "Sí" | presencia == "Sin complicaciones") %>% 
  
  select(-presencia, -recuento) %>%
  
  left_join(DW_GBD_recorte,
            by = join_by(complicacion == `complicación crónica_Qualidiab`)) %>%
  
  mutate(DW_promedio = case_when(
    complicacion == "Sin complicaciones" ~ 0.0490114147,
    .default = DW_promedio
  )) %>% 
  
  select(sexo, grupo_edad_10, `tipo_complicación crónica`, complicacion, frec_compli, DW_promedio) %>% 
  
  rename("tipo_complicacion" = "tipo_complicación crónica",
         "DW" = "DW_promedio") %>%
  ungroup()


# Guardar datos limpios ---------------------------------------------------
write_csv(complic_frecyDW_10, file = "Bases de datos/clean/frec_DW_complic_sexo_ge10.csv")


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = c(
    # "anio_enfr", #cuando se agregue, habilitar
    "sexo", 
    "grupo_edad_10",
    "tipo_complicacion", 
    "complicacion",
    "frec_compli",
    "DW"),
  
  descripcion = c(
    # "Año de realización ENFR",
    "Sexo biológico",
    "Grupo de edad decenal",
    "Tipo de complicación crónica",
    "Nombre de la complicación crónica",
    "Frecuencia de la complicación crónica según sexo y grupo etario",
    "Peso de discapacidad asociado a la complicación"),
  
  tipo_var = c(rep("factor", 4), rep("numeric", 2)),
  
  valores = list(
    c("Varón", "Mujer"),
    levels(qualidiab_2014_dm2$grupo_edad_10),
    levels(DW_GBD_recorte$`tipo_complicación crónica`),
    levels(DW_GBD_recorte$`complicación crónica_Qualidiab`),
   "0-Inf", 
   "0-Inf") |> 
    as.character() |> 
    str_remove_all('^c\\(|\\)$|"')
)


## Guardar diccionario de datos
export(data_dict, file = "Bases de datos/clean/dic_frec_DW_complic.xlsx")
