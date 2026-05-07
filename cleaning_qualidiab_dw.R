### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
###  en Argentina, período 2005-2018
### Limpieza y procesamiento de los datasets:
## - Complicaciones asociadas a DM en Argentina, según datos provistos por la red
## QUALIDIAB correspondientes al año 2014.
## - Pesos de discapacidad (DW) asociados a DM según tablas publicadas por el GBD
### Corrección de los registros sin datos:
## - DM2: antidiabético oral y/o mayor de 70 años con tratamiento de insulina
## - DM1: menor de 70 años con tratamiento de insulina
## - IAM: se usará como proxy si el paciente recibió stent o CRM (cirugía de revascularización miocárdica)
#  a confirmar por Jorge Elgart
## - Neuropatía periférica: se usará como proxy si el paciente fue revascularizado.
## - No se considerarán en el análisis: HVI, AIT e hipotensión ortostática.
### Corrección de DW:
## - Si existe más de un DW para la complicación se utilizará el promedio.
## - Retinopatía no proliferativa: se usará el promedio de DW para retinopatías np.
## - Retinopatía proliferativa: se usará el DW correspondiente a retinopatía severa.
## - Disfunción eréctil: se asumirá el mismo DW que para neuropatía periférica.
## - Nefropatía: se evaluará usar los DW promedio de estadíos 3-5 (incluye diálisis/
## transplante) o evaluar por separado nefropatía (DW promedio estadíos 3-4) y
## diálisis/transplante (DW estadío 5).
### Autoras:
## - Micaela Gauto
## - Tamara Ricardo
# Última modificación: 19-02-2026 13:30

# Carga de paquetes -------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  epikit,
  flextable,
  tidyverse
)

# Carga de datos ----------------------------------------------------------
## Qualidiab 2005 y 2009 ----
qualidiab_05_09_raw <- import(
  "bases_datos/QUALIDIAB_2005_2009_anonimo_arg.xlsx"
)

## Qualidiab 2013 y 2018 ----
qualidiab_13_18_raw <- import(
  "bases_datos/QUALIDIAB_2013_2018_anonimo_arg.xlsx"
)

## Pesos de discapacidad para complicaciones de Qualidiab ----
# (corregida para "Retinopatía proliferativa", "Disfunción eréctil" y "Nefropatía"
#  (combinada con diálisis/tx) según criterios consensuados)
DW_GBD_raw <- import("bases_datos/DW_GBD.xlsx")


# Limpiar datos ----------------------------------------------------------

## Qualidiab 2005 y 2009 ----

qualidiab_2005_2009 <- qualidiab_05_09_raw |>
  
  # Renombrar columnas
  rename(
    anio = Año,
    comp_claud_mi = complicaciones_claudicacion_miembros_inferiores,
    comp_retinopatia_np = ojos_retinopatia_no_proliferativa,
    comp_retinopatia_pp = ojos_retinopatia_preproliferativa,
    comp_retinopatia_p = ojos_retinopatia_proliferativa
  ) |>
  
  # # Acortar nombres de columnas
  # rename_with(
  #   .cols = starts_with(c("cob", "ant", "exp", "aut", "tab", "hos")),
  #   .fn = ~ str_replace(., "^.*?_", paste0(str_sub(., 1, 3), "_"))
  # ) |>
  
  rename_with(
    .cols = starts_with(c("sint", "comp", "trat")),
    .fn = ~ str_replace(., "^.*?_", paste0(str_sub(., 1, 4), "_"))
  ) |>
  
  # Cambiar niveles variables binarias
  mutate(
    across(
      .cols = starts_with(c("cob", "ant", "comp", "obito")),
      .fns = ~ if_else(.x == 0, "No", "Sí", missing = "Sin datos")
    )
  ) |>
  
  mutate(
    across(
      .cols = starts_with("trat"),
      .fns = ~ case_when(.x == 1 ~ "Sí",
                         .x == 0 ~ "No",
                         is.na(.x) ~ "Sin datos",
                         .default = as.character(.x))
    )
  ) |>
  
  # Cambiar niveles sexo
  mutate(sexo = if_else(sexo == 0, "Mujer", "Varón")) |>
  
  # Cambiar formato fechas
  mutate(across(.cols = starts_with("fecha"), .fns = ~ ymd(.x))) |>

  # Calcular edad: no hay fecha registro, se calcula edad a mitad de período.
  mutate(
    edad = case_when(
      anio == 2005 ~
        interval(fecha_de_nacimiento, "2005-06-30") |>
        time_length(unit = "year") |>
        floor(),
      anio == 2009 ~
        interval(fecha_de_nacimiento, "2009-06-30") |>
        time_length(unit = "year") |>
        floor()
    )
  ) |>
  
  # Crear variable para grupo etario decenal
  mutate(
    grupo_edad_10 = age_categories(
      edad,
      lower = 0,
      upper = 80,
      by = 10,
      separator = " a "
    )
  ) |>
  
  # Corregir neuropatía periférica usando revascularización como proxy
  mutate(
    comp_neurop_perif_c = if_else(
      comp_neuropatia_periferica != "Sí" & comp_revascularizacion == "Sí",
      "Sí",
      comp_neuropatia_periferica
    )
  ) |>
  
  # Corregir nefropatía
  mutate(
    # Usar diálisis/transplante como proxy
    comp_nefropatia_c1 = if_else(
      comp_nefropatia != "Sí" & comp_dialisis_transplante == "Sí",
      "Sí",
      comp_nefropatia
    )
  ) |>
  
  # Unificar amputación
  mutate(
    comp_amputacion_c = if_else(
      comp_amputacion_sobre_tobillo == "Sí" | comp_amputacion_debajo_tobillo == "Sí",
      "Sí",
      "No"
    )
  ) |>
  
  # Unificar retinopatía pre y no proliferativa
  mutate(
    comp_retinopatia_np_c = if_else(
      comp_retinopatia_pp == "Sí" | comp_retinopatia_np == "Sí",
    "Sí",
    "No"
    )
  ) |>
  
  # Crear variable para presencia/ausencia de complicaciones
  mutate(
    comp_alguna = if_else(
      # Si cualquiera de las columnas tiene un "Sí"
      if_all(
        .cols = contains(c("ceguera", "disfuncion", "amputacion_c", "retinopatia_p", "retinopatia_np_c",
                           "neurop_perif_c", "nefropatia_c1",
                           "acv", "claud_mi", 
                           "iam")),
        ~ .x != "Sí"
      ),
      "No",
      "Sí"
    )
  ) |>
  
  # Crear variable para presencia/ausencia de complicaciones microvasculares
  mutate(
    comp_micro = if_else(
      # Si cualquiera de las columnas tiene un "Sí"
      if_all(
        .cols = contains(c("ceguera", "disfuncion", "amputacion_c", "retinopatia_p", "retinopatia_np_c",
                           "neurop_perif_c", "nefropatia_c1")),
        .fns = ~ .x != "Sí"
      ),
      "No",
      "Sí"
    )
  ) |>
  
  # Crear variable para presencia/ausencia de complicaciones macrovasculares
  mutate(
    comp_macro = if_else(
      # Si cualquiera de las columnas tiene un "Sí"
      if_all(
        .cols = contains(c(
          "acv", "claud_mi", 
          #"comp_ic", --> No disponible en 2005-2009
          "iam"
        )),
        .fns = ~ .x != "Sí"
      ),
      "No",
      "Sí"
    )
  ) |>
  
  # Crear variable para tratamiento oral
  mutate(
    trat_oral = if_else(
      if_any(
        .cols = starts_with("trat") &
          contains(c("biguanidas", "sulfonilureas", "meglitidinas",
                     "inh_glucosidasas", "tiazolidinedionas")),
        .fns = ~ .x == "Sí" ) |
      trat_otro_cual == "GLEMAZ" | 
        trat_otro_cual == "METF*" | 
        trat_otro_cual == "GLIBLU*" | 
        trat_otro_cual == "PPAR DUAL",
        
      "Sí",
      "No"
    )
  ) |>
  
  # Crear variable para tratamiento insulina
  mutate(
    trat_insu = if_else(trat_insulina_si_no == "Sí" | trat_pen_o_bomba == "Sí",
      "Sí",
      "No"
    )
  ) |>
  
  # Crear variable para tipo de diabetes
  mutate(
    tipo_dm = case_when(
      
      # Renombre de categorías
      diabetes_tipo == 0 ~ "DM1",
      diabetes_tipo == 1 ~ "DM2",
      diabetes_tipo == 2 ~ "DMG",
      
      # Clasificación por Proxy (Tratamiento + Edad)
      diabetes_tipo == 3 &
        (trat_oral == "Sí" |
           (trat_oral == "No" & trat_insu == "Sí" & edad >= 70)) ~ "DM2",
      
      diabetes_tipo == 3 & trat_oral == "No" & trat_insu == "Sí" & edad < 70 ~ "DM1",
      
      # Cualquier otro caso no contemplado
      .default = "DM_otra"
    )
  ) |>
  
  # Variables caracter a factor
  mutate(across(.cols = where(is.character), 
                .fns = ~ factor(.x)
  ))


## Qualidiab 2013 y 2018 ----

qualidiab_2013_2018 <- qualidiab_13_18_raw |>
  
  # Renombrar columnas
  rename(
    anio = Año,
    comp_claud_mi = complicaciones_claudicacion_miembros_inferiores,
    comp_retinopatia_np = ojos_retinopatia_no_proliferativa,
    comp_retinopatia_p = ojos_retinopatia_proliferativa
  ) |>

  # Acortar nombres de columnas
  rename_with(
    .cols = starts_with(c("cob", "ant", "exp", "aut", "tab", "hos")),
    .fn = ~ str_replace(., "^.*?_", paste0(str_sub(., 1, 3), "_"))
  ) |>

  rename_with(
    .cols = starts_with(c("sint", "comp", "trat")),
    .fn = ~ str_replace(., "^.*?_", paste0(str_sub(., 1, 4), "_"))
  ) |>

  # Cambiar niveles variables binarias
  mutate(
    across(
      .cols = starts_with(c("cob", "ant", "com", "obito")),
      .fns = ~ if_else(.x == 0, "No", "Sí", missing = "Sin datos")
    )
  ) |>

  # Cambiar niveles sexo
  mutate(sexo = if_else(sexo == 0, "Mujer", "Varón")) |>

  # Cambiar formato fechas
  mutate(across(.cols = starts_with("fecha"), .fns = ~ ymd(.x))) |>

  # Calcular edad: no hay fecha registro, se calcula edad a mitad de período.
  mutate(
    edad = case_when(anio == 2013 ~
                       interval(fecha_de_nacimiento, "2013-06-30") |>
                       time_length(unit = "year") |>
                       floor(),
                     anio == 2018 ~
                       interval(fecha_de_nacimiento, "2018-06-30") |>
                       time_length(unit = "year") |>
                       floor()
    )
  ) |>

  # Crear variable para grupo etario decenal
  mutate(
    grupo_edad_10 = age_categories(
      edad,
      lower = 0,
      upper = 80,
      by = 10,
      separator = " a "
    )
  ) |>

  # Corregir IAM con Stent como proxy
  mutate(
    comp_iam_c = if_else(
      comp_iam != "Sí" & comp_stent == "Sí",
      "Sí",
      comp_iam
    )
  ) |>

  # Corregir neuropatía periférica usando revascularización como proxy
  mutate(
    comp_neurop_perif_c = if_else(
      comp_neuropatia_periferica != "Sí" & comp_revascularizacion == "Sí",
      "Sí",
      comp_neuropatia_periferica
    )
  ) |>

  # Corregir nefropatía
  mutate(
    # Usar diálisis/transplante como proxy
    # (opción si usamos categoría combinada) -> Avanzamos con esta
    comp_nefropatia_c1 = if_else(
      comp_nefropatia != "Sí" & comp_dialisis_transplante == "Sí",
      "Sí",
      comp_nefropatia
    )
  ) |>

  # Crear variable para presencia/ausencia de complicaciones
  mutate(
    comp_alguna = if_else(
      # Si cualquiera de las columnas tiene un "Sí"
      if_all(
        .cols = contains(c("ceguera", "disfuncion", "amputacion", "retinopatia",
                           "neurop_perif_c", "nefropatia_c1",
                           "acv", "claud_mi", "comp_ic", "iam_c")),
        ~ .x != "Sí"
      ),
      "No",
      "Sí"
    )
  ) |>

  # Crear variable para presencia/ausencia de complicaciones microvasculares
  mutate(
    comp_micro = if_else(
      # Si cualquiera de las columnas tiene un "Sí"
      if_all(
        .cols = contains(c("ceguera", "disfuncion", "amputacion", "retinopatia",
                           "neurop_perif_c", "nefropatia_c1")),
        .fns = ~ .x != "Sí"
      ),
      "No",
      "Sí"
    )
  ) |>

  # Crear variable para presencia/ausencia de complicaciones macrovasculares
  mutate(
    comp_macro = if_else(
      # Si cualquiera de las columnas tiene un "Sí"
      if_all(
        .cols = contains(c(
          "acv", "claud_mi", "comp_ic", "iam_c"
        )),
        .fns = ~ .x != "Sí"
      ),
      "No",
      "Sí"
    )
  ) |>

  # Crear variable para tratamiento oral
  mutate(
    trat_oral = if_else(
      if_all(
        .cols = starts_with("trat") &
          contains(c("sul", "met", "gli", "idpp4", "arglp1", "dm_otros")),
        .fns = ~ is.na(.x)
      ),
      "No",
      "Sí"
    )
  ) |>

  # Crear variable para tratamiento insulina
  mutate(
    trat_insu = if_else(
      if_all(
        .cols = starts_with("trat") &
          contains(c("nph", "cris", "analogos", "numero_inyecciones")),
        .fns = ~ is.na(.x)
      ),
      "No",
      "Sí"
    )
  ) |>

  # Crear variable para tipo de diabetes
  mutate(
    tipo_dm = case_when(
      # Diagnóstico único
      ant_dm1 == "Sí" & if_all(c(ant_dm2, ant_dg), ~ .x == "No") ~ "DM1",
      ant_dm2 == "Sí" & if_all(c(ant_dm1, ant_dg), ~ .x == "No") ~ "DM2",
      ant_dg == "Sí" & if_all(starts_with("ant_dm"), ~ .x == "No") ~ "DMG",

      # Conflicto de diagnósticos (DM1 + DM2) -> Predomina DM2 si usa orales
      if_all(starts_with("ant_dm"), ~ .x == "Sí") &
        ant_dg == "No" &
        trat_oral == "Sí" ~ "DM2",

      # Clasificación por Proxy (Tratamiento + Edad)
      if_all(c(ant_dm1, ant_dm2, ant_dg), ~ .x == "No") &
        (trat_oral == "Sí" |
          (trat_oral == "No" & trat_insu == "Sí" & edad >= 70)) ~ "DM2",

      if_all(c(ant_dm1, ant_dm2, ant_dg), ~ .x == "No") &
        trat_oral == "No" &
        trat_insu == "Sí" &
        edad < 70 ~ "DM1",

      # Cualquier otro caso no contemplado
      .default = "Otro"
    )
  ) |>

  # Variables caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)))


# Pesos de discapacidad ----

DW_GBD <- DW_GBD_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>
  rename(
    comp_tipo = tipo_complicacion_cronica,
    comp_qualidiab = complicacion_cronica_qualidiab
  ) |>

  # Agrupar datos
  group_by(comp_tipo, comp_qualidiab) |>
  summarise(
    dw_promedio = sum(dw_secuela) / length(dw_secuela),
    .groups = "drop"
  ) |>

  # Filtrar NAs
  drop_na()


# Cálculo de frecuencias por sexo y grupos de edad ------------------------

## Crear dataset DM2 y DW ------------------------------------------------

## DM2 2005 y 2009
qualidiab_dm2_05_09 <- qualidiab_2005_2009 |>
  filter(tipo_dm == "DM2") |>
  
  # Seleccionar columnas relevantes
  select(
    anio,
    sexo,
    grupo_edad_10,
    comp_alguna,
    comp_iam,
    #comp_ic,
    comp_acv,
    comp_claud_mi,
    comp_retinopatia_np_c,
    comp_retinopatia_p,
    comp_ceguera,
    comp_nefropatia_c1,
    comp_neurop_perif_c,
    comp_amputacion_c,
    comp_disfuncion_erectil
  ) |>
  
  # Pasar a formato long
  pivot_longer(cols = starts_with("comp"), names_to = "comp_qualidiab") |> 
  
  # Cambiar etiquetas complicaciones
  mutate(
    comp_qualidiab = fct_relabel(
      comp_qualidiab,
      ~ c(
        "ACV",
        "Sin complicaciones",
        "Amputación",
        "Ceguera",
        "Claudicación miembros inferiores",
        "Disfunción eréctil",
        "IAM",
        "Nefropatía",
        "Neuropatía periférica",
        "Retinopatía no proliferativa",
        "Retinopatía proliferativa"
      )
    )
  ) |>
  
  # Reagrupar datos
  count(anio, sexo, grupo_edad_10, comp_qualidiab, value) |>
  
  # Calcular frecuencias
  mutate(
    comp_frec = n / sum(n),
    .by = c(anio, sexo, grupo_edad_10, comp_qualidiab)
  ) |>
  
  # Filtrar datos
  filter(
    (str_detect(comp_qualidiab, "complicaciones") & value == "No") |
      (!str_detect(comp_qualidiab, "complicaciones") & value == "Sí")
  )

## DM2 2013 y 2018
qualidiab_dm2_13_18 <- qualidiab_2013_2018 |>
  filter(tipo_dm == "DM2") |>

  # Seleccionar columnas relevantes
  select(
    anio,
    sexo,
    grupo_edad_10,
    comp_alguna,
    comp_iam_c,
    comp_ic,
    comp_acv,
    comp_claud_mi,
    comp_retinopatia_np,
    comp_retinopatia_p,
    comp_ceguera,
    comp_nefropatia_c1,
    comp_neurop_perif_c,
    comp_amputacion,
    comp_disfuncion_erectil
  ) |>

  # Pasar a formato long
  pivot_longer(cols = starts_with("comp"), names_to = "comp_qualidiab") |>

  # Cambiar etiquetas complicaciones
  mutate(
    comp_qualidiab = fct_relabel(
      comp_qualidiab,
      ~ c(
        "ACV",
        "Sin complicaciones",
        "Amputación",
        "Ceguera",
        "Claudicación miembros inferiores",
        "Disfunción eréctil",
        "IAM",
        "IC",
        "Nefropatía",
        "Neuropatía periférica",
        "Retinopatía no proliferativa",
        "Retinopatía proliferativa"
      )
    )
  ) |>

  # Reagrupar datos
  count(anio, sexo, grupo_edad_10, comp_qualidiab, value) |>

  # Calcular frecuencias
  mutate(
    comp_frec = n / sum(n),
    .by = c(anio, sexo, grupo_edad_10, comp_qualidiab)
  ) |>

  # Filtrar datos
  filter(
    (str_detect(comp_qualidiab, "complicaciones") & value == "No") |
      (!str_detect(comp_qualidiab, "complicaciones") & value == "Sí")
  )

## Unión de bases de complicaciones y pesos de discapacidad
qualidiab_dm2_dw <-
  
  # Unir bases de frecuencias de complicaciones 2005, 2009, 2013 y 2018
  bind_rows(qualidiab_dm2_05_09, qualidiab_dm2_13_18) %>% 

  # Añadir pesos de discapacidad (DW)
  left_join(DW_GBD) |>

  # Asignar DW a DM2 sin complicaciones
  mutate(dw = replace_na(dw_promedio, 0.0490114147)) |>

  # Variables caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x)),
         
         # Para eliminar filas de "disfunción eréctil" en mujeres: se asumen como errores
         filtro = case_when(
           sexo == "Mujer" & comp_qualidiab == "Disfunción eréctil" ~ "eliminar",
           .default = "ok")) %>% 
  
  # Elimino filas erróneas
  filter(filtro == "ok") %>% 

  # Reordenar columnas
  select(anio, sexo, grupo_edad_10, comp_tipo, comp_qualidiab, comp_frec, dw)
  
  
qualidiab_dm2_dw %>% 
  filter(grupo_edad_10 != "0 a 9" & grupo_edad_10 != "10 a 19" & grupo_edad_10 != "20 a 29") %>% 
  pivot_wider(names_from = anio, values_from = comp_frec) %>% 
  view()

qualidiab_dm2_dw %>% 
  mutate(anio = as.character(anio)) %>% 
  ggplot(aes(x = anio, y = comp_frec)) +
  geom_bar(stat = "identity") +
  facet_wrap(~comp_qualidiab + sexo, scales = "free_y")

qualidiab_dm2_05_09 %>% 
  bind_rows(qualidiab_dm2_13_18) %>% 
  filter(grupo_edad_10 != "0 a 9" & grupo_edad_10 != "10 a 19" & grupo_edad_10 != "20 a 29") %>% 
  count(anio, sexo, grupo_edad_10, wt = n) %>% 
  #pivot_wider(names_from = anio, values_from = n) %>% 
  ggplot(aes(x = anio, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~sexo, scales = "free_y")
  #facet_wrap(~sexo + grupo_edad_10, scales = "free_y")



# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = names(qualidiab_dm2_dw),

  descripcion = c(
    "Año de registro",
    "Sexo biológico",
    "Grupo de edad decenal",
    "Tipo de complicación crónica",
    "Nombre de la complicación crónica",
    "Frecuencia de la complicación crónica según sexo y grupo etario",
    "Peso de discapacidad asociado a la complicación"
  ),

  tipo_var = map_chr(qualidiab_dm2_dw, ~ paste(class(.x), collapse = ", ")),

  niveles = map_chr(
    qualidiab_dm2_dw,
    ~ if (is.factor(.x)) {
      paste(levels(.x), collapse = ", ")
    } else {
      "0-Inf"
    }
  )
)


# Guardar datos limpios ---------------------------------------------------
## Frecuencias complicaciones y DW
export(qualidiab_dm2_dw, file = "datos_limpios/fr_comp_DW_ge10.csv")

## Guardar diccionario de datos
export(data_dict, file = "datos_limpios/dic_fr_comp_DW.xlsx")


# Limpiar environment y desactivar paquetes ------------------------------
rm(list = ls())

pacman::p_unload("all")
