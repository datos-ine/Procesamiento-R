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
# Última modificación: 26-01-2026 09:07

# Carga de paquetes -------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  epikit,
  flextable,
  tidyverse
)

# Carga de datos ----------------------------------------------------------
## Qualidiab 2014 ----
qualidiab_2014_raw <- import(
  "bases_datos/fichas_pacientes_QUALIDIAB_solo_ARG_2014.xlsx"
)


## Pesos de discapacidad para complicaciones de Qualidiab ----
# (corregida para "Retinopatía proliferativa", "Disfunción eréctil" y "Nefropatía"
#  (combinada con diálisis/tx) según criterios consensuados)
DW_GBD_raw <- import("bases_datos/DW_GBD.xlsx")


# Limpiar datos ----------------------------------------------------------
## Qualidiab 2014 ----
qualidiab_2014 <- qualidiab_2014_raw |>
  # Renombrar columnas
  rename(
    edad_dx = edad_al_diagnostico_de_la_diabetes,
    fecha_registro = registro_fecha,
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

  # Calcular edad al momento del registro
  mutate(
    edad = interval(fecha_de_nacimiento, fecha_registro) |>
      time_length(unit = "year") |>
      floor()
  ) |>

  # Crear variable para grupo etario decenal
  mutate(
    grupo_edad10 = age_categories(
      edad,
      lower = 0,
      upper = 80,
      by = 10,
      separator = " a "
    )
  ) |>

  # Calcular tiempo desde el diagnóstico
  mutate(tiempo_dx = if_else(edad - edad_dx > 0, edad - edad_dx, NA)) |>

  # Corregir IAM con Stent y CRM como proxy (confirmar CRM)
  mutate(
    comp_iam_c = if_else(
      comp_iam != "Sí" & (comp_stent == "Sí" | comp_crm == "Sí"),
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
    ),
    # Saco registro de nefropatía para los que tienen TX para no
    # contar doble (opción si uso categorías por separado)
    comp_nefropatia_c2 = if_else(
      comp_nefropatia == "Sí" & comp_dialisis_transplante == "Sí",
      "No",
      comp_nefropatia
    )
  ) |>

  # Crear variable para presencia/ausencia de complicaciones
  mutate(
    comp_alguna = if_else(
      # Si cualquiera de las columnas tiene un "Sí"
      if_all(
        starts_with("comp_"),
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
        .cols = contains(c("ceg", "nefropatia_c1", "disf", "amp", "ret")),
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
          "hipo",
          "iam_c",
          "acv",
          "claud",
          "rev",
          "hvi",
          "ait",
          "crm",
          "stent",
          "comp_ic"
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
          contains(c("sul", "met", "gli", "idpp4", "arglp1", "dm_")),
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
          contains(c("nph", "cris", "ana", "num")),
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

# Explorar datos ---------------------------------------------------------
## Explorar por tipo DM ----
qualidiab_2014 |>
  count(tipo_dm, ant_dm1, ant_dm2, ant_dg) |>
  flextable() |>
  merge_v(j = 1)

## En la nueva categorización de DM2 hay:
# 1261 registros que en la base tenían DM2,
# 16 registros nuevos por tratamiento y
# 1 registro con doble antecedente que queda como DM2 por tratamiento.
## Los que en la recategorización son "otros" corresponden a personas con pre-diabetes.
# Cálculo de frecuencias por sexo y grupos de edad ------------------------

# Crear dataset DM2 y DW -------------------------------------------------
qualidiab_dm2_dw <- qualidiab_2014 |>
  filter(tipo_dm == "DM2") |>

  # Seleccionar columnas relevantes
  select(
    sexo,
    grupo_edad10,
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
  count(sexo, grupo_edad10, comp_qualidiab, value) |>

  # Calcular frecuencias
  mutate(comp_frec = n / sum(n), .by = c(sexo, grupo_edad10, comp_qualidiab)) |>

  # Filtrar datos
  filter(
    (str_detect(comp_qualidiab, "alguna") & value == "No") |
      (!str_detect(comp_qualidiab, "alguna") & value == "Sí")
  ) |>

  # Añadir pesos de discapacidad (DW)
  left_join(DW_GBD) |>

  # Asignar DW a DM2 sin complicaciones
  mutate(dw = replace_na(dw_promedio, 0.0490114147)) |>

  # Variables caracter a factor
  mutate(across(.cols = where(is.character), .fns = ~ factor(.x))) |>

  # Reordenar columnas
  select(sexo, grupo_edad10, comp_tipo, comp_qualidiab, comp_frec, dw)


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = names(qualidiab_dm2_dw),

  descripcion = c(
    # "Año de realización ENFR",
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
