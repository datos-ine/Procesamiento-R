### Cálculo de prevalencia de DM a partir de los datos publicados de las ENFR,
### considerando la prevalencia de DM o glucemia elevada por autorreporte y grupos
### quinquenales y decenales de edad.
### Autora: Tamara Ricardo
### Fecha modificación:
# Fri May 30 14:21:34 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  srvyr,
  epikit,
  janitor,
  tidyverse
)


# Cargar datos crudos -----------------------------------------------------
## Etiquetas provincias INDEC
id_provincias <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_etarios <- read_csv("Bases de datos/grupos_etarios.csv") |> 
  mutate_all(~ factor(.x))


## ENFR 2005
datos05_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2005 - Base usuario.txt",
                       col_select = c(
                         prov_id = PROV, 
                         reg_id = REGION,
                         sexo = CHCH04, 
                         edad = CHCH05, 
                         dm_auto = CIDI01, 
                         # dm_gest = CIDI02,
                         ponderacion = PONDERACION)) 

## ENFR 2009
datos09_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2009 - Base usuario.txt",
                       col_select = c(id = IDENTIFI, 
                                      prov_id = PRVNC,
                                      reg_id = REGION,
                                      sexo = BHCH04,
                                      edad = BHCH05,
                                      dm_auto = BIDI01,
                                      # dm_gest = BIDI02,
                                      ponderacion = PONDERACION))

## ENFR 2013
datos13_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2013 - Base usuario.txt",
                       col_select = c(id = ID,
                                      prov_id = COD_PROVINCIA,
                                      reg_id = REGION,
                                      sexo = BHCH04,
                                      edad = BHCH05,
                                      dm_auto = BIDI01,
                                      # dm_gest = BIDI02,
                                      ponderacion = PONDERACION))

## ENFR 2018
datos18_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2018 - Base usuario.txt",
                       col_select = c(id,
                                      prov_id = cod_provincia,
                                      reg_id = region,
                                      sexo = bhch03,
                                      edad = bhch04,
                                      dm_auto = bidi01,
                                      # dm_gest = bidi02,
                                      wf1p)) |> 
  
  # Añadir base de réplicas
  left_join(read_delim("Bases de datos/ENFR_bases/ENFR2018_base_rep_filter.csv"))


### Explorar datos crudos
tabyl(datos05_raw$sexo)

tabyl(datos09_raw$sexo)

tabyl(datos13_raw$sexo)

tabyl(datos18_raw$sexo)

tabyl(datos05_raw$dm_auto)

tabyl(datos09_raw$dm_auto)

tabyl(datos13_raw$dm_auto)

tabyl(datos18_raw$dm_auto)


# Función para limpiar datos ----------------------------------------------
cleaning_enfr <- function(x){
  x |> 
    # Filtrar menores de 20 años
    filter(edad >= 20) |>
    
    # Añadir etiquetas provincia
    left_join(id_provincias) |> 
    
    # Añadir etiquetas región
    mutate(reg_nombre = factor(reg_id,
                                  labels = c("Gran Buenos Aires",
                                             "Pampeana",
                                             "Noroeste",
                                             "Noreste",
                                             "Cuyo",
                                             "Patagónica")), 
           .after = reg_id) |> 
    
    # Crear grupos de edad
    mutate(
      # Grupo edad quinquenal
      grupo_edad_5 = age_categories(edad,
                                  lower = 20,
                                  upper = 80,
                                  by = 5) |> 
        fct_relabel(~ levels(grupos_etarios$grupo_edad_5)),
      
      # Grupo edad decenal
      grupo_edad_10 = age_categories(edad,
                                     lower = 20,
                                     upper = 80,
                                     by = 10) |> 
        fct_relabel(~ levels(grupos_etarios$grupo_edad_10)),
      .after = edad) |> 
    
    # Cambiar etiquetas sexo
    mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) |> 
    
    # Crear variable binomial para diabetes por autorreporte
    mutate(dm_auto_bin = if_else(dm_auto == 1, 1, 0)) |> 
    
    # Cambiar etiquetas DM por autorreporte
    mutate(dm_auto = factor(dm_auto,
                            labels = c("Sí", "No", "NS/NC")))}


# Limpiar datos -----------------------------------------------------------
## ENFR 2005
datos05 <- datos05_raw |> 
  # Aplicar función de limpieza
  cleaning_enfr()

## ENFR 2009
datos09 <- datos09_raw |> 
  # Aplicar función de limpieza
  cleaning_enfr()

## ENFR 2013
datos13 <- datos13_raw |> 
  # Aplicar función de limpieza
  cleaning_enfr()

## ENFR 2018
datos18 <- datos18_raw |> 
  # Aplicar función de limpieza
  cleaning_enfr()


# Explorar combinaciones faltantes ----------------------------------------
## Crear objeto para combinaciones posibles según grupo edad quinquenal
comb_ge5 <- id_provincias |> 
  cross_join(grupos_etarios |>  
               select(grupo_edad_5)) |> 
  
  expand_grid(sexo = c("Varón", "Mujer"),
              dm_auto = "Sí") |> 
  
  distinct()
  
## Faltantes ENFR 2005
comb_ge5 |> 
  anti_join(datos05)

## Faltantes ENFR 2009
comb_ge5 |> 
  anti_join(datos09)

## Faltantes ENFR 2013
comb_ge5 |> 
  anti_join(datos13)

## Faltantes ENFR 2018
comb_ge5 |> 
  anti_join(datos18)

## Crear objeto para combinaciones posibles según grupo edad decenal
comb_ge10 <- id_provincias |> 
  cross_join(grupos_etarios |>  
               select(grupo_edad_10)) |> 
  
  expand_grid(sexo = c("Varón", "Mujer"),
              dm_auto = "Sí") |> 
  
  distinct()

## Faltantes ENFR 2005
comb_ge10 |> 
  anti_join(datos05)

## Faltantes ENFR 2009
comb_ge10 |> 
  anti_join(datos09)

## Faltantes ENFR 2013
comb_ge10 |> 
  anti_join(datos13)

## Faltantes ENFR 2018
comb_ge10 |> 
  anti_join(datos18)


# Calcular prevalencias por grupos quinquenales ---------------------------
## ENFR 2005
prev05_ge5 <- datos05 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, reg_id, reg_nombre, grupo_edad_5, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
            dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
            .groups = "drop")


## ENFR 2009
prev09_ge5 <- datos09 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, reg_id, reg_nombre, grupo_edad_5, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
            dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
            .groups = "drop")


## ENFR 2013
prev13_ge5 <- datos13 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, reg_id, reg_nombre, grupo_edad_5, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
            dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
            .groups = "drop")


## ENFR 2018 (Warning)
prev18_ge5 <- datos18 |> 
  # Crear objeto diseño
  as_survey_rep(weights = wf1p, 
                repweights = starts_with("wf1p"),
                type = "bootstrap"
                ) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, reg_id, reg_nombre, grupo_edad_5, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
            dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
            .groups = "drop")


## Unir bases prevalencia por grupos quinquenales
prev_join_ge5 <- bind_rows(prev05_ge5,
                          prev09_ge5,
                          prev13_ge5,
                          prev18_ge5,
                          .id = "anio_enfr") |> 
  
  # Reemplaza etiquetas año ENFR
  mutate(anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
         ) |> 
  
  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, 2))) |> 
  
  # Variables categóricas a factor
  mutate(across(.cols = c(anio_enfr:sexo),
                .fns = ~ factor(.x)))


# Calcular prevalencias por grupos decenales ------------------------------
## ENFR 2005
prev05_ge10 <- datos05 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, reg_id, reg_nombre, grupo_edad_10, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
            dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
            .groups = "drop")


## ENFR 2009
prev09_ge10 <- datos09 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, reg_id, reg_nombre, grupo_edad_10, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
            dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
            .groups = "drop")

## ENFR 2013
prev13_ge10 <- datos13 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, reg_id, reg_nombre, grupo_edad_10, sexo) |>  
  summarise(dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
            dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
            .groups = "drop")

## ENFR 2018 (Warning)
prev18_ge10 <- datos18 |> 
  # Crear objeto diseño
  as_survey_rep(weights = wf1p, 
                repweights = starts_with("wf1p"),
                type = "bootstrap"
  ) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, reg_id, reg_nombre, grupo_edad_10, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = c("se", "cv")),
            dm_prev = survey_mean(dm_auto_bin, vartype = c("se", "cv")),
            .groups = "drop")


## Unir bases prevalencia por grupos decenales
prev_join_ge10 <- bind_rows(prev05_ge10,
                           prev09_ge10,
                           prev13_ge10,
                           prev18_ge10,
                           .id = "anio_enfr") |> 
  
  # Reemplaza etiquetas año ENFR
  mutate(anio_enfr = fct_relabel(anio_enfr, ~ c("2005", "2009", "2013", "2018"))
         ) |> 
  
  # Redondear variables numéricas
  mutate(across(.cols = where(is.numeric),
                .fns = ~ round(.x, 2))) |> 
  
  # Variables categóricas a factor
  mutate(across(.cols = c(anio_enfr:sexo),
                .fns = ~ factor(.x)))

# Guardar datos limpios ---------------------------------------------------
## Grupos etarios quinquenales
write_csv(prev_join_ge5, file = "Bases de datos/clean/arg_prev_dm_ge5.csv")

## Grupos etarios decenales
write_csv(prev_join_ge10, file = "Bases de datos/clean/arg_prev_dm_ge10.csv")


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = c("anio_enfr", "prov_id", "prov_nombre", "reg_id", "reg_nombre",
               "grupo_edad", "grupo_edad_10", "sexo",
               "dm_total", "dm_total_se", "dm_total_cv",
               "dm_prev", "dm_prev_se", "dm_prev_cv"),
  
  descripcion = c(
    "Año de realización ENFR",
    "Identificador numérico de provincia",
    "Identificador categórico de provincia",
    "Identificador numérico de región estadística",
    "Identificador categórico de región estadística",
    "Grupo de edad quinquenal",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Total estimado de personas con diabetes mellitus por provincia, edad y sexo",
    "Error estándar del total de personas con DM",
    "Coeficiente de variación del total de personas con DM",
    "Prevalencia de diabetes mellitus por autorreporte",
    "Error estándar del total de la prevalencia de personas con DM",
    "Coeficiente de variación de la prevalencia de personas con DM"),
  
  tipo_var = c(rep("factor", 8), rep("numeric", 6)),
  
  niveles = list(c(2005, 2009, 2013, 2018),
                 levels(id_provincias$prov_id |>  factor()),
                 levels(id_provincias$prov_nombre),
                 levels(prev_join_ge5$reg_id),
                 levels(prev_join_ge5$reg_nombre),
                 levels(grupos_etarios$grupo_edad_5),
                 levels(grupos_etarios$grupo_edad_10),
                 c("Varón", "Mujer"),
                 NA, NA, NA, NA, NA, NA) |> 
    as.character()
) |> 
  
  mutate(niveles = str_remove_all(niveles, '^c\\(|\\)$|"'))


## Guardar el diccionario de datos
export(data_dict, file = "Bases de datos/clean/dic_arg_prev_dm.xlsx")


## Limpiar environment y desactivar paquetes
rm(list = ls())

pacman::p_unload("all")

