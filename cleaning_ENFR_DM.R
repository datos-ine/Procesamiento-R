### Cálculo de prevalencia de DM a partir de los datos publicados de las ENFR,
### considerando la prevalencia de DM o glucemia elevada por autorreporte y grupos
### quinquenales y decenales de edad.
### Autora: Tamara Ricardo
### Fecha modificación:
# Wed May 28 09:51:47 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  srvyr,
  epikit,
  janitor,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
## Etiquetas provincias INDEC
id_prov <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_edad <- read_csv("Bases de datos/grupos_edad.csv") |> 
  mutate_all(.funs = ~ factor(.x))


## ENFR 2005
datos05_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2005 - Base usuario.txt",
                       col_select = c(
                         prov_id = PROV, 
                         sexo = CHCH04, 
                         edad = CHCH05, 
                         dm_auto = CIDI01, 
                         # dm_gest = CIDI02,
                         ponderacion = PONDERACION)) 

## ENFR 2009
datos09_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2009 - Base usuario.txt",
                       col_select = c(id = IDENTIFI, 
                                      prov_id = PRVNC,
                                      sexo = BHCH04,
                                      edad = BHCH05,
                                      dm_auto = BIDI01,
                                      # dm_gest = BIDI02,
                                      ponderacion = PONDERACION))

## ENFR 2013
datos13_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2013 - Base usuario.txt",
                       col_select = c(id = ID,
                                      prov_id = COD_PROVINCIA,
                                      sexo = BHCH04,
                                      edad = BHCH05,
                                      dm_auto = BIDI01,
                                      # dm_gest = BIDI02,
                                      ponderacion = PONDERACION))

## ENFR 2018
datos18_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2018 - Base usuario.txt",
                       col_select = c(id,
                                      prov_id = cod_provincia,
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
    left_join(id_prov) |> 
    
    # Crear grupos de edad
    mutate(
      # Grupo edad quinquenal
      grupo_edad = age_categories(edad,
                                  lower = 20,
                                  upper = 80,
                                  by = 5,
                                  separator = " a "),
      
      # Grupo edad decenal
      grupo_edad_10 = age_categories(edad,
                                     lower = 20,
                                     upper = 80,
                                     by = 10,
                                     separator = " a "),
      .after = edad) |> 
    
    # Cambiar etiquetas sexo
    mutate(sexo = if_else(sexo == 1, "Varón", "Mujer")) |> 
    
    # Crear variable binomial para diabetes por autorreporte
    mutate(dm_auto_bin = if_else(dm_auto == 1, 1, 0)) |> 
    
    # Cambiar etiquetas DM por autorreporte
    mutate(dm_auto = factor(dm_auto,
                            labels = c("Sí", "No", "NS/NC"))) #|> 
    
    # # Cambiar etiqueta DM gestacional
    # mutate(dm_gest = case_when(dm_gest == 1 ~ "Sí",
    #                            dm_gest == 2 ~ "No",
    #                            dm_gest == 9 ~ "NS/NC",
    #                            TRUE ~ NA))
}


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
comb_5 <- id_prov |> 
  cross_join(grupos_edad |>  
               select(grupo_edad)) |> 
  
  expand_grid(sexo = c("Varón", "Mujer"),
              dm_auto = "Sí") |> 
  
  distinct()
  
## Faltantes ENFR 2005
comb_5 |> 
  anti_join(datos05)

## Faltantes ENFR 2009
comb_5 |> 
  anti_join(datos09)

## Faltantes ENFR 2013
comb_5 |> 
  anti_join(datos13)

## Faltantes ENFR 2018
comb_5 |> 
  anti_join(datos18)

## Crear objeto para combinaciones posibles según grupo edad decenal
comb_10 <- id_prov |> 
  cross_join(grupos_edad |>  
               select(grupo_edad_10)) |> 
  
  expand_grid(sexo = c("Varón", "Mujer"),
              dm_auto = "Sí") |> 
  
  distinct()

## Faltantes ENFR 2005
comb_10 |> 
  anti_join(datos05)

## Faltantes ENFR 2009
comb_10 |> 
  anti_join(datos09)

## Faltantes ENFR 2013
comb_10 |> 
  anti_join(datos13)

## Faltantes ENFR 2018
comb_10 |> 
  anti_join(datos18)


# Calcular prevalencias por grupos quinquenales ---------------------------
## ENFR 2005
prev05_g5 <- datos05 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = "cv"),
            dm_prev = survey_mean(dm_auto_bin, vartype = "cv"),
            .groups = "drop")


## ENFR 2009
prev09_g5 <- datos09 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = "cv"),
            dm_prev = survey_mean(dm_auto_bin, vartype = "cv"),
            .groups = "drop")


## ENFR 2013
prev13_g5 <- datos13 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = "cv"),
            dm_prev = survey_mean(dm_auto_bin, vartype = "cv"),
            .groups = "drop")


## ENFR 2018 (Warning)
prev18_g5 <- datos18 |> 
  # Crear objeto diseño
  as_survey_rep(weights = wf1p, 
                repweights = starts_with("wf1p"),
                type = "bootstrap"
                ) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = "cv"),
            dm_prev = survey_mean(dm_auto_bin, vartype = "cv"),
            .groups = "drop")


## Unir bases prevalencia por grupos quinquenales
prev_join_g5 <- bind_rows(prev05_g5,
                          prev09_g5,
                          prev13_g5,
                          prev18_g5,
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
prev05_g10 <- datos05 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = "cv"),
            dm_prev = survey_mean(dm_auto_bin, vartype = "cv"),
            .groups = "drop")


## ENFR 2009
prev09_g10 <- datos09 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = "cv"),
            dm_prev = survey_mean(dm_auto_bin, vartype = "cv"),
            .groups = "drop")

## ENFR 2013
prev13_g10 <- datos13 |> 
  # Generar objeto de diseño
  as_survey_design(weights = ponderacion) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = "cv"),
            dm_prev = survey_mean(dm_auto_bin, vartype = "cv"),
            .groups = "drop")

## ENFR 2018 (Warning)
prev18_g10 <- datos18 |> 
  # Crear objeto diseño
  as_survey_rep(weights = wf1p, 
                repweights = starts_with("wf1p"),
                type = "bootstrap"
  ) |> 
  
  # Estimar cantidad de personas con DM y prevalencia
  group_by(prov_id, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(dm_total = survey_total(dm_auto_bin, vartype = "cv"),
            dm_prev = survey_mean(dm_auto_bin, vartype = "cv"),
            .groups = "drop")


## Unir bases prevalencia por grupos decenales
prev_join_g10 <- bind_rows(prev05_g10,
                           prev09_g10,
                           prev13_g10,
                           prev18_g10,
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


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = c("anio_enfr", "prov_id", "prov_nombre", 
               "grupo_edad", "grupo_edad_10", "sexo",
               "dm_total", "dm_total_cv", "dm_prev", "dm_prev_cv"),
  
  descripcion = c(
    "Año de realización ENFR",
    "Identificador numérico provincia",
    "Nombre de provincia",
    "Grupo de edad quinquenal",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Total estimado de personas con diabetes mellitus por provincia, edad y sexo",
    "Coeficiente de variación del total",
    "Prevalencia de diabetes mellitus por autorreporte",
    "Coeficiente de variación de la prevalencia"),
  
  tipo_var = c(rep("factor", 6), rep("numeric", 4)),
  
  niveles = list(c(2005, 2009, 2013, 2018),
                 levels(id_prov$prov_id |>  factor()),
                 levels(id_prov$prov_nombre),
                 levels(grupos_edad$grupo_edad),
                 levels(grupos_edad$grupo_edad_10),
                 c("Varón", "Mujer"),
                 NA, NA, NA, NA) |> 
    as.character()
) |> 
  
  mutate(niveles = str_remove_all(niveles, '^c\\(|\\)$|"'))


# Guardar datos limpios ---------------------------------------------------
## Grupos etarios quinquenales
write_csv(prev_join_g5, file = "Bases de datos/clean/arg_prev_dm_g5.csv")

## Grupos etarios decenales
write_csv(prev_join_g10, file = "Bases de datos/clean/arg_prev_dm_g10.csv")

# Diccionario de datos
export(data_dict, file = "Bases de datos/clean/dic_arg_prev_dm.xlsx")


### Limpiar environment
rm(list = ls())
