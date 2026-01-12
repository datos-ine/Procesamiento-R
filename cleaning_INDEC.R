### Limpieza y procesamiento de las proyecciones poblacionales de INDEC para los
### años 2010-2021 según provincia, sexo y grupo edad quinquenal.
### Se suma el cálculo por grupo de edad decenal y por región.
### Autoras: Tamara Ricardo y Micaela Gauto
### Fecha modificación:
# 2025-12-30

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  tabulapdf, # Extraer datos de PDF
  janitor,
  tidyverse,
  readxl,
  geoAr
)


# Cargar datos ------------------------------------------------------------
## Etiquetas provincias INDEC
# id_provincias <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
#   mutate(prov_nombre = factor(prov_nombre))

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
  ),
  
  # Pasar a formato num
  prov_id = as.double(prov_id))


## Etiquetas grupos de edad
grupos_etarios <- read_csv("Bases de datos/grupos_etarios.csv") |> 
  mutate_all(~ factor(.x)) |> 
  filter(!str_detect(grupo_edad_5, "20-24|25-29")) # Edición de filtro de edad


## Cargar datos 2001 (para cálculo de proyección 2009)
proy_01_raw <- read_excel("Bases de datos/Proyecciones INDEC/proyec_2001.xls")


## Cargar datos 2005
proy_05_raw <- read_csv("Bases de datos/Proyecciones INDEC/proy_2005.csv")


## Proyecciones 2010-2018
# Ruta del archivo de Excel
indec_10 <- "Bases de datos/Proyecciones INDEC/c2_proyecciones_prov_2010_2040.xls" 

# Cargar/unir hojas
proy_10_18_raw <- excel_sheets(indec_10)[-c(1:2)] |>  # Listar hojas por provincia
  # Crear columna para la provincia
  set_names() |> 
  
  # Leer filas para 2010-2015 y unir por provincia
  map(~ read_excel(indec_10, sheet = .x, range = "A3:X28")) |> 
  list_rbind(names_to = "prov") |> 
  
  # Unir filas para 2016-2021
  bind_cols(
    excel_sheets(indec_10)[-c(1:2)] |>  # Listar hojas por provincia
      # Crear columna para la provincia
      set_names() |> 
      
      # Leer filas para 2016-2021 y unir por provincia
      map(~ read_excel(indec_10, sheet = .x, range = "A31:X56")) |> 
      list_rbind(names_to = "prov")
  )

# Limpiar datos -----------------------------------------------------------
## Limpiar datos del 2001 y homogeneizar formato
proy_01 <- proy_01_raw %>% 
  
  # Estandarizar nombres de columnas
  clean_names() |>
  
  # Filtrar totales provincia
  filter(jurisdiccion != "Total") |> 

  # Asignar identificador numérico a cada provincia
  left_join(id_provincias, 
            by = join_by("jurisdiccion" == "prov_nombre")) %>% 
  
  # Añadir etiquetas región (según ENFR 2018, se combina Pampeana y GBA)
  mutate(region = case_when(
    jurisdiccion == "CABA" | jurisdiccion == "Buenos Aires" | jurisdiccion == "Santa Fe" |
      jurisdiccion == "Córdoba" | jurisdiccion == "Entre Ríos" | jurisdiccion == "La Pampa" ~ "Centro",
    jurisdiccion == "Jujuy" | jurisdiccion == "Salta" | jurisdiccion == "Tucumán" | jurisdiccion == "Catamarca" |
      jurisdiccion == "La Rioja" | jurisdiccion == "Santiago del Estero" ~ "Noroeste",
    jurisdiccion == "Chaco" | jurisdiccion == "Formosa" | jurisdiccion == "Misiones" |
      jurisdiccion == "Corrientes" ~ "Noreste",
    jurisdiccion == "San Luis" | jurisdiccion == "San Juan" | jurisdiccion == "Mendoza" ~ "Cuyo",
    jurisdiccion == "Neuquén" | jurisdiccion == "Río Negro" | jurisdiccion == "Chubut" |
      jurisdiccion == "Santa Cruz" | jurisdiccion == "Tierra del Fuego" ~ "Patagonia",
    .default = "Otro")) %>% 
  
  # Seleccionar columnas relevantes
  select(region,
         prov_id,
         grupo_edad = edad,
         Varón_2001 = varones,
         Mujer_2001 = mujeres) |>
  
  # # Filtrar <20 años y totales
  # filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19")) |>
  
  # # Filtrar <35 años y totales
  # filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19",
  #                           "20-24", "25-29", "30-34")) |>

  # Filtrar <30 años y totales
  filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19",
                            "20-24", "25-29")) |>
  
  # Pasar a formato long para obtener proyecciones
  pivot_longer(cols = c(Varón_2001, Mujer_2001)) |>
  
  # Crear columnas para sexo y año
  separate(name, into = c("sexo", "anio"), sep = "_") %>% 
  
  # Transformar anio a formato número
  mutate(anio = parse_number(anio))
        

## Guardar (opcional)
#write_csv(proy_01, "Bases de datos/Proyecciones INDEC/proy_2001.csv")


## Limpiar la tabla de proyecciones 2005
proy_05 <- proy_05_raw |>
  
  # Filtrar datos 2005
  filter(anio == 2005) |> 
  
#   # Filtrar edad <35 años
#   filter(between(grupo_edad, "35-39", "80 y más"))
  
  # Filtrar edad <30 años
  filter(between(grupo_edad, "30-34", "80 y más")) %>% 
  
  # Asignar identificador categórico a cada provincia
  left_join(id_provincias, 
            by = join_by("prov_id" == "prov_id")) %>%
  
  # Añadir etiquetas región (según ENFR 2018, se combina Pampeana y GBA)
  mutate(region = case_when(
    prov_nombre == "CABA" | prov_nombre == "Buenos Aires" | prov_nombre == "Santa Fe" |
      prov_nombre == "Córdoba" | prov_nombre == "Entre Ríos" | prov_nombre == "La Pampa" ~ "Centro",
    prov_nombre == "Jujuy" | prov_nombre == "Salta" | prov_nombre == "Tucumán" | prov_nombre == "Catamarca" |
      prov_nombre == "La Rioja" | prov_nombre == "Santiago del Estero" ~ "Noroeste",
    prov_nombre == "Chaco" | prov_nombre == "Formosa" | prov_nombre == "Misiones" |
      prov_nombre == "Corrientes" ~ "Noreste",
    prov_nombre == "San Luis" | prov_nombre == "San Juan" | prov_nombre == "Mendoza" ~ "Cuyo",
    prov_nombre == "Neuquén" | prov_nombre == "Río Negro" | prov_nombre == "Chubut" |
      prov_nombre == "Santa Cruz" | prov_nombre == "Tierra del Fuego" ~ "Patagonia",
    .default = "Otro")) %>% 
  
  # Seleccionar columnas relevantes
  select(region, prov_id, grupo_edad, sexo, anio, value)


## Limpiar tablas 2010-2018
proy_10_18 <- proy_10_18_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  
  # Seleccionar columnas relevantes
  select(prov_id = prov_1,
         grupo_edad = edad_2,
         Varón_2010 = x4,
         Mujer_2010 = x5,
         Varón_2013 = x16,
         Mujer_2013 = x17,
         Varón_2018 = x37,
         Mujer_2018 = x38)  |> 
  
  # Filtrar filas con valores ausentes
  drop_na() |> 
  
  # # Filtrar <20 años y totales
  # filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19")) |> 
  
  # # Filtrar <35 años y totales
  # filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19",
  #                           "20-24", "25-29", "30-34")) |> 

  # Filtrar <30 años y totales
  filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19",
                            "20-24", "25-29")) |> 
  
  # Limpiar id numérico de provincia
  mutate(prov_id = str_sub(prov_id, 1, 2) |> 
           parse_number()) |> 
  
  # Asignar identificador categórico a cada provincia
  left_join(id_provincias, 
            by = join_by("prov_id" == "prov_id")) %>%
  
  # Añadir etiquetas región (según ENFR 2018, se combina Pampeana y GBA)
  mutate(region = case_when(
    prov_nombre == "CABA" | prov_nombre == "Buenos Aires" | prov_nombre == "Santa Fe" |
      prov_nombre == "Córdoba" | prov_nombre == "Entre Ríos" | prov_nombre == "La Pampa" ~ "Centro",
    prov_nombre == "Jujuy" | prov_nombre == "Salta" | prov_nombre == "Tucumán" | prov_nombre == "Catamarca" |
      prov_nombre == "La Rioja" | prov_nombre == "Santiago del Estero" ~ "Noroeste",
    prov_nombre == "Chaco" | prov_nombre == "Formosa" | prov_nombre == "Misiones" |
      prov_nombre == "Corrientes" ~ "Noreste",
    prov_nombre == "San Luis" | prov_nombre == "San Juan" | prov_nombre == "Mendoza" ~ "Cuyo",
    prov_nombre == "Neuquén" | prov_nombre == "Río Negro" | prov_nombre == "Chubut" |
      prov_nombre == "Santa Cruz" | prov_nombre == "Tierra del Fuego" ~ "Patagonia",
    .default = "Otro")) %>% 

  # Formato long
  pivot_longer(cols = c(Varón_2010:Mujer_2018)) |> 
  
  # Crear columnas para año y sexo
  separate(name, into = c("sexo", "anio"), sep = "_") |> 
  
  # Convertir año y proyección a numérico
  mutate(across(.cols = c(anio, value),
                .fns = ~ parse_number(.x))) %>% 
  
  # Seleccionar columnas relevantes
  select(region, prov_id, grupo_edad, sexo, anio, value)


## Crear población estándar 2010
pob_2010 <- proy_10_18 |>
  
  # Filtrar datos 2010
  filter(anio == 2010) |> 
  
  # Descartar columnas innecesarias
  select(-anio) |> 
  
  # Añadir grupos etarios
  left_join(grupos_etarios) |> 
  
  # Recalcular proyección
  count(#prov_id, # saco prov_id porque la población estándar es nacional
        grupo_edad_10, # corrección a grupos decenales
        sexo, 
        wt = value, name = "pob_est_2010")


# Estimar proyección 2009 -------------------------------------------------

# Método lineal
proy_09 <- proy_10_18  |>
  # Filtrar proyecciones 2010
  filter(anio == 2010) |>
  
  # Unión con población 2001
  bind_rows(proy_01) %>% 
  
  # Formato wide
  pivot_wider(names_from = anio,
              values_from = value,
              names_prefix = "pob_") |>
  
  # Interpolación lineal
  mutate(anio = 2009,
         tasa_anual = log(pob_2010 / pob_2001) / 9,
         proy_pob = round((pob_2001 * tasa_anual * 8) + pob_2001)) |>
  
  # Descartar columnas innecesarias
  select(region, prov_id, grupo_edad, sexo, anio, 
         value = proy_pob)


# Unión de proyecciones 2005, 2009, 2013 y 2018 ---------------------------
## Por grupos de edad decenales
proy_join_10 <- bind_rows(proy_05, 
                       proy_09, 
                       proy_10_18 |> filter(anio != 2010)) |> 
  
  # Añadir nombre de provincia
  left_join(id_provincias) |> 
  
  # Añadir grupos etarios
  left_join(grupos_etarios |>  
              select(-grupo_edad_5)) |>  # corrección a grupos decenales
  
  # Recalcular proyecciones
  count(anio, prov_id, prov_nombre, grupo_edad_10, sexo,
        wt = value, name = "proy_pob") |> 
  
  # Añadir población estándar 2010
  left_join(pob_2010)

## Por grupos de edad decenales y región
proy_join_10_reg <- bind_rows(proy_05, 
                          proy_09, 
                          proy_10_18 |> filter(anio != 2010)) |> 
  
  # Añadir grupos etarios
  left_join(grupos_etarios |>  
              select(-grupo_edad_5)) |>  # corrección a grupos decenales
  
  # Recalcular proyecciones
  count(anio, region, grupo_edad_10, sexo,
        wt = value, name = "proy_pob") |> 
  
  # Añadir población estándar 2010
  left_join(pob_2010)


# Guardar datos limpios ---------------------------------------------------
write_csv(proy_join_10, file = "Bases de datos/clean/arg_proy_2005_2018_ge10.csv")

write_csv(proy_join_10_reg, file = "Bases de datos/clean/arg_proy_2005_2018_ge10_reg.csv")


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = c("anio_enfr", 
               "anio",
               "prov_id", 
               "prov_nombre", 
               # "grupo_edad_5", 
               "grupo_edad_10",
               "sexo", 
               "proy_pob", 
               "pob_est_2010"),
  
  descripcion = c(
    "Año de realización ENFR",
    "Año para la proyección poblacional (para 2009 se interpoló linealmente a partir de 2005 y 2010)",
    "Identificador numérico de provincia",
    "Identificador categórico de provincia",
    # "Grupo de edad quinquenal",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Proyección poblacional",
    "Población estándar Censo 2010"),
  
  tipo_var = c(rep("factor", 6), rep("numeric", 2)),
  
  valores = list(c(2005, 2009, 2013, 2018),
                 c(2005, 2010, 2013, 2018),
                 levels(id_provincias$prov_id |>  factor()),
                 levels(id_provincias$prov_nombre),
                 # levels(grupos_etarios$grupo_edad_5),
                 levels(grupos_etarios$grupo_edad_10),
                 c("Varón", "Mujer"),
                 "0-Inf", "0-Inf") |> 
    as.character() |> 
    str_remove_all('^c\\(|\\)$|"')
)

data_dict_reg <- tibble(
  variable = c("anio_enfr", 
               "anio",
               "region", 
               "grupo_edad_10",
               "sexo", 
               "proy_pob", 
               "pob_est_2010"),
  
  descripcion = c(
    "Año de realización ENFR",
    "Año para la proyección poblacional (para 2009 se interpoló linealmente a partir de 2005 y 2010)",
    "Identificador categórico de región",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Proyección poblacional",
    "Población estándar Censo 2010"),
  
  tipo_var = c(rep("factor", 5), rep("numeric", 2)),
  
  valores = list(c(2005, 2009, 2013, 2018),
                 c(2005, 2010, 2013, 2018),
                 levels(proy_join_10_reg$region |>  factor()),
                 levels(grupos_etarios$grupo_edad_10),
                 c("Varón", "Mujer"),
                 "0-Inf", "0-Inf") |> 
    as.character() |> 
    str_remove_all('^c\\(|\\)$|"')
)


## Guardar diccionario de datos
export(data_dict, file = "Bases de datos/clean/dic_arg_proy_2005_2018.xlsx")

export(data_dict_reg, file = "Bases de datos/clean/dic_arg_proy_2005_2018_reg.xlsx")


## Limpiar environment y desactivar paquetes
rm(list = ls())

pacman::p_unload("all")
