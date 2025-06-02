### Limpieza y procesamiento de las proyecciones poblacionales de INDEC para los
### años 2010-2021 según provincia, sexo y grupo edad quinquenal
### Autora: Tamara Ricardo
### Fecha modificación:
# Mon Jun  2 10:19:37 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  tabulapdf, # Extraer datos de PDF
  janitor,
  tidyverse,
  readxl
)


# Cargar datos ------------------------------------------------------------
## Etiquetas provincias INDEC
id_provincias <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_etarios <- read_csv("Bases de datos/grupos_etarios.csv") |> 
  mutate_all(~ factor(.x))


# ## Proyecciones 2001-2005 (extraer tablas por provincia)
# proy_01_raw <- extract_areas(
#   "Bases de datos/Proyecciones INDEC/INDEC_proyec 2001-2015.pdf",
#   pages = c(22:24, 27:28, 25:26, 29:43, 45, 44))

## Cargar datos limpios 2005 (opcional)
proy_01 <- read_csv("Bases de datos/Proyecciones INDEC/proy_2005.csv")


## Proyecciones 2010-2040
# Ruta del archivo de Excel
indec_10 <- "Bases de datos/Proyecciones INDEC/c2_proyecciones_prov_2010_2040.xls" 
  

# Cargar/unir hojas
proy_10_raw <- excel_sheets(indec_10)[-c(1:2)] |>  # Listar hojas por provincia
  # Crear columna para la provincia
  set_names() |> 
  
  # Leer filas para 2010-2015 y unir por provincia
  map(~ read_excel(indec_10, sheet = .x, range = "A3:X28")) |> 
  list_rbind(names_to = "prov") |> 
  
  # Leer filas para 2016-2021 y unir por provincia
  bind_cols(
    excel_sheets(indec_10)[-c(1:2)] |>  # Listar hojas por provincia
      # Crear columna para la provincia
      set_names() |> 
      
      # Leer filas para 2010-2015 y unir por provincia
      map(~ read_excel(indec_10, sheet = .x, range = "A31:X56")) |> 
      list_rbind(names_to = "prov")
  )


# Limpiar datos -----------------------------------------------------------
# ## Unir y limpiar las tablas de proyecciones 2001-2005
# proy_01 <- proy_01_raw |> 
#   # Asignar identificador numérico a cada provincia
#   set_names(unique(id_provincias$prov_id)) |> 
#   
#   # Unir tablas de provincias
#   list_rbind(names_to = "prov_id") |> 
#   
#   # Estandarizar nombres de columnas
#   clean_names() |> 
#   
#   # Seleccionar columnas relevantes
#   select(prov_id,
#          grupo_edad = x1,
#          Varón_2005 = x2005,
#          Mujer_2005 = x7) |> 
#   
#   # Filtrar filas con valores ausentes
#   drop_na() |> 
#   
#   # Filtrar <20 años y totales
#   filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19")) |> 
#   
#   # Identificador de provincia a numérico
#   mutate(prov_id = parse_number(prov_id)) |> 
#   
#   # Pasar a formato long para obtener proyecciones
#   pivot_longer(cols = c(Varón_2005, Mujer_2005)) |> 
#   
#   # Crear columnas para sexo y año
#   separate(name, into = c("sexo", "anio"), sep = "_") |> 
#   
#   # Transformar escala proyección poblacional
#   mutate(value = parse_number(value, locale = locale(decimal_mark = ","))) 
#   
# ## Guardar (opcional)
# write_csv(proy_01, "Bases de datos/Proyecciones INDEC/proy_2005.csv")


## Limpiar tablas 2010-2018
proy_10 <- proy_10_raw |> 
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
  
  # Filtrar <20 años y totales
  filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19")) |> 
  
  # Limpiar id numérico de provincia
  mutate(prov_id = str_sub(prov_id, 1, 2) |> 
           parse_number()) |> 
  
  # Formato long
  pivot_longer(cols = c(Varón_2010:Mujer_2018)) |> 
  
  # Crear columnas para año y sexo
  separate(name, into = c("sexo", "anio"), sep = "_") |> 
  
  # Convertir año y proyección a numérico
  mutate(across(.cols = c(anio, value),
                .fns = ~ parse_number(.x)))


### Unir bases proyecciones por grupo quinquenal de edad
proy_join <- bind_rows(proy_01, proy_10) |> 
  
  # Añadir nombre de provincia
  left_join(id_provincias) |> 
  
  # Añadir grupos etarios
  left_join(grupos_etarios) |> 
  
  # Recalcular proyecciones
  count(anio, prov_id, prov_nombre, grupo_edad_5, grupo_edad_10, sexo,
        wt = value, name = "proy_pob")


# Estimar proyección para 2009 --------------------------------------------
## Graficar tendencia para los años disponibles
proy_join |> 
  ggplot(aes(x = prov_nombre, y = proy_pob, fill = factor(anio))) +
  
  geom_col(position = "dodge") +
  facet_wrap(~ grupo_edad_5) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

## Estimar proyección 2009
# Método lineal
proy_2009 <- proy_join  |>
  # Filtrar proyecciones 2005 y 2010
  filter(between(anio, 2005, 2010)) |>

  # Formato wide
  pivot_wider(names_from = anio,
              values_from = proy_pob,
              names_prefix = "pob_") |>

  # Interpolación lineal
  mutate(anio = 2009,
         proy_pob = pob_2005 + (4 / 5) * (pob_2010 - pob_2005)) |>

  # Filtrar datos 2009
  filter(anio == 2009) |>

  # Descartar columnas innecesarias
  select(-starts_with("pob_"))


# Añadir proyección 2009 --------------------------------------------------
proy_join_ge5 <- bind_rows(proy_join, proy_2009) |> 
  
  # Crear variable para año ENFR
  mutate(anio_enfr = if_else(anio == 2010, NA, anio)) |> 
  
  # Crear variable para población estándar Censo 2010
  left_join(proy_join |> 
              filter(anio == 2010) |> 
              select(prov_id, prov_nombre, grupo_edad_5, grupo_edad_10, sexo,
                     pob_est_2010 = proy_pob)) |> 
  
  # Eliminar NAs
  drop_na()


# Guardar datos limpios ---------------------------------------------------
write_csv(proy_join_ge5, file = "Bases de datos/clean/arg_proy_2005_2018_ge5.csv")


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = c("anio_enfr", "anio", "prov_id", "prov_nombre", 
               "grupo_edad_5", "grupo_edad_10", "sexo", "proy_pob", "pob_est_2010"),
  
  descripcion = c(
    "Año de realización ENFR",
    "Año para la proyección poblacional (para 2009 se interpoló linealmente a partir de 2005 y 2010)",
    "Identificador numérico de provincia",
    "Identificador categórico de provincia",
    "Grupo de edad quinquenal",
    "Grupo de edad decenal",
    "Sexo biológico",
    "Proyección poblacional",
    "Población estándar Censo 2010"),
  
  tipo_var = c(rep("factor", 7), rep("numeric", 2)),
  
  valores = list(c(2005, 2009, 2013, 2018),
                 c(2005, 2010, 2013, 2018),
                 levels(id_provincias$prov_id |>  factor()),
                 levels(id_provincias$prov_nombre),
                 levels(grupos_etarios$grupo_edad_5),
                 levels(grupos_etarios$grupo_edad_10),
                 c("Varón", "Mujer"),
                 "0-Inf", "0-Inf") |> 
    as.character() |> 
    str_remove_all('^c\\(|\\)$|"')
)


## Guardar diccionario de datos
export(data_dict, file = "Bases de datos/clean/dic_arg_proy_2005_2018.xlsx")


## Limpiar environment y desactivar paquetes
rm(list = ls())

pacman::p_unload("all")
