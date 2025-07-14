### Limpieza y procesamiento de las proyecciones poblacionales de INDEC para los
### años 2010-2021 según provincia, sexo y grupo edad quinquenal
### Autoras: Tamara Ricardo y Micaela Gauto
### Fecha modificación:
# Tue Jul  8 10:17 2025 ------------------------------


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
  mutate_all(~ factor(.x)) |> 
  filter(!str_detect(grupo_edad_5, "20-24|25-29|30-34"))


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
  
  # Seleccionar columnas relevantes
  select(prov_id,
         grupo_edad = edad,
         Varón_2001 = varones,
         Mujer_2001 = mujeres) |>
  
  # # Filtrar <20 años y totales
  # filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19")) |>
  
  # Filtrar <35 años y totales
  filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19",
                            "20-24", "25-29", "30-34")) |>
  
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
  # Filtrar datos 2001
  filter(anio == 2005) |> 
  
  # Filtrar edad <35 años
  filter(between(grupo_edad, "35-39", "80 y más"))


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
  
  # Filtrar <35 años y totales
  filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19",
                            "20-24", "25-29", "30-34")) |> 
  
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


## Crear población estándar 2010
pob_2010 <- proy_10_18 |>
  # Filtrar datos 2010
  filter(anio == 2010) |> 
  
  # Descartar columnas innecesarias
  select(-anio) |> 
  
  # Añadir grupos etarios
  left_join(grupos_etarios) |> 
  
  # Recalcular proyección
  count(prov_id, grupo_edad_5, sexo, 
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
  select(prov_id, grupo_edad, sexo, anio, 
         value = proy_pob)


# Unión de proyecciones 2005, 2009, 2013 y 2018 ---------------------------
proy_join <- bind_rows(proy_05, 
                       proy_09, 
                       proy_10_18 |> filter(anio != 2010)) |> 
  
  # Añadir nombre de provincia
  left_join(id_provincias) |> 
  
  # Añadir grupos etarios
  left_join(grupos_etarios |>  select(-grupo_edad_10)) |> 
  
  # Recalcular proyecciones
  count(anio, prov_id, prov_nombre, grupo_edad_5, sexo,
        wt = value, name = "proy_pob") |> 
  
  # Añadir población estándar 2010
  left_join(pob_2010)


# Guardar datos limpios ---------------------------------------------------
write_csv(proy_join, file = "Bases de datos/clean/arg_proy_2005_2018_ge5.csv")


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = c("anio_enfr", 
               "anio",
               "prov_id", 
               "prov_nombre", 
               "grupo_edad_5", 
               "grupo_edad_10",
               "sexo", 
               "proy_pob", 
               "pob_est_2010"),
  
  descripcion = c(
    "Año de realización ENFR",
    "Año para la proyección poblacional (para 2009 se interpoló linealmente a partir de 2005 y 2010)",
    "Identificador numérico de provincia",
    "Identificador categórico de provincia",
    "Grupo de edad quinquenal",
    # "Grupo de edad decenal",
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
