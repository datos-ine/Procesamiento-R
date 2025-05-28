### Limpieza y procesamiento de las proyecciones poblacionales de INDEC para los
### años 2010-2021 según provincia, sexo y grupo edad quinquenal
### Autora: Tamara Ricardo
### Fecha modificación:
# Wed May 28 10:29:11 2025 ------------------------------


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
id_prov <- read_csv("Bases de datos/cod_pcias_arg.csv") |> 
  mutate(prov_nombre = factor(prov_nombre))

## Etiquetas grupos de edad
grupos_edad <- read_csv("Bases de datos/grupos_edad.csv") |> 
  mutate_all(.funs = ~ factor(.x))


## Proyecciones 2001-2005 (extraer tablas por provincia)
proy_01_raw <- extract_areas(
  "Bases de datos/Proyecciones INDEC/INDEC_proyec 2001-2015.pdf",
  pages = 22:45,
  # # Descomentar para guardar cada tabla como csv
  # output = "csv",
  # outdir = "Bases de datos/Proyecciones INDEC/"
  )

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
## Unir y limpiar las tablas de proyecciones 2001-2015
proy_01 <- proy_01_raw |> 
  # Asignar identificador numérico a cada provincia
  set_names(c(2, 6, 10, 22, 26, 14, 18, 30, 34, 38, 42, 46, 50,
              54, 58, 62, 66, 70, 74, 78, 82, 86, 94, 90)) |> 
  
  # Unir tablas
  list_rbind(names_to = "prov_id") |> 
  
  # Estandarizar nombres de columnas
  clean_names() |> 
  
  # Seleccionar columnas relevantes
  select(prov_id,
         grupo_edad = x1,
         # V_2001 = x2001,
         # M_2001 = x4,
         V_2005 = x2005,
         M_2005 = x7) |> 
  
  # Filtrar filas con valores ausentes
  drop_na() |> 
  
  # Filtrar <20 años y totales
  filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19")) |> 
  
  # Identificador de provincia a numérico
  mutate(prov_id= as.numeric(prov_id)) |> 
  
  # Pasar a formato long para obtener proyecciones
  pivot_longer(cols = c(V_2005,M_2005),
               values_to = "proy_pob") |> 
  
  # Crear columnas para sexo y año
  separate(name, into = c("sexo", "anio")) |> 
  
  # Transformar escala proyección poblacional
  mutate(proy_pob = parse_number(proy_pob, 
                                 locale = locale(decimal_mark = ","))) 
  
  
## Limpiar tablas 2010-2040
proy_10 <- proy_10_raw |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  
  # Seleccionar columnas relevantes
  select(prov_id = prov_1,
         grupo_edad = edad_2,
         V_2010 = x4,
         M_2010 = x5,
         V_2013 = x16,
         M_2013 = x17,
         V_2018 = x37,
         M_2018 = x38)  |> 
  
  # Filtrar filas con valores ausentes
  drop_na() |> 
  
  # Filtrar <20 años y totales
  filter(!grupo_edad %in% c("Total", "0-4", "5-9", "10-14", "15-19")) |> 
  
  # Limpiar id numérico de provincia
  mutate(prov_id = str_sub(prov_id, 1, 2) |> 
           as.numeric()) |> 
  
  # Formato long
  pivot_longer(cols = c(V_2010:M_2018), 
               values_to = "proy_pob") |> 
  
  # Crear columnas para año y sexo
  separate(name, into = c("sexo", "anio")) |> 
  
  # Agrupar mayores de 80 años
  mutate(grupo_edad = fct_collapse(grupo_edad,
                                 "80+" = c("80-84", "85-89","90-94",
                                           "95-99", "100 y más"))) |>
  
  # Convertir proyección a numérico
  mutate(proy_pob = parse_number(proy_pob)) |> 
  
  # Calcular población para mayores de 80 años
  group_by(anio, prov_id, sexo, grupo_edad) |> 
  summarise(proy_pob = sum(proy_pob, na.rm = TRUE),
            .groups = "drop") 
  

### Unir bases proyecciones
proy_join <- bind_rows(proy_01, proy_10) |> 
  
  # Añadir nombre de provincia
  left_join(id_prov) |> 
  
  # Cambiar etiquetas sexo
  mutate(sexo = if_else(sexo == "V", "Varón", "Mujer")) |> 
  
  # Cambiar etiquetas grupo edad
  mutate(grupo_edad = fct_recode(grupo_edad, 
                                 "80+" = "80 y más") |> 
           fct_relabel(~ levels(grupos_edad$grupo_edad))
         ) |> 
  
  # Añadir grupo etario decenal
  left_join(grupos_edad) |> 
  
  # Añadir año ENFR
  mutate(anio_enfr = if_else(anio == "2010", "2009", anio)) |> 
  
  # Ordenar columnas
  select(starts_with("anio"), starts_with("prov"), starts_with("grupo"),
         sexo, proy_pob)


# Diccionario de datos ----------------------------------------------------
data_dict <- tibble(
  variable = c("anio", "anio_enfr", "prov_id", "prov_nombre", 
               "grupo_edad", "grupo_edad_10", "sexo", "proy_pob"),
  
  descripcion = c(
    "Año",
    "Año de realización ENFR",
    "Identificador numérico provincia",
    "Nombre de provincia",
    "Grupo de edad quinquenal",
    "Grupo edad decenal",
    "Sexo biológico",
    "Proyección poblacional"),
  
  tipo_var = c(rep("factor", 7), "numeric"),
  
  niveles = list(c(2005, 2010, 2013, 2018),
                 c(2005, 2009, 2013, 2018),
                 levels(id_prov$prov_id |>  factor()),
                 levels(id_prov$prov_nombre),
                 levels(grupos_edad$grupo_edad),
                 levels(grupos_edad$grupo_edad_10),
                 c("Varón", "Mujer"),
                 NA) |> 
    as.character()
) |> 
  
  mutate(niveles = str_remove_all(niveles, '^c\\(|\\)$|"'))

# Guardar datos limpios ---------------------------------------------------
## Proyecciones poblacionales
write_csv(proy_join, file = "Bases de datos/clean/arg_proy_2005_2018.csv")

## Diccionario de datos
export(data_dict, file = "Bases de datos/clean/dic_arg_proy_205_2018.xlsx")
