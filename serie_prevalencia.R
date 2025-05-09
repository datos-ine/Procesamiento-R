### Cálculo de prevalencia de DM a partir de los datos publicados de las ENFR,
### considerando la prevalencia de DM o glucemia elevada por autorreporte.
### Autora: Micaela Gauto
### Colaboradora: Tamara Ricardo
### Fecha modificación:
# Thu May  8 13:35:20 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
library(janitor)
library(epikit)
library(srvyr)
library(tidyverse)

# Cargar datos crudos -----------------------------------------------------
datos_raw <- read_delim("Bases de datos/ENFR_bases/ENFR 2005 - Base usuario.txt") |> 
  select(prov_res = PROV,
         sexo = CHCH04,
         edad = CHCH05,
         dm_auto = CIDI01,
         ponderacion = PONDERACION) |> 
  mutate(anio = 2005) |> 
  
  # Añade datos 2009
  bind_rows(
    read_delim("Bases de datos/ENFR_bases/ENFR 2009 - Base usuario.txt") |> 
      select(prov_res = PRVNC,
             sexo = BHCH04,
             edad = BHCH05,
             dm_auto = BIDI01,
             ponderacion = PONDERACION) |> 
      mutate(anio = 2009)
  ) |> 
  
# Añade datos 2013
  bind_rows(
    read_delim("Bases de datos/ENFR_bases/ENFR 2013 - Base usuario.txt") |> 
      select(prov_res = COD_PROVINCIA,
             sexo = BHCH04,
             edad = BHCH05,
             dm_auto = BIDI01,
             ponderacion = PONDERACION) |> 
      mutate(anio = 2013)
  ) |> 
  
# Añade datos 2018
  bind_rows(
    read_delim("Bases de datos/ENFR_bases/ENFR 2018 - Base usuario.txt") |> 
      select(prov_res = cod_provincia,
             sexo = bhch03,
             edad = bhch04,
             dm_auto = bidi01,
             ponderacion = wf1p) |> 
      mutate(anio = 2018)
  )

### Explorar datos
tabyl(datos_raw$prov_res)

tabyl(datos_raw$sexo)

# Limpieza de datos -------------------------------------------------------
datos <- datos_raw |> 
  
  # Modifica etiquetas provincia
  mutate(prov_res_cat = factor(prov_res,
                            labels = c("CABA", 
                                       "Buenos Aires", 
                                       "Catamarca",
                                       "Córdoba",
                                       "Corrientes",
                                       "Chaco",
                                       "Chubut",
                                       "Entre Ríos",
                                       "Formosa",
                                       "Jujuy",
                                       "La Pampa",
                                       "La Rioja",
                                       "Mendoza",
                                       "Misiones",
                                       "Neuquén",
                                       "Río Negro",
                                       "Salta",
                                       "San Juan",
                                       "San Luis",
                                       "Santa Cruz",
                                       "Santa Fe",
                                       "Santiago del Estero",
                                       "Tucumán",
                                       "Tierra del Fuego")),
         .after = prov_res) |> 
  
  # Modifica etiquetas sexo
  mutate(sexo = if_else(sexo == 1, "Masculino", "Femenino")) |> 
  
  # Modifica etiquetas DM autorreporte
  mutate(dm_auto = case_when(
    dm_auto == 1 ~ "Si",
    dm_auto == 2 ~ "No",
    dm_auto %in% c(9, 99) ~ "NS/NC",
    TRUE ~ NA
  )) |> 
  
  # Crea grupo etario
    mutate(grupo_edad = age_categories(
      edad,
      lower = 15,
      upper = 80,
      by = 5,
      # breakers = c(18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80),
      separator = " a ",
      above.char = " y más"), .after = edad)


# Guardar datos -----------------------------------------------------------
write_csv(datos, file = "Bases de datos/clean/ENFR_dm.csv")

