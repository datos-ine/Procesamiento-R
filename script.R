# Limpiar datos -----------------------------------------------------------
pob_clean <- pob_05 |> 
  # Estandarizar nombres de columnas
  clean_names() |> 
  
  # Unir con proyecciones 2010-18
  bind_rows(pob_10 |> 
              clean_names()) |> 
  
  # Renombrar columnas
  rename(prov_nombre = prov_desc,
         grupo_edad_quin = grupedad) |> 
  
  # Completar año faltante
  mutate(anio = replace_na(ano, 2005)) |> 
  
  # Añadir año ENFR
  mutate(anio_enfr = fct_relabel(factor(anio), ~ c("2005", "2009", "2013", "2018"))) |> 
  
  # Filtrar categorías de edad no relevantes
  filter(!str_detect(grupo_edad_quin, "0-4$|5-9$|10-14|80ymas|Total")) |> 
  
  # Modificar niveles grupo edad quinquenal
  mutate(grupo_edad_quin = fct_collapse(grupo_edad_quin,
                                        "80 y más" = c("80-84", "85-89",
                                                       "90-94", "95-99", "100 y más"))
  ) |> 
  
  # Crear variable para grupo de edad ENFR
  mutate(grupo_edad_enfr = fct_collapse(grupo_edad_quin,
                                        "18 a 24" = c("15-19", "20-24"),
                                        "25 a 34" = c("25-29", "30-34"),
                                        "35 a 49" = c("35-39", "40-44", "45-49"),
                                        "50 a 64" = c("50-54", "55-59", "60-64"),
                                        "65+" = c("65-69", "70-74", "75-79",
                                                  "80 y más"))) |> 
  
  # Formato long
  pivot_longer(cols = c("varones", "mujeres"),
               names_to = "sexo") |> 
  
  # Modificar etiquetas sexo
  mutate(sexo = factor(sexo,
                       labels = c("Varón",
                                  "Mujer")))


# Proyección por grupo edad quinquenal
serie_pob_quin <- pob_clean |> 
  group_by(anio, anio_enfr, prov_nombre, grupo_edad_quin, sexo) |> 
  summarise(proy_pop = sum(value, na.rm = TRUE),
            .groups = "drop")

# Proyección por grupo edad ENFR
serie_pob_enfr <- pob_clean |> 
  group_by(anio, anio_enfr, prov_nombre, grupo_edad_enfr, sexo) |> 
  summarise(proy_pob = sum(value, na.rm = TRUE),
            .groups = "drop")


# Análisis de datos -------------------------------------------------------
# Grupo edad ENFR
serie_AVAD_enfr <- serie_AVD_enfr |> 
  # Unir con defunciones y AVP
  left_join(serie_AVP_enfr) |> 
  
  # Variables caracter a factor
  mutate(across(.cols = c(anio_enfr:sexo),
                .fns = ~ factor(.x))
  ) |> 
  
  # Calcular AVAD
  mutate(AVAD = AVP + AVD_total) |> 
  
  # Calcular proporción AVP y AVD
  rowwise() |> 
  mutate(prop_AVP = AVP/AVD_total*100,
         prop_AVD = AVD_total/AVP*100) |> 
  
  # Unir con datos proyección poblacional
  left_join(serie_pob_enfr) |> 
  
  # Calcular tasas brutas
  mutate(
    tasa_AVP = AVP/proy_pob * 100000,
    tasa_AVD = AVD_total/proy_pob * 100000,
    tasa_AVAD = AVAD/proy_pob * 100000,
  )


# Explorar datos ----------------------------------------------------------
# Tasas por año ENFR, provincia y sexo
serie_AVAD_enfr |> 
  group_by(anio_enfr, prov_nombre, sexo) |> 
  summarise(AVP_gral = sum(AVP)/sum(proy_pob) * 100000,
            AVD_gral = sum(AVD_total)/sum(proy_pob) * 100000,
            AVAD_gral = sum(AVAD)/sum(proy_pob) *100000,
  )

# Tasas por año ENFR, sexo y grupo de edad
serie_AVAD_enfr |> 
  group_by(anio_enfr, grupo_edad_enfr, sexo) |> 
  summarise(AVP_gral = sum(AVP)/sum(proy_pob) * 100000,
            AVD_gral = sum(AVD_total)/sum(proy_pob) * 100000,
            AVAD_gral = sum(AVAD)/sum(proy_pob) *100000,
  )



