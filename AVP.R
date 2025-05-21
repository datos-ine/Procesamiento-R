# Calcular AVP ------------------------------------------------------------
## AVP por grupo edad quinquenal
AVP_g5 <- def_join |> 
  
  # Conteo defunciones por año, provincia, sexo y grupo etario
  count(anio, anio_enfr, prov_id, prov_nombre, grupo_edad, sexo,
        wt = total,
        name = "defun_dm") |> 
  
  # Calcular defunciones totales y promedio por trienio ENFR
  group_by(anio_enfr, prov_nombre, grupo_edad, sexo) |> 
  summarise(defun_tri = sum(defun_dm, na.rm = TRUE),
            defun_mean_tri = mean(defun_dm, na.rm = TRUE) |>  round(2),
            .groups = "drop") |> 
  
  # Añadir datos esperanza de vida
  left_join(esp_vida |> select(grupo_edad, sexo, lx, Tx, ex)) |> 
  
  # Calcular AVP
  mutate(AVP = defun_mean_tri * ex)


## AVP por grupo edad cada 10 años
AVP_g10 <- def_join |> 
  # Completar filas faltantes (sin defunciones)
  complete(nesting(anio, anio_enfr), nesting(prov_id, prov_nombre),
           sexo, grupo_edad_10,
           fill = list(total = 0)) |> 
  
  # Conteo defunciones por año, provincia, sexo y grupo etario
  count(anio, anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo,
        wt = total,
        name = "defun_dm") |> 
  
  # Calcular defunciones totales y promedio por trienio ENFR
  group_by(anio_enfr, prov_nombre, grupo_edad_10, sexo) |> 
  summarise(defun_tri = sum(defun_dm, na.rm = TRUE),
            defun_mean_tri = mean(defun_dm, na.rm = TRUE) |>  round(2),
            .groups = "drop") |> 
  
  # Añadir datos esperanza de vida
  left_join(esp_vida |> 
              # Esperanza de vida por grupo edad quinquenal
              group_by(grupo_edad_10, sexo) |> 
              summarise(esp_vida = sum(Tx)/sum(lx),
                        .groups = "drop")
  ) |> 
  
  # Calcular AVP
  mutate(AVP = defun_mean_tri * esp_vida)


# Explorar datos limpios --------------------------------------------------
## Diferencias en la mortalidad por DM según sexo
# Grupos quinquenales
mod1 <- lm(defun_mean_tri ~ sexo, data = AVP_g5) 

summary(mod1) # p = 0.521

# Grupos cada 10 años
mod2 <- lm(defun_mean_tri ~ sexo, data = AVP_g10) 

summary(mod2) # p = 0.585 


## Diferencias en la mortalidad por DM según sexo y grupo etario
# Grupos quinquenales
mod3 <- lm(defun_mean_tri ~ sexo * grupo_edad, data = AVP_g5) 

summary(mod3) # Solo hay diferencias significativas para el grupo 80+ (p = 0.001)


# Grupos cada 10 años
mod4 <- lm(defun_mean_tri ~ sexo * grupo_edad_10, data = AVP_g10) 

summary(mod4) # Solo hay diferencias significativas para el grupo 80+ (p = 0.044)


# Guardar datos limpios ---------------------------------------------------
write_csv(AVP_g5, file = "Bases de datos/clean/AVP_serie_5.csv")

write_csv(AVP_g10, file = "Bases de datos/clean/AVP_serie_10.csv")

# Limpiar environment
rm(list = ls())