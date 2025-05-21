### Gráficos exploratorios
### Autora: Tamara Ricardo
### Fecha de modificación:
# Tue May 20 11:29:55 2025 ------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  apyramid,
  scico,
  janitor,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
## Serie defunciones por grupos edad cada 10 años + AVP
defun <- read_delim("Bases de datos/clean/AVP_serie_10.csv")




# Graficar datos ----------------------------------------------------------
# Defunciones promedio por provincia y año ENFR
defun |> 
  ggplot(aes(x = prov_nombre, y = defun_mean_tri, fill = factor(anio_enfr))) +
  
  geom_col(position = "dodge") +
  
  scale_fill_scico_d(palette = "tokyo", end = .75) +
  
  labs(x = "Provincia", y = "Promedio defunciones") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank())


# Defunciones promedio por provincia, sexo y año ENFR
defun |> 
  ggplot(aes(x = prov_nombre, y = defun_mean_tri, fill = factor(anio_enfr))) +
  
  geom_col(position = "dodge") +
  
  scale_fill_scico_d(palette = "tokyo", end = .75) +
  
  labs(x = "Provincia", y = "Promedio defunciones") +
  
  facet_wrap(~ sexo) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank())


# Defunciones promedio por provincia, grupo de edad y año ENFR
defun |> 
  ggplot(aes(x = prov_nombre, y = defun_mean_tri, fill = factor(anio_enfr))) +
  
  geom_col(position = "dodge") +
  
  scale_fill_scico_d(palette = "tokyo", end = .75) +
  
  labs(x = "Provincia", y = "Promedio defunciones") +
  
  facet_wrap(~ grupo_edad_10) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank())


# AVP por provincia y año ENFR
defun |> 
  ggplot(aes(x = prov_nombre, y = AVP, fill = factor(anio_enfr))) +
  
  geom_col(position = "dodge") +
  
  scale_fill_scico_d(palette = "tokyo", end = .75) +
  
  labs(x = "Provincia", y = "AVP") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank())


# AVP por provincia, sexo y año ENFR
defun |> 
  ggplot(aes(x = prov_nombre, y = AVP, fill = factor(anio_enfr))) +
  
  geom_col(position = "dodge") +
  
  scale_fill_scico_d(palette = "tokyo", end = .75) +
  
  labs(x = "Provincia", y = "Promedio defunciones") +
  
  facet_wrap(~ sexo) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank())


# AVP por provincia, grupo de edad y año ENFR
defun |> 
  ggplot(aes(x = prov_nombre, y = AVP, fill = factor(anio_enfr))) +
  
  geom_col(position = "dodge") +
  
  scale_fill_scico_d(palette = "tokyo", end = .75) +
  
  labs(x = "Provincia", y = "Promedio defunciones") +
  
  facet_wrap(~ grupo_edad_10) +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank())
