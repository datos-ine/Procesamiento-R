### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
### en Argentina, período 2005-2018
### Cálculo del índice de Moran global y local para evaluar autocorrelación espacial 
### entre provincias de Argentina para las tasas ajustadas de AVAD, AVP y AVD.
### Autoras: Micaela Gauto y Tamara Ricardo

### Fecha de creación: 11-02-2026
# Última modificación: 18-02-2026

# Carga de paquetes -------------------------------------------------------

pacman::p_load(sf,
               geoAr,
               spdep,
               ggplot2,
               viridis,
               
               # Manejo de datos
               tidyverse,
               rio)


# Carga de bases de datos -------------------------------------------------

## Shapefile de provincias
provincias_sf <- get_geo(geo = "ARGENTINA", level = "provincia") %>% 
  add_geo_codes()

str(provincias_sf)

## Tasas provinciales ajustadas
tasa_std_prov <- import("datos_limpios/arg_tasas_est_prov.rds")

str(tasas_prov_ge10_aj)


# Creación de base de trabajo ---------------------------------------------

provincias_sf <- provincias_sf %>% 
  left_join(tasa_std_prov, by = join_by(codprov_censo == codprov_censo))


# Índice de Moran global --------------------------------------------------

## Vecinos y pesos
provincias_geom <- provincias_sf %>%
  distinct(prov_nombre, geometry)

vecinos <- poly2nb(provincias_geom, queen = TRUE)

pesos <- nb2listw(
  vecinos,
  style = "W",
  zero.policy = TRUE
)

## Función para cálculo de I de Moran global (con permutaciones)
moran_global_perm <- function(x, listw, nsim = 999) {
  
  moran <- moran.mc(
    x,
    listw = listw,
    nsim = nsim,
    zero.policy = TRUE
  )
  
  tibble(
    I = as.numeric(moran$statistic),
    p_value = as.numeric(moran$p.value)
  )
}

### Índice de Moran global para AVAD ----
moran_avad_global <- provincias_sf %>%
  
  group_by(anio_enfr, sexo) %>%
  
  summarise(
    moran = list(
      moran_global_perm(
        AVAD_tasa_std,
        pesos,
        nsim = 999
      )
    ),
    .groups = "drop"
  ) %>%
  unnest(moran) %>% 
  
  mutate(autocorrel = case_when(
    p_value >= 0.05 ~ "ns",
    I > 0  & p_value < 0.05 ~ "positiva",
    I < 0 & p_value < 0.05 ~ "negativa"))

## Sólo se observa una autocorrelación positiva en el año 2009 en ambos sexos.
## En el resto de los casos, la tasa ajustada de AVAD no presenta autocorrelación espacial.

### Índice de Moran global para AVP ----
moran_avp_global <- provincias_sf %>%
  
  group_by(anio_enfr, sexo) %>%
  
  summarise(
    moran = list(
      moran_global_perm(
        AVP_tasa_std,
        pesos,
        nsim = 999
      )
    ),
    .groups = "drop"
  ) %>%
  unnest(moran) %>% 
  
  mutate(autocorrel = case_when(
    p_value >= 0.05 ~ "ns",
    I > 0  & p_value < 0.05 ~ "positiva",
    I < 0 & p_value < 0.05 ~ "negativa"))

## Se observa autocorrelación espacial positiva en el indicador AVP
## en los años 2009 para ambos sexos y en 2018 para varones.

### Índice de Moran global para AVD ----
moran_avd_global <- provincias_sf %>%
  
  group_by(anio_enfr, sexo) %>%
  
  summarise(
    moran = list(
      moran_global_perm(
        AVD_tasa_std,
        pesos,
        nsim = 999
      )
    ),
    .groups = "drop"
  ) %>%
  unnest(moran) %>% 
  
  mutate(autocorrel = case_when(
    p_value >= 0.05 ~ "ns",
    I > 0  & p_value < 0.05 ~ "positiva",
    I < 0 & p_value < 0.05 ~ "negativa"))

## Se observa autocorrelación positiva para el indicador AVD entre varones en el 2005 y 2018.


# Índice de Moral local (con permutaciones) -------------------------------

## Índice de Moran local para AVAD ----
prov_lisa_avad <- provincias_sf %>%
  group_by(anio_enfr, sexo) %>%
  group_modify(~ {
    
    lisa <- localmoran_perm(
      .x$AVAD_tasa_std,
      listw = pesos,
      nsim = 999,
      zero.policy = TRUE
    )
    
    bind_cols(
      .x,
      tibble(
        Ii = lisa[, "Ii"],
        p_value = lisa[, "Pr(z != E(Ii)) Sim"]
      )
    )
  }) %>%
  ungroup()

## Identificación de clusters
media_x <- prov_lisa_avad %>%
  group_by(anio_enfr, sexo) %>%
  summarise(media = mean(AVAD_tasa_std), .groups = "drop")

prov_lisa_avad <- prov_lisa_avad %>%
  left_join(media_x, by = c("anio_enfr", "sexo")) %>%
  
  mutate(
    i_sign = if_else(
      p_value < 0.05, "significativo", "ns"), 
      
    cluster = case_when(
      AVAD_tasa_std >= media & Ii >= 0 & p_value < 0.05 ~ "Alto-Alto",
      AVAD_tasa_std <= media & Ii >= 0 & p_value < 0.05 ~ "Bajo-Bajo",
      AVAD_tasa_std >= media & Ii < 0 & p_value < 0.05 ~ "Alto-Bajo",
      AVAD_tasa_std <= media & Ii < 0 & p_value < 0.05 ~ "Bajo-Alto",
      TRUE ~ "No significativo"
    )
  )


## Índice de Moran local para AVP ----
prov_lisa_avp <- provincias_sf %>%
  group_by(anio_enfr, sexo) %>%
  group_modify(~ {
    
    lisa <- localmoran_perm(
      .x$AVP_tasa_std,
      listw = pesos,
      nsim = 999,
      zero.policy = TRUE
    )
    
    bind_cols(
      .x,
      tibble(
        Ii = lisa[, "Ii"],
        p_value = lisa[, "Pr(z != E(Ii)) Sim"]
      )
    )
  }) %>%
  ungroup()

## Identificación de clusters
media_x <- prov_lisa_avp %>%
  group_by(anio_enfr, sexo) %>%
  summarise(media = mean(AVP_tasa_std), .groups = "drop")

prov_lisa_avp <- prov_lisa_avp %>%
  left_join(media_x, by = c("anio_enfr", "sexo")) %>%
  
  mutate(
    i_sign = if_else(
      p_value < 0.05, "significativo", "ns"), 
    
    cluster = case_when(
      AVP_tasa_std >= media & Ii >= 0 & p_value < 0.05 ~ "Alto-Alto",
      AVP_tasa_std <= media & Ii >= 0 & p_value < 0.05 ~ "Bajo-Bajo",
      AVP_tasa_std >= media & Ii < 0 & p_value < 0.05 ~ "Alto-Bajo",
      AVP_tasa_std <= media & Ii < 0 & p_value < 0.05 ~ "Bajo-Alto",
      TRUE ~ "No significativo"
    )
  )


## Índice de Moran local para AVD ----
prov_lisa_avd <- provincias_sf %>%
  group_by(anio_enfr, sexo) %>%
  group_modify(~ {
    
    lisa <- localmoran_perm(
      .x$AVD_tasa_std,
      listw = pesos,
      nsim = 999,
      zero.policy = TRUE
    )
    
    bind_cols(
      .x,
      tibble(
        Ii = lisa[, "Ii"],
        p_value = lisa[, "Pr(z != E(Ii)) Sim"]
      )
    )
  }) %>%
  ungroup()

## Identificación de clusters
media_x <- prov_lisa_avd %>%
  group_by(anio_enfr, sexo) %>%
  summarise(media = mean(AVD_tasa_std), .groups = "drop")

prov_lisa_avd <- prov_lisa_avd %>%
  left_join(media_x, by = c("anio_enfr", "sexo")) %>%
  
  mutate(
    i_sign = if_else(
      p_value < 0.05, "significativo", "ns"), 
    
    cluster = case_when(
      AVD_tasa_std >= media & Ii >= 0 & p_value < 0.05 ~ "Alto-Alto",
      AVD_tasa_std <= media & Ii >= 0 & p_value < 0.05 ~ "Bajo-Bajo",
      AVD_tasa_std >= media & Ii < 0 & p_value < 0.05 ~ "Alto-Bajo",
      AVD_tasa_std <= media & Ii < 0 & p_value < 0.05 ~ "Bajo-Alto",
      TRUE ~ "No significativo"
    )
  )


# Mapeo -------------------------------------------------------------------
# Se mapea primero el indicador, luego los clusters por provincia

pal_lisa <- c(
  "Alto-Alto" = "#B2182B",
  "Bajo-Bajo" = "#2166AC",
  "Alto-Bajo" = "#EF8A62",
  "Bajo-Alto" = "#67A9CF",
  "No significativo" = "grey80"
)

## AVAD ----
prov_lisa_avad %>% st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = AVAD_tasa_std), color = "white", size = 0.2) +
  scale_fill_viridis(option = "mako", direction = -1) +
  #scale_fill_manual(values = pal_lisa, drop = FALSE) +
  facet_grid(sexo ~ anio_enfr) +
  labs(
    fill = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 8))

prov_lisa_avad %>% st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = cluster), color = "white", size = 0.2) +
  scale_fill_manual(values = pal_lisa, drop = FALSE) +
  facet_grid(sexo ~ anio_enfr) +
  labs(
    fill = "Clusters LISA - AVAD"
  ) +
  theme_minimal() +
  theme(
        #legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 8))


## AVP ----
prov_lisa_avp %>% st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = AVP_tasa_std), color = "white", size = 0.2) +
  scale_fill_viridis(option = "mako", direction = -1) +
  #scale_fill_manual(values = pal_lisa, drop = FALSE) +
  facet_grid(sexo ~ anio_enfr) +
  labs(
    fill = "Tasa ajustada de AVP (c/100.000 hab)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 8))

  
prov_lisa_avp %>% st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = cluster), color = "white", size = 0.2) +
  scale_fill_manual(values = pal_lisa, drop = FALSE) +
  facet_grid(sexo ~ anio_enfr) +
  labs(
    fill = "Clusters LISA - AVP"
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        axis.text = element_text(size = 8))


## AVD ----
prov_lisa_avd %>% st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = AVD_tasa_std), color = "white", size = 0.2) +
  scale_fill_viridis(option = "mako", direction = -1) +
  #scale_fill_manual(values = pal_lisa, drop = FALSE) +
  facet_grid(sexo ~ anio_enfr) +
  labs(
    fill = "Tasa ajustada de AVD (c/100.000 hab)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 8))


prov_lisa_avd %>% st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = cluster), color = "white", size = 0.2) +
  scale_fill_manual(values = pal_lisa, drop = FALSE) +
  facet_grid(sexo ~ anio_enfr) +
  labs(
    fill = "Clusters LISA - AVD"
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        axis.text = element_text(size = 8))

