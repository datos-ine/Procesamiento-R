### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
### en Argentina, período 2005-2018
### Prueba de gráficos y presentación de resultados - nivel nacional

### Autoras:
## - Micaela Gauto
## - Tamara Ricardo

### Fecha de creación: 11-02-2026
# Última modificación: 27-05-2026 13:24

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  apyramid,
  gghighlight,
  scico,
  patchwork,
  scales,
  MetBrewer,
  ggrepel,
  readxl,
  segmented,
  joinpointR,
  ggridges, # ridge plot
  hrbrthemes,
  viridis,
  ggpattern,
  ggbump,
  # Manejo de datos
  rio,
  janitor,
  tidyverse
)


# Cargar datos limpios ----------------------------------------------------
## Datos de AVAD, AVP y AVD por grupo decenal y sexo a nivel nacional
avad_arg <- import("datos_limpios/arg_sim_avad.rds")

## Tasas de AVAD, AVP y AVD ajustadas por edad según sexo a nivel nacional
tasas_avad_arg <- import("datos_limpios/arg_sim_tasa_est.rds")

## Recuento absoluto de AVAD, AVP y AVD según sexo a nivel nacional
abs_avad_arg <- import("datos_limpios/arg_avad_abs.rds")

## Proyecciones poblacionales
proy_pob <- import("datos_limpios/arg_proy_pob.rds")

## Población estándar Censo 2010
pob_est_2010 <- import("datos_limpios/pob_est_2010.rds")


# Tablas anexas para gráficos ---------------------------------------------

## Recuento absoluto de AVAD a nivel nacional: varones, mujeres y ambos sexos ----
abs_avad_arg <- abs_avad_arg %>% 

  # Edito categorías de sexo para etiquetas
  mutate(
    sexo = case_when(
      sexo == "Mujer" ~ "Mujeres",
      sexo == "Varón" ~ "Varones",
      .default = sexo
    )
  )

## Aumento proporcional de AVAD, AVD y AVP 2005-2018 ----
indic_cambio <- abs_avad_arg %>%
  
  select(anio_enfr:AVAD) %>% 

  pivot_longer(
    cols = c("AVAD", "AVD", "AVP"),
    names_to = "indicador",
    values_to = "valor"
  ) %>%

  # Calculo cambio absoluto y relativo de cada indicador respecto del año basal (2005)
  group_by(sexo, indicador) %>%
  arrange(anio_enfr) %>%
  mutate(
    indic_cambio_abs = valor - first(valor), # cambio absoluto
    indic_cambio_perc = (valor - first(valor)) * 100 / first(valor)) # cambio relativo


## Distribución de AVAD, AVD y AVP por sexo y grupo de edad para pirámides ----
datos_piramide <- avad_arg |>

  group_by(anio_enfr) %>%
  mutate(
    AVAD_prop = AVAD / (sum(AVAD)), # proporción de AVAD sobre AVAD totales
    AVD_prop_t = AVD / (sum(AVAD)), # propoción de AVD sobre AVAD totales
    AVP_prop_t = AVP / (sum(AVAD)), # propoción de AVP sobre AVAD totales
    AVD_prop = AVD / (sum(AVD)), # propoción de AVD sobre AVD totales
    AVP_prop = AVP / (sum(AVP))
  ) %>% # propoción de AVP sobre AVP totales

  pivot_longer(
    cols = c(AVAD_prop, AVD_prop_t, AVP_prop_t, AVD_prop, AVP_prop),
    names_to = "indicador",
    values_to = "valor"
  ) %>%

  select(anio_enfr, sexo, grupo_edad_10, AVD, AVP, AVAD, indicador, valor) %>%

  # Edito categorías de sexo para etiquetas
  mutate(
    sexo = case_when(
      sexo == "Mujer" ~ "Mujeres",
      sexo == "Varón" ~ "Varones",
      .default = sexo
    )
  )


## Distribución de AVAD, AVD y AVP por sexo y grupo de edad para áreas ----
datos_area <- datos_piramide %>%

  # Filtro proporción de cada indicador sobre recuento total de AVAD
  filter(indicador %in% c("AVAD_prop", "AVD_prop_t", "AVP_prop_t")) %>%

  # Edito nombre de categorías de indicador para etiquetas
  mutate(
    indicador = case_when(
      indicador == "AVAD_prop" ~ "AVAD",
      indicador == "AVD_prop_t" ~ "AVD",
      indicador == "AVP_prop_t" ~ "AVP",
      .default = indicador
    ),

    # Valores de mujeres en negativo para gráfico
    valor_plot = if_else(sexo == "Mujeres", valor * (-1), valor)
  )


## AVD por complicación: tasas específicas para tipo de complicación por sexo y grupo de edad ----
AVD_tasas_ind <- avad_arg %>% 
  
  select(anio_enfr, sexo, grupo_edad_10,
         contains("AVD_tasa_total")) %>% 
  
  # Edito nombre de categorías de sexo e tipo de complicación para etiquetas
  mutate(
    sexo = case_when(
      sexo == "Mujer" ~ "Mujeres",
      sexo == "Varón" ~ "Varones",
      .default = sexo
    ))

## AVD por complicación: cambios relativos y absolutos respecto del 2005 ----
AVD_cambio <- abs_avad_arg %>%

  select(c(anio_enfr, sexo, AVD_total_micro, AVD_total_macro, AVD_sin_complicaciones)) %>% 
  
  pivot_longer(
    cols = contains("AVD"),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  
  # Calculo cambio absoluto y relativo de cada indicador respecto del año basal (2005)
  group_by(sexo, indicador) %>%
  arrange(anio_enfr) %>%
  mutate(
    indic_cambio_abs = valor - first(valor), # cambio absoluto
    indic_cambio_perc = (valor - first(valor)) * 100 / first(valor)) # cambio relativo


# Gráficos exploratorios --------------------------------------------------
# Paleta por indicador y sexo (accesible) ----
colores_indicador <- c(
  "AVAD" = "#1B6FA8",
  "AVD" = "#2E9B6E",
  "AVP" = "#D4660A",
  "Mujeres" = "#7B3FA0",
  "Varones" = "#C4820A"
)

## Tendencia del recuento absoluto AVAD, AVD y AVP por año y sexo ----

### Gráfico de tendencia: áreas ----
abs_avad_arg |>

  pivot_longer(
    cols = c(AVD, AVP),
    names_to = "indicador",
    values_to = "recuento"
  ) %>%

  ggplot(aes(
    x = anio_enfr,
    y = recuento,
    group = indicador,
    fill = indicador
  )) +

  geom_area(alpha = 1) +

  labs(x = "Año", y = "Recuento", fill = "Indicador") +

  facet_wrap(~sexo) +

  scale_y_continuous(
    labels = label_number(
      big.mark = ".", # separador de miles
      decimal.mark = ","
    )
  ) +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  ) +

  scale_fill_manual(
    values = c("#2E9B6E", "#D4660A"),

    labels = c("AVD_total" = "AVD", "AVP_total" = "AVP")
  )


### Decomposition plot: tendencia y relación entre AVP y AVD por año y sexo  ----
abs_avad_arg %>%

  filter(sexo != "Ambos sexos") %>%

  ggplot(aes(x = AVP, y = AVD, size = AVAD, color = sexo)) +

  geom_point(alpha = 0.7) +

  scale_x_continuous(
    limits = c(0, NA),
    labels = label_number(
      big.mark = ".", # separador de miles
      decimal.mark = ","
    )
  ) +

  scale_y_continuous(
    limits = c(0, NA),
    labels = label_number(
      big.mark = ".", # separador de miles
      decimal.mark = ","
    )
  ) +

  scale_size_continuous(name = "AVAD (recuento)", transform = "sqrt") +

  labs(
    x = "AVP (recuento)",
    y = "AVD (recuento)",
    color = "Sexo"
  ) +

  facet_wrap(ncol = 4, vars(anio_enfr)) +

  scale_color_manual(
    values = c(
      "Varones" = "#C4820A",
      "Mujeres" = "#7B3FA0"
    )
  ) +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold")
  )


## Tasas ajustadas: Tendencia nacional por año y sexo ----
# tasas_avad_arg |>
#   select(anio_enfr, sexo, AVD_tasa_std, AVP_tasa_std, AVAD_tasa_std) %>%
#   mutate(sexo = if_else(sexo == "Mujer", "Mujeres", "Varones")) %>%
#   pivot_longer(cols = c(AVD_tasa_std, AVP_tasa_std, AVAD_tasa_std), names_to = "indicador", values_to = "tasas") %>%
#
#   ggplot(aes(x = anio_enfr, y = tasas, colour = indicador, group = indicador)) +
#   geom_line() +
#   geom_point() +
#
#   # Dividir por sexo
#   facet_grid(cols = vars(sexo)) +
#
#   # Etiquetas ejes
#   labs(x = "Año", y = "Tasa ajustada (c/100.000 hab)", colour = "") +
#
#   # Tema
#   theme_minimal() +
#   # theme(axis.text.x = element_text(angle = 90)) +
#   # scale_x_continuous(breaks = c(2005, 2009, 2013, 2018),
#   #                    labels = c("2005", "2009", "2013", "2018")) +
#   # scale_y_continuous(
#   # labels = label_number(
#   #   big.mark = ",",     # separador de miles
#   #   decimal.mark = "."
#   # )) +
#
#   scale_colour_manual(values = c("#1B6FA8",
#                                  "#2E9B6E",
#                                  "#D4660A"),
#                       labels = c("AVAD_tasa_std" = "AVAD",
#                                  "AVD_tasa_std" = "AVD",
#                                  "AVP_tasa_std" = "AVP"))

## Distribución proporcional de indicadores por sexo y grupo edad decenal ----

### Pirámide de AVAD por sexo y grupo edad decenal ----

datos_piramide %>%
  filter(indicador == "AVAD_prop") %>%

  # Pirámide
  age_pyramid(
    age_group = grupo_edad_10,
    split_by = sexo,
    stack_by = sexo,
    count = valor,
    proportional = T,
    show_midpoint = FALSE
  ) +

  # Dividir por año ENFR
  facet_wrap(~anio_enfr) +

  # Colores
  scale_fill_manual(
    values = c(
      "Varones" = "#C4820A",
      "Mujeres" = "#7B3FA0"
    )
  ) +

  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Proporción de AVAD (%)", fill = "") +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  ) +

  # Formato del eje y
  scale_y_continuous(
    expand = c(0.2, 0),
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  )


### Pirámide de AVD y AVP combinados ----

datos_piramide %>%
  filter(indicador %in% c("AVD_prop_t", "AVP_prop_t")) %>%

  # Pirámide
  age_pyramid(
    age_group = grupo_edad_10,
    split_by = sexo,
    stack_by = indicador,
    count = valor,
    show_midpoint = FALSE
  ) +

  # Dividir por año ENFR
  facet_wrap(~anio_enfr) +

  # Colores
  scale_fill_manual(
    values = c(
      "AVD_prop_t" = "#2E9B6E",
      "AVP_prop_t" = "#D4660A"
    ),
    labels = c("AVD_prop_t" = "AVD", "AVP_prop_t" = "AVP")
  ) +

  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Proporción de AVAD (%)", fill = "") +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  ) +

  scale_y_continuous(
    expand = c(0.2, 0),
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  )

### Pirámide de AVP sobre AVP total ----

datos_piramide %>%
  filter(indicador == "AVP_prop") %>%

  # Pirámide
  age_pyramid(
    age_group = grupo_edad_10,
    split_by = sexo,
    stack_by = sexo,
    count = valor,
    proportional = T,
    show_midpoint = FALSE
  ) +

  # Dividir por año ENFR
  facet_wrap(~anio_enfr) +

  # Colores
  scale_fill_manual(
    values = c(
      "Varones" = "#C4820A",
      "Mujeres" = "#7B3FA0"
    )
  ) +

  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Proporción de AVP (%)", fill = "") +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  ) +

  scale_y_continuous(
    expand = c(0.2, 0),
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  )

### Pirámide de AVD sobre AVD total ----

datos_piramide %>%
  filter(indicador == "AVD_prop") %>%

  # Pirámide
  age_pyramid(
    age_group = grupo_edad_10,
    split_by = sexo,
    stack_by = sexo,
    count = valor,
    proportional = T,
    show_midpoint = FALSE
  ) +

  # Dividir por año ENFR
  facet_wrap(~anio_enfr) +

  # Colores
  scale_fill_manual(
    values = c(
      "Varones" = "#C4820A",
      "Mujeres" = "#7B3FA0"
    )
  ) +

  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Proporción de AVD (%)", fill = "") +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  ) +

  scale_y_continuous(
    expand = c(0.2, 0),
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  )

### Pirámides de cada indicador sobre AVAD totales graficadas en conjunto ----

datos_piramide %>%
  filter(indicador %in% c("AVAD_prop", "AVD_prop_t", "AVP_prop_t")) %>%
  mutate(
    indicador = case_when(
      indicador == "AVAD_prop" ~ "AVAD",
      indicador == "AVD_prop_t" ~ "AVD",
      indicador == "AVP_prop_t" ~ "AVP",
      .default = indicador
    )
  ) %>%

  age_pyramid(
    age_group = grupo_edad_10,
    split_by = sexo,
    stack_by = sexo,
    count = valor,
    proportional = T,
    show_midpoint = FALSE
  ) +

  # Dividir por año ENFR
  facet_grid(rows = vars(anio_enfr), cols = vars(indicador)) +

  # Colores
  scale_fill_manual(
    values = c(
      "Varones" = "#C4820A",
      "Mujeres" = "#7B3FA0"
    )
  ) +

  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Proporción (%)", fill = "") +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  ) +

  scale_y_continuous(
    expand = c(0.02, 0),
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  )


### Gráfico de áreas: proporción de cada indicador sobre AVAD totales ----
datos_area %>%

  ggplot(aes(x = grupo_edad_10, group = indicador, fill = indicador)) +

  geom_area(
    data = datos_area %>% filter(sexo == "Mujeres"),
    aes(y = valor_plot),
    stat = "identity",
    position = position_identity(),
    alpha = 0.6
  ) +
  geom_area(
    data = datos_area %>% filter(sexo == "Varones"),
    aes(y = valor_plot),
    stat = "identity",
    position = position_identity(),
    alpha = 0.6
  ) +

  geom_line(y = 0) +

  # Etiquetas de sexo
  annotate(
    "label",
    x = 1, # primera categoría del eje x
    y = 0.11, # ajustá según la escala de tus datos
    label = "Varones",
    hjust = 0,
    vjust = 0,
    size = 3.5,
    fill = "white",
    alpha = 0.7,
    label.size = 0.3,
    color = "grey30"
  ) +

  annotate(
    "label",
    x = 1,
    y = -0.11, # ajustá según la escala de tus datos
    label = "Mujeres",
    hjust = 0,
    vjust = 1,
    size = 3.5,
    fill = "white",
    alpha = 0.7,
    label.size = 0.3,
    color = "grey30"
  ) +

  labs(x = "Grupo de edad", y = "Proporción (%)", fill = "Indicador") +

  facet_grid(cols = vars(anio_enfr)) +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 90),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  ) +

  scale_y_continuous(
    labels = function(x) scales::percent(abs(x), accuracy = 1)
  ) +

  scale_fill_manual(values = c("#1B6FA8", "#2E9B6E", "#D4660A"))


## Descripción de AVD según complicación ----

### Paletas para gráficos (accesible) ----

colores_tipo_comp <- c(
  "Microvasculares" = "#1B6FA8",
  "Macrovasculares" = "#2E9B6E",
  "Sin complicaciones" = "#D4660A"
)

colores_comp <- c(
  # macrovasculares
  "ACV" = "#1B6FA8",
  "Claudicación miembros inferiores" = "#2E9B6E",
  "IAM" = "#D4660A",
  "IC" = "#7B3FA0",

  # microvasculares
  "Amputación" = "#1B6FA8",
  "Ceguera" = "#2E9B6E",
  "Disfunción eréctil" = "#D4660A",
  "Nefropatía" = "#7B3FA0",
  "Neuropatía periférica" = "#C4820A",
  "Retinopatía no proliferativa" = "#1A7A6E",
  "Retinopatía proliferativa" = "#B04030",

  "Sin complicaciones" = "#5A5AA0"
)

### Bump chart según tipo de complicación (microvascular o macrovascular) y sexo ----

abs_avad_arg %>%
  select(anio_enfr, sexo, AVD_total_micro, AVD_total_macro, AVD_sin_complicaciones) %>% 
  
  pivot_longer(
    cols = c(AVD_total_micro, AVD_total_macro, AVD_sin_complicaciones),
    names_to = "indicador",
    values_to = "recuento"
  ) %>%
  
  mutate(indicador = case_when(
    indicador == "AVD_total_micro" ~ "Microvasculares",
    indicador == "AVD_total_macro" ~ "Macrovasculares",
    indicador == "AVD_sin_complicaciones" ~ "Sin complicaciones"
  )) %>% 
  
  ggplot(aes(
    x = anio_enfr,
    y = recuento,
    color = indicador,
    group = indicador
  )) +

  geom_bump(linewidth = 0.9, smooth = 6) +
  geom_point(size = 2.5) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.4
  ) +

  facet_wrap(~sexo) +
  scale_color_manual(values = colores_tipo_comp) +

  scale_y_continuous(
    limits = c(0, NA),
    labels = label_number(
      big.mark = ".", # separador de miles
      decimal.mark = ","
    )
  ) +

  labs(
    x = "Año",
    y = "AVD (recuento)",
    color = "Tipo de complicaciones"
  ) +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "right",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 0),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold")
  )

### Recuento de AVD para complicaciones microvasculares ----

abs_avad_arg %>% 
  select(anio_enfr, sexo, contains("AVD_micro")) %>% 
  filter(sexo != "Ambos sexos") %>% 
  
  pivot_longer(
    cols = contains("AVD_micro"),
    names_to = "indicador",
    values_to = "recuento"
  ) %>%
  
  mutate(indicador = case_when(
    indicador == "AVD_micro_amputacion" ~ "Amputación",
    indicador == "AVD_micro_ceguera" ~ "Ceguera",
    indicador == "AVD_micro_disf_erectil" ~ "Disfunción eréctil",
    indicador == "AVD_micro_nefropatia" ~ "Nefropatía",
    indicador == "AVD_micro_neuropatia_p" ~ "Neuropatía periférica",
    indicador == "AVD_micro_retinopatia_np" ~ "Retinopatía no proliferativa",
    indicador == "AVD_micro_retinopatia_p" ~ "Retinopatía proliferativa"
  )) %>%

  ggplot(aes(
    x = anio_enfr,
    y = recuento,
    group = indicador,
    color = indicador
  )) +

  geom_bump(linewidth = 0.9, smooth = 6) +
  geom_point(size = 2.5) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.4
  ) +

  scale_color_manual(values = colores_comp) +

  # Etiquetas al final de cada línea
  geom_text_repel(
    data = abs_avad_arg %>% 
      select(anio_enfr, sexo, contains("AVD_micro")) %>% 
      
      pivot_longer(
        cols = contains("AVD_micro"),
        names_to = "indicador",
        values_to = "recuento"
      ) %>%
      
      filter(
          anio_enfr == 2018 &
          sexo != "Ambos sexos"
      ) %>% 
      
      mutate(indicador = case_when(
        indicador == "AVD_micro_amputacion" ~ "Amputación",
        indicador == "AVD_micro_ceguera" ~ "Ceguera",
        indicador == "AVD_micro_disf_erectil" ~ "Disfunción eréctil",
        indicador == "AVD_micro_nefropatia" ~ "Nefropatía",
        indicador == "AVD_micro_neuropatia_p" ~ "Neuropatía periférica",
        indicador == "AVD_micro_retinopatia_np" ~ "Retinopatía no proliferativa",
        indicador == "AVD_micro_retinopatia_p" ~ "Retinopatía proliferativa"
      )),

    aes(label = indicador),
    hjust = 0,
    nudge_x = 0.7, # empuja las etiquetas hacia la derecha
    direction = "y", # solo se mueven verticalmente para evitar solapamiento
    segment.size = 0.3, # grosor de la línea que conecta etiqueta con punto
    segment.color = "grey60",
    size = 3,
    fontface = "plain",
    box.padding = 0.3 # espacio mínimo entre etiquetas
  ) +

  facet_wrap(~sexo) +

  # Etiquetas de eje
  labs(
    x = "Año",
    y = "AVD (recuento)"
  ) +

  scale_x_discrete(
    expand = expansion(mult = c(0.05, 0.35)) # espacio para etiquetas
  ) +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "none",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 0),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold")
  )

### Recuento de AVD para complicaciones macrovasculares ----

abs_avad_arg %>% 
  select(anio_enfr, sexo, contains("AVD_macro")) %>% 
  filter(sexo != "Ambos sexos") %>% 
  
  pivot_longer(
    cols = contains("AVD_macro"),
    names_to = "indicador",
    values_to = "recuento"
  ) %>%

  mutate(indicador = case_when(
    indicador == "AVD_macro_iam" ~ "IAM",
    indicador == "AVD_macro_acv" ~ "ACV",
    indicador == "AVD_macro_ic" ~ "IC",
    indicador == "AVD_macro_claudicacion" ~ "Claudicación miembros inferiores",
    )) %>%
  
  ggplot(aes(
    x = anio_enfr,
    y = recuento,
    group = indicador,
    color = indicador
  )) +
  
  geom_bump(linewidth = 0.9, smooth = 6) +
  geom_point(size = 2.5) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.4
  ) +
  
  scale_color_manual(values = colores_comp) +
  
  # Etiquetas al final de cada línea
  geom_text_repel(
    data = abs_avad_arg %>% 
      select(anio_enfr, sexo, contains("AVD_macro")) %>% 
      
      pivot_longer(
        cols = contains("AVD_macro"),
        names_to = "indicador",
        values_to = "recuento"
      ) %>%
      
      filter(
        anio_enfr == 2018 &
          sexo != "Ambos sexos"
      ) %>% 
      
      mutate(indicador = case_when(
        indicador == "AVD_macro_iam" ~ "IAM",
        indicador == "AVD_macro_acv" ~ "ACV",
        indicador == "AVD_macro_ic" ~ "IC",
        indicador == "AVD_macro_claudicacion" ~ "Claudicación miembros inferiores",
      )),
    
    aes(label = indicador),
    hjust = 0,
    nudge_x = 0.7, # empuja las etiquetas hacia la derecha
    direction = "y", # solo se mueven verticalmente para evitar solapamiento
    segment.size = 0.3, # grosor de la línea que conecta etiqueta con punto
    segment.color = "grey60",
    size = 3,
    fontface = "plain",
    box.padding = 0.3 # espacio mínimo entre etiquetas
  ) +
  
  facet_wrap(~sexo) +
  
  # Etiquetas de eje
  labs(
    x = "Año",
    y = "AVD (recuento)"
  ) +
  
  scale_x_discrete(
    expand = expansion(mult = c(0.05, 0.35)) # espacio para etiquetas
  ) +
  
  # Tema
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "none",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 0),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold")
  )

### Según tasas por tipo de complicación y grupo de edad ----

AVD_tasas_ind %>%
  pivot_longer(
    cols = contains("AVD_tasa"),
    names_to = "indicador",
    values_to = "tasa_esp"
  ) %>%
  
  mutate(indicador = case_when(
    indicador == "AVD_tasa_total_micro" ~ "Microvasculares",
    indicador == "AVD_tasa_total_macro" ~ "Macrovasculares"
  )) %>%

  ggplot(aes(
    x = anio_enfr,
    y = tasa_esp,
    group = grupo_edad_10,
    color = grupo_edad_10
  )) +

  geom_bump(linewidth = 0.9, smooth = 6) +
  geom_point(size = 2.5) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.4
  ) +

  # Etiquetas al final de cada línea
  geom_text_repel(
    data = AVD_tasas_ind %>%
      pivot_longer(
        cols = contains("AVD_tasa"),
        names_to = "indicador",
        values_to = "tasa_esp"
      ) %>%
      
      mutate(indicador = case_when(
        indicador == "AVD_tasa_total_micro" ~ "Microvasculares",
        indicador == "AVD_tasa_total_macro" ~ "Macrovasculares"
      )) %>%

      filter(anio_enfr == 2018),

    aes(label = grupo_edad_10),
    hjust = 0,
    nudge_x = 0.6, # empuja las etiquetas hacia la derecha
    direction = "y", # solo se mueven verticalmente para evitar solapamiento
    segment.size = 0.3, # grosor de la línea que conecta etiqueta con punto
    segment.color = "grey60",
    size = 3,
    fontface = "plain",
    box.padding = 0.8 # espacio mínimo entre etiquetas
  ) +

  scale_color_manual(
    values = c(
      "30 a 39" = "#1B6FA8",
      "40 a 49" = "#2E9B6E",
      "50 a 59" = "#D4660A",
      "60 a 69" = "#7B3FA0",
      "70 a 79" = "#C4820A",
      "80+" = "#1A7A6E"
    )
  ) +

  scale_y_continuous(
    limits = c(0, NA),
    labels = label_number(
      big.mark = ".", # separador de miles
      decimal.mark = ","
    )
  ) +

  facet_grid(rows = vars(sexo), cols = vars(indicador)) +

  labs(
    x = "Año",
    y = "Tasa específica de AVD (c/100.000 hab)",
    color = "Grupo de edad"
  ) +

  # Tema
  theme_minimal(base_size = 14) +

  theme(
    legend.position = "none",
    axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
    axis.text.x = element_text(size = 10, angle = 0),
    axis.title.y = element_text(face = "bold", margin = margin(r = 15)),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(face = "bold", size = 12),
    strip.text = element_text(face = "bold")
  )


# Análisis joinpoint ------------------------------------------------------
tasas_jp <- tasas_avad_arg |>
  # Año ENFR a numérico
  mutate(anio_enfr = parse_number(as.character(anio_enfr)))


## Tasa de AVAD ajustada por edad ----
## model_jp no selecciona joinpoints porque la diferencia entre el modelo con 0
## y 1 breakpoints no es significativa (menos de 2 puntos de diferencia en el BIC).
## Para ajustar directamente un joinpoint usar segmented::segmented(), aunque igual da
## un warning y no calcula IC, probablemente porque solo tenemos 4 puntos en el tiempo.
## Voy a crear una nueva función en el paquete joinpointR para que permita ajustar la
## lista de modelos al ingresar la variable de agrupamiento. Por ahora usar:
mod_avad <- list(
  mujer = segmented(
    obj = lm(
      log(AVAD_tasa_std) ~ anio_enfr,
      data = tasas_jp |> filter(sexo == "Mujer")
    ),
    npsi = 1
  ),
  varon = segmented(
    obj = lm(
      log(AVAD_tasa_std) ~ anio_enfr,
      data = tasas_jp |> filter(sexo == "Varón")
    ),
    npsi = 1
  )
)

# APC (only works when class segmented lm)
get_apc(mod_avad$mujer, digits = 1, time = "year", dec = ".") # Will generate an error

get_apc(mod_avad$varon, digits = 1, time = "year", dec = ".")

# AAPC with 95% CI
get_aapc(mod_avad$mujer, show_ci = TRUE)

# AAPC with 95% CI
get_aapc(mod_avad$mujer, show_ci = TRUE)

# Summary Table
summary_jp(mod_avad)

# Gráfico facetado
mod_avad |>
  gg_jpoint(obs = TRUE, jp = TRUE, facets = TRUE)


## Tasa de AVD ajustada por edad ----

mod_AVD <- tasas_avad_arg %>%

  mutate(anio_enfr = as.numeric(anio_enfr)) %>%

  model_jp(
    value = "AVD_tasa_std",
    time = "anio_enfr",
    group = "sexo",
    k = 2,
    test = FALSE
  )


# APC (only works when class segmented lm)
get_apc(mod_AVD$Mujer, digits = 1, time = "year", dec = ".") # Will generate an error

get_apc(mod_AVD$Varón, digits = 1, time = "year", dec = ".")

# AAPC with 95% CI
get_aapc(mod_AVD$Mujer, show_ci = TRUE)

# AAPC with 95% CI
get_aapc(mod_AVD$Varón, show_ci = TRUE)

# Summary Table
summary_jp(mod_AVD)

# Gráfico facetado
mod_AVD |>
  gg_jpoint(obs = TRUE, jp = TRUE, facets = TRUE)


## Tasa de AVP ajustada por edad ----

mod_AVP <- tasas_avad_arg %>%

  mutate(anio_enfr = as.numeric(anio_enfr)) %>%

  model_jp(
    value = "AVP_tasa_std",
    time = "anio_enfr",
    group = "sexo",
    test = FALSE
  )

# APC (only works when class segmented lm)
get_apc(mod_AVP$Mujer, digits = 1, time = "year", dec = ".") # Will generate an error

get_apc(mod_AVP$Varón, digits = 1, time = "year", dec = ".")

# AAPC with 95% CI
get_aapc(mod_AVP$Mujer, show_ci = TRUE)

# AAPC with 95% CI
get_aapc(mod_AVP$Varón, show_ci = TRUE)

# Summary Table
summary_jp(mod_AVP)

# Gráfico facetado
mod_AVP |>
  gg_jpoint(obs = TRUE, jp = TRUE, facets = TRUE)


# Guardar gráficos --------------------------------------------------------

ggsave(
  plot = g1,
  filename = "prueba.png",
  device = ragg::agg_png(),
  units = "cm",
  width = 18,
  height = 10,
  dpi = 300
)
