### Análisis espacial y tendencia de la carga de enfermedad por diabetes mellitus
### en Argentina, período 2005-2018
### Prueba de gráficos y presentación de resultados

### Autoras:
## - Micaela Gauto
## - Tamara Ricardo
### Fecha de creación: 11-02-2026
# Última modificación: 19-02-2026


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  
  apyramid,
  gghighlight,
  scico,
  patchwork,
  scales,
  MetBrewer,
  ggrepel,
  # Manejo de datos
  rio,
  janitor,
  tidyverse,
  readxl,
  ggridges # ridge plot
  )


# Cargar datos limpios ----------------------------------------------------

## Datos de AVAD, AVP y AVD por grupo decenal y sexo a nivel nacional
avad_arg <- import("datos_limpios/arg_sim_avad_dm2.rds")

## Datos de AVAD, AVP y AVD por grupo decenal y sexo según región
avad_reg <- import("datos_limpios/arg_sim_avad_dm2_reg.rds")

## Datos de AVAD, AVP y AVD por grupo decenal y sexo según provincias
avad_prov <- import("datos_limpios/arg_sim_avad_dm2_prov.rds")

## Datos de AVAD, AVP y AVD por grupo decenal y sexo según provincias
avd_ind <- import("datos_limpios/arg_sim_avd_ind.rds")

## Tasas de AVAD, AVP y AVD ajustadas por edad según sexo a nivel nacional
tasas_avad_arg <- import("datos_limpios/arg_tasas_est.rds")

## Tasas de AVAD, AVP y AVD ajustadas por edad según sexo y región
tasas_avad_reg <- import("datos_limpios/arg_tasas_est_reg.rds")

## Tasas de AVAD, AVP y AVD ajustadas por edad según sexo y provincia
tasas_avad_prov <- import("datos_limpios/arg_tasas_est_prov.rds")

## Población estándar Censo 2010
pob_est_2010 <- import("datos_limpios/pob_est_2010.rds")


# Gráficos exploratorios --------------------------------------------------

# Escala de colores:
"#555448" #varones
"#4c9061" #mujeres
"#bc7247" #AVAD
"#4a96d8" #AVD
"#af9e1f" #AVP

## Recuento absoluto de AVAD, AVP y AVD a nivel nacional ----
# avad_arg_est <- tasas_avad_arg %>% 
#   
#   # Uno población estándar por sexo
#   left_join(pob_est_2010 %>% 
#               group_by(sexo) %>% 
#               summarise(pob_est_2010 = sum(pob_est_2010)), by = join_by(sexo)) %>% 
#   
#   # Calculo recuento absoluto de cada indicador a partir de la tasa estandarizada
#   mutate(
#     across(
#       .cols = starts_with(c("AVAD", "AVD", "AVP")),
#       .fns = ~ (.x * pob_est_2010 / 100000),
#       .names = "{.col}_abs")
#       ) %>% 
#   
#   select(c(1:2, 13:21))
#       
# export(avad_arg_est, file = "datos_limpios/arg_abs_est.xlsx")

avad_arg %>% 
  group_by(anio_enfr) %>% 
  summarise(AVAD_total = sum(AVAD))

(583691*100/329547)-100

avad_arg %>% 
  group_by(anio_enfr, sexo) %>% 
  summarise(AVAD_total = sum(AVAD))

#mujeres
(248489*100/155947)-100
#varones
(335202*100/173600)-100

avd_ind %>% 
  group_by(anio_enfr, sexo, comp_qualidiab) %>% 
  summarise(AVD_ind = sum(AVD)) %>% view()
  
  ggplot(aes(x = anio_enfr, y = AVD_ind)) +
  geom_bar(stat = "identity") +
  
  facet_wrap(~sexo + comp_qualidiab, ncol = 4)

## Tasas ajustadas: Tendencia nacional por año y sexo ----
tasas_avad_arg |> 
  select(anio_enfr, sexo, AVD_tasa_std, AVP_tasa_std, AVAD_tasa_std) %>% 
  mutate(sexo = if_else(sexo == "Mujer", "Mujeres", "Varones")) %>% 
  pivot_longer(cols = c(AVD_tasa_std, AVP_tasa_std, AVAD_tasa_std), names_to = "indicador", values_to = "tasas") %>% 
  
  ggplot(aes(x = anio_enfr, y = tasas, colour = indicador, group = indicador)) +
  geom_line() +
  geom_point() +
  
  # Dividir por sexo
  facet_grid(cols = vars(sexo)) +
  
  # Etiquetas ejes
  labs(x = "Año", y = "Tasa ajustada (c/100.000 hab)", colour = "") +
  
  # Tema
  theme_minimal() +
  # theme(axis.text.x = element_text(angle = 90)) +
  # scale_x_continuous(breaks = c(2005, 2009, 2013, 2018),
  #                    labels = c("2005", "2009", "2013", "2018")) +
  # scale_y_continuous(
  # labels = label_number(
  #   big.mark = ",",     # separador de miles
  #   decimal.mark = "."
  # )) +
  
  scale_colour_manual(values = c("#bc7247",
                                 "#4a96d8",
                                 "#af9e1f"),
                      labels = c("AVAD_tasa_std" = "AVAD",
                                 "AVD_tasa_std" = "AVD",
                                 "AVP_tasa_std" = "AVP"))

## Recuento absoluto: Tendencia nacional por año y sexo ----
# tasas_nac_plot_abs |>
#   group_by(anio_enfr, sexo, indicador_abs) %>% 
#   summarise(valor_sum = sum(valor_abs)) %>% 
#   
#   ggplot(aes(x = anio_enfr, y = valor_sum, colour = indicador_abs, group = indicador_abs)) +
#   geom_line() +
#   geom_point() +
#   
#   # Dividir por sexo
#   facet_grid(cols = vars(sexo)) +
#   
#   # Etiquetas ejes
#   labs(x = "Año", y = "Recuento", colour = "") +
#   
#   # Tema
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   scale_x_continuous(limits = c(2004.8, 2018.2), expand = c(0.01,0),
#                      breaks = c(2005, 2009, 2013, 2018)) +
#   scale_y_continuous(
#     labels = label_number(
#       big.mark = ".",     # separador de miles
#       decimal.mark = ","
#     )) +
#   scale_colour_manual(values = c("#756429",
#                                  "#297567",
#                                  "#732975"
#                                  ),
#                       labels = c("AVAD_abs" = "AVAD",
#                                  "AVD_abs" = "AVD",
#                                  "AVP_abs" = "AVP"))


## Pirámide por sexo y grupo edad decenal: tasa AVAD general ----
avad_arg |> 
  # Pirámide
  age_pyramid(age_group = grupo_edad_10,
              split_by = sexo,
              stack_by = sexo,
              count = AVAD_tasa,
              show_midpoint = FALSE) +
  
  # Dividir por año ENFR
  facet_wrap(~ anio_enfr) +
  
  # Colores
  scale_fill_manual(
    values = c(
      "Varón" = "#555448",
      "Mujer" = "#4c9061"),
    labels = c("Mujer" = "Mujeres",
               "Varón" = "Varones")) +
  
  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Tasa AVAD (c/100.000 hab)",
       fill = "")+
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(expand = c(0.2, 0))


## Pirámide por sexo y grupo edad decenal: mostrando valores relativos de AVAD ----
avad_arg |> 
  group_by(anio_enfr) %>% 
  mutate(AVAD_prop = AVAD/(sum(AVAD))) %>% 
  
  # Pirámide
  age_pyramid(age_group = grupo_edad_10,
              split_by = sexo,
              stack_by = sexo,
              count = AVAD_prop,
              proportional = T,
              show_midpoint = FALSE) +
  
  # Dividir por año ENFR
  facet_wrap(~ anio_enfr) +
  
  # Colores
  scale_fill_manual(
    values = c(
      "Varón" = "#555448",
      "Mujer" = "#4c9061"),
    labels = c("Mujer" = "Mujeres",
               "Varón" = "Varones")) +
  
  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Proporción de AVAD (%)", fill = "")+
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(expand = c(0.2, 0))



## Pirámide por sexo y grupo edad decenal: mostrando proporción AVD/AVP ----
avad_arg |> 
  
  group_by(anio_enfr) %>% 
  mutate(AVD_prop = AVD/(sum(AVAD)),
         AVP_prop = AVP/(sum(AVAD))) %>% 
  
  pivot_longer(cols = c(AVD_prop, AVP_prop),
               names_to = "indicador",
               values_to = "valor") %>% 
  #mutate(sexo_indicador = interaction(sexo, indicador, sep = " - ")) %>% 
  
  # Pirámide
  age_pyramid(age_group = grupo_edad_10,
              split_by = sexo,
              stack_by = indicador,
              count = valor,
              #fill_by = sexo_indicador,
              show_midpoint = FALSE) +
  
  # Dividir por año ENFR
  facet_wrap(~ anio_enfr) +
  
  # Colores
  scale_fill_manual(
    values = c(
      "AVD_prop" = "#4a96d8",
      "AVP_prop" = "#af9e1f"),
    labels = c("AVD_prop" = "AVD",
               "AVP_prop" = "AVP")) +
  
  # scale_fill_scico_d(palette = "hawaii") +
  
  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Proporción de AVAD (%)", fill = "")+
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(expand = c(0.2, 0))

## Gráficos por región ----

### Ridge plot por región ----
tasas_avad_reg %>% 
  ggplot(aes(x = AVAD_tasa_std, 
             y = factor(anio_enfr), 
             fill = factor(anio_enfr))) +
  
  geom_density_ridges(alpha = 0.7, color = "white", scale = 1.2) +
  labs(
    title = "Distribución de tasas ajustadas de AVAD regionales por sexo y año",
    x = "Tasa ajustada de AVAD (c/100.000 hab)",
    y = "Año",
    fill = "Año"
  ) +
  
  facet_wrap(~ sexo) +
  theme_minimal() +
  theme(legend.position = "none")


### Decomposition plot ----
tasas_avad_reg %>% 
  ggplot(aes(x = AVP_tasa_std, 
             y = AVD_tasa_std, 
             size = AVAD_tasa_std, 
             color = region_deis)) +
  geom_point(alpha = 0.7) +
  
  geom_vline(data = tasas_avad_arg,
             aes(xintercept = AVP_tasa_std), 
             linetype = 2) +
  geom_hline(data = tasas_avad_arg,
             aes(yintercept = AVD_tasa_std), 
             linetype = 2) +
  
  scale_size_continuous(name = "Tasa ajustada de AVAD") +
  labs(
    title = "Descomposición de AVAD por año",
    x = "Tasa ajustada de AVP (c/100.000 hab)",
    y = "Tasa ajustada de AVD (c/100.000 hab)",
    color = "Región"
  ) +
  
  facet_wrap(nrow = 2, ncol = 4, vars(sexo, anio_enfr)) 



### Ranking de tasas ajustadas de AVAD por región y sexo ----

# 1) Calcular ranking por año
df_rank_reg <- tasas_avad_reg %>%
  filter(!is.na(AVAD_tasa_std)) %>%
  group_by(anio_enfr, sexo) %>%
  arrange(desc(AVAD_tasa_std)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# 2) Resaltar primeras 10
top_reg <- df_rank_reg %>%
  group_by(sexo) %>%
  filter(anio_enfr == max(anio_enfr)) %>%
  slice_min(rank, n = 1) %>%
  ungroup() %>%
  select(sexo, region_deis)

df_rank_reg <- df_rank_reg %>%
  left_join(top_reg %>% mutate(destacar = TRUE),
            by = c("sexo", "region_deis")) %>%
  mutate(destacar = if_else(is.na(destacar), FALSE, destacar),
         sexo = case_when(sexo == "Mujer" ~ "Mujeres",
                          sexo == "Varón" ~ "Varones")) 


# 3) Gráfico estilo bump chart
df_rank_reg %>% 
  
  # Opción resaltando primera
  ggplot(aes(x = anio_enfr, y = rank, group = region_deis)) +
  geom_line(aes(color = destacar), linewidth = 1) +
  geom_point(aes(color = destacar)) +
  geom_text(data = df_rank_reg %>% filter(anio_enfr == max(anio_enfr) & rank < 6),
            aes(label = region_deis),
            hjust = -0.1,
            size = 3) +
  scale_color_manual(values = c("grey80", "#bc7247")) +
  coord_cartesian(clip = "off") +
  
  # Invertir eje y para que el rank = 1 esté arriba
  scale_y_reverse(breaks = 1:max(df_rank_reg$rank)) +
  
  labs(
    title = "Ranking de tasa ajustada de AVAD por región según sexo",
    x = "Año",
    y = "Ranking",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  ) +
  
  facet_wrap(~sexo, nrow=2)


### Tasas AVAD, AVD y AVP ajustadas: Tendencia por región, año y sexo ----
tasas_avad_reg %>% 
  mutate(sexo = case_when(sexo == "Mujer" ~ "Mujeres",
                          sexo == "Varón" ~ "Varones")) %>% 
  
  pivot_longer(cols = c(AVAD_tasa_std, AVD_tasa_std, AVP_tasa_std), names_to = "indicador", values_to = "tasas") %>% 
  
  ggplot(aes(x = anio_enfr, y = tasas, color = indicador, group = indicador)) +
  geom_line() +
  geom_point() +
  
  # Dividir por región y sexo
  facet_grid(rows = vars(sexo), cols = vars(region_deis), switch = "y") +
  
  # Colores
  scale_color_manual(
    values = c(
      "AVAD_tasa_std" = "#bc7247",
      "AVD_tasa_std" = "#4a96d8",
      "AVP_tasa_std" = "#af9e1f"),
    labels = c(
      "AVAD_tasa_std" = "AVAD",
      "AVD_tasa_std" = "AVD",
      "AVP_tasa_std" = "AVP")) +
  
  # Etiquetas ejes
  labs(x = "Año", 
       y = "Tasa ajustada (c/100.000 hab)",
       color = "") +
  
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        axis.text.y = element_text(size = 8))


### Proporción de AVD/AVP por año, región y sexo ----
avad_reg %>%
  mutate(anio_enfr = as.factor(anio_enfr),
         sexo = case_when(sexo == "Mujer" ~ "Mujeres",
                          sexo == "Varón" ~ "Varones")) %>% 
  
  group_by(anio_enfr, region_deis, sexo) %>% 
  summarise(AVP_prop = round(sum(AVP)/sum(AVAD)*100, digits = 2),
            AVD_prop = round(sum(AVD)/sum(AVAD)*100, digits = 2)) %>% 
  
  pivot_longer(cols = c(AVD_prop, AVP_prop), names_to = "indicador", values_to = "prop") %>% 
  
  ggplot(aes(x = anio_enfr, y = prop, fill = indicador)) +
  geom_bar(stat = "identity", position = "fill") +
  # scale_x_continuous(limits = c(2003, 2020), expand = c(0.01,0),
  #                    breaks = c(2005, 2009, 2013, 2018)) +
  # 
  geom_hline(yintercept = 0.5) +
  scale_x_discrete() +
  scale_y_continuous(labels = scales::label_percent()) +
  
  
  # Dividir por región y sexo
  facet_grid(rows = vars(sexo), cols = vars(region_deis), switch = "y") +
  
  # Colores
  scale_fill_manual(
    values = c(
      "AVD_prop" = "#4a96d8",
      "AVP_prop" = "#af9e1f"),
    labels = c(
      "AVD_prop" = "AVD",
      "AVP_prop" = "AVP")) +
  
  # Etiquetas ejes
  labs(x = "Año", 
       y = "Proporción (%)",
       fill = "") +
  
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        axis.text.y = element_text(size = 8))

## Gráficos por provincia ----

### Ridge plot por provincia ----
tasas_avad_prov %>% 
  ggplot(aes(x = AVAD_tasa_std, 
             y = factor(anio_enfr), 
             fill = factor(anio_enfr))) +
  
  geom_density_ridges(alpha = 0.7, color = "white", scale = 1.2) +
  labs(
    title = "Distribución de tasas ajustadas de AVAD provinciales por sexo y año",
    x = "Tasa ajustada de AVAD (c/100.000 hab)",
    y = "Año",
    fill = "Año"
  ) +
  
  facet_wrap(~ sexo) +
  theme_minimal() +
  theme(legend.position = "none")


### Ranking de tasas ajustadas de AVAD por provincia y sexo ----

# 1) Calcular ranking por año
df_rank <- tasas_avad_prov %>%
  filter(!is.na(AVAD_tasa_std)) %>%
  group_by(anio_enfr, sexo) %>%
  arrange(desc(AVAD_tasa_std)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# 2) Resaltar primeras 10
top_provs <- df_rank %>%
  group_by(sexo) %>%
  filter(anio_enfr == max(anio_enfr)) %>%
  slice_min(rank, n = 5) %>%
  ungroup() %>%
  select(sexo, prov_nombre)

df_rank <- df_rank %>%
  left_join(top_provs %>% mutate(destacar = TRUE),
            by = c("sexo", "prov_nombre")) %>%
  mutate(destacar = if_else(is.na(destacar), FALSE, destacar),
         sexo = case_when(sexo == "Mujer" ~ "Mujeres",
                          sexo == "Varón" ~ "Varones")) 



# 3) Gráfico estilo bump chart
df_rank %>% 
  
  # Opción resaltando primeras 10
  ggplot(aes(x = anio_enfr, y = rank, group = prov_nombre)) +
  geom_line(aes(color = destacar), linewidth = 1) +
  geom_point(aes(color = destacar), size = 1) +
  geom_text(data = df_rank %>% filter(anio_enfr == max(anio_enfr) & rank < 6),
            aes(label = prov_nombre),
            hjust = -0.1,
            size = 3) +
  scale_color_manual(values = c("grey80", "#bc7247")) +
  coord_cartesian(clip = "off") +
  
  # Invertir eje y para que el rank = 1 esté arriba
  scale_y_reverse(breaks = 1:max(df_rank$rank)) +
  
  labs(
    title = "Ranking de tasa ajustada de AVAD por provincia según sexo",
    x = "Año",
    y = "Ranking",
    color = "Provincia"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8)
  ) +
  
  facet_wrap(~sexo, nrow=2)
  
  
### Otras opciones de rankings ----
# Centro
tasas_prov_ge10_aj %>% 
  filter(sexo == "Mujer") %>% 
  
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "Centro"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "Centro" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("Centro") +
  theme_minimal()

# NOA
tasas_prov_ge10_aj %>% 
  filter(sexo == "Mujer") %>% 
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "NOA"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "NOA" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("NOA") +
  theme_minimal()

# NEA
tasas_prov_ge10_aj %>% 
  filter(sexo == "Mujer") %>% 
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "NEA"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "NEA" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("NEA") +
  theme_minimal()

# Cuyo
tasas_prov_ge10_aj %>% 
  filter(sexo == "Mujer") %>% 
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "Cuyo"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "Cuyo" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("Cuyo") +
  theme_minimal()

# Sur
tasas_prov_ge10_aj %>% 
  filter(sexo == "Mujer") %>% 
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "Sur"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "Sur" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("Sur") +
  theme_minimal()



## Varones
# Centro
tasas_prov_ge10_aj %>% 
  filter(sexo == "Varón") %>% 
  
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "Centro"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "Centro" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("Centro") +
  theme_minimal()

# NOA
tasas_prov_ge10_aj %>% 
  filter(sexo == "Varón") %>% 
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "NOA"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "NOA" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("NOA") +
  theme_minimal()

# NEA
tasas_prov_ge10_aj %>% 
  filter(sexo == "Varón") %>% 
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "NEA"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "NEA" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("NEA") +
  theme_minimal()

# Cuyo
tasas_prov_ge10_aj %>% 
  filter(sexo == "Varón") %>% 
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "Cuyo"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "Cuyo" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("Cuyo") +
  theme_minimal()

# Sur
tasas_prov_ge10_aj %>% 
  filter(sexo == "Varón") %>% 
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +
  
  # Gráfico de región
  geom_point(aes(color = prov_nombre), size = 2, 
             data = ~. |> filter(region == "Sur"),
             show.legend = F) +
  
  geom_text_repel(aes(label = prov_nombre),
                  data = tasas_prov_ge10_aj |> 
                    filter(region == "Sur" & anio_enfr == 2018 & sexo == "Mujer"),
                  nudge_x = 0.5,
                  direction = "y",
                  size = 3) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(limits = c(2004.8, 2020), expand = c(0.01,0),
                     breaks = c(2005, 2009, 2013, 2018)) +
  scale_y_reverse(breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
                  labels = number_format(suffix = ".")) +
  labs(x = "Año",
       y = "Tasa ajustada de AVAD (c/100.000 hab)"
  ) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  ggtitle("Sur") +
  theme_minimal()


### Tasas AVAD, AVD y AVP ajustadas: Tendencia por provincia y año, mujeres ----
tasas_avad_prov |> 
  filter(sexo == "Mujer") %>% 
  pivot_longer(cols = c(AVAD_tasa_std, AVD_tasa_std, AVP_tasa_std), names_to = "indicador", values_to = "tasas") %>% 
  
  ggplot(aes(x = anio_enfr, y = tasas, color = indicador, group = indicador)) +
  geom_line() +
  geom_point() +
  
  # Dividir por provincia
  facet_wrap(~ prov_nombre, ncol = 6) +
  
  # Colores
  scale_color_manual(
    values = c(
      "AVAD_tasa_std" = "#bc7247",
      "AVD_tasa_std" = "#4a96d8",
      "AVP_tasa_std" = "#af9e1f"),
    labels = c(
      "AVAD_tasa_std" = "AVAD",
      "AVD_tasa_std" = "AVD",
      "AVP_tasa_std" = "AVP")) +
  
  # Etiquetas ejes
  labs(x = "Año", 
       y = "Tasa ajustada (c/100.000 hab)",
       color = "") +
  
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        axis.text.y = element_text(size = 8)) +
  ggtitle("Mujeres")


### Tasas AVD y AVP ajustadas: Tendencia por provincia y año, varones ----
tasas_avad_prov |> 
  filter(sexo == "Varón") %>% 
  pivot_longer(cols = c(AVAD_tasa_std, AVD_tasa_std, AVP_tasa_std), names_to = "indicador", values_to = "tasas") %>% 
  
  ggplot(aes(x = anio_enfr, y = tasas, color = indicador, group = indicador)) +
  geom_line() +
  geom_point() +
  
  # Dividir por provincia
  facet_wrap(~ prov_nombre, ncol = 6) +
  
  # Colores
  scale_color_manual(
    values = c(
      "AVAD_tasa_std" = "#bc7247",
      "AVD_tasa_std" = "#4a96d8",
      "AVP_tasa_std" = "#af9e1f"),
    labels = c(
      "AVAD_tasa_std" = "AVAD",
      "AVD_tasa_std" = "AVD",
      "AVP_tasa_std" = "AVP")) +
  
  # Etiquetas ejes
  labs(x = "Año", 
       y = "Tasa ajustada (c/100.000 hab)",
       color = "") +
  
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8),
        axis.text.y = element_text(size = 8)) +
  ggtitle("Varones")

