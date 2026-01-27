### Cálculo de AVP, AVD y AVAD para diabetes mellitus tipo 2 (DM2) en Argentina
### para los periodos correspondientes a las cuatro Encuestas Nacionales de
### Factores de Riesgo (2005, 2009, 2013 y 2018).
### Autoras: Micaela Gauto y Tamara Ricardo

### Fecha modificación:
# 2026-01-02

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  apyramid,
  gghighlight,
  scico,
  patchwork,
  #ggbump,
  scales,
  MetBrewer,
  ggrepel,
  tidyverse
)


# Cargar datos ------------------------------------------------------------
## AVAD por sexo, grupo etario decenal y provincia
avad_ge10_prov <- import("datos_limpios/arg_AVAD_ge10_prov.xlsx")

## AVAD por sexo, grupo etario decenal y región
avad_ge10_reg <- import("datos_limpios/arg_AVAD_ge10_reg.xlsx")

## Población estándar 2010
pob_est_2010 <- import("datos_limpios/arg_pob_est_2010.rds")


# Cálculo de tasas --------------------------------------------------------
## Tasa nacional general por año ENFR, sexo y grupo etario decenal
tasas_nac_ge10 <- avad_ge10_prov |>

  tasas_nac_ge10 <- AVAD_ge10 |>
  group_by(anio_enfr, grupo_edad_10, sexo) |>
  summarise(
    across(
      .cols = c(AVD, AVP, AVAD),
      .fns = list(
        abs = ~ sum(.x),
        tasa_gral = ~ round(sum(.x) / sum(proy_pob) * 100000, 2)
      )
    ),
    .groups = "drop"
  )

## Tasa nacional por año ENFR y sexo, ajustada por edad
tasas_nac_ge10_aj <- AVAD_ge10 %>%

  # Calculo recuentos estandarizados para tasa ajustada
  mutate(across(
    .cols = c(AVD, AVP, AVAD),
    .fns = list(
      n_est = ~ (.x / proy_pob * pob_est_2010) # recuentos estandarizados
    )
  )) |>

  # Calculo valores absolutos de cada indicador para población estándar
  group_by(anio_enfr, sexo) %>%
  summarise(across(
    .cols = c(AVAD_n_est, AVP_n_est, AVD_n_est),
    .fns = list(
      abs = ~ sum(.x)
    ),
    .names = "{gsub('_n_est$', '', .col)}_est_sum"
  )) %>%

  # Calculo y uno población 2010 nacional
  left_join(
    proy_ge10 %>%
      filter(anio == 2005 & prov_id == 2) %>% # como la población estándar se repite por cada año y prov, filtro para quedarme sin duplicados
      group_by(sexo) %>%
      summarise(pob_nac_2010 = sum(pob_est_2010)),
    by = join_by(sexo == sexo)
  ) %>%

  # Calculo tasa ajustada por edad, según año y sexo
  group_by(anio_enfr, sexo) %>%
  mutate(across(
    .cols = c(AVAD_est_sum, AVP_est_sum, AVD_est_sum),
    .fns = list(
      tasa_aj = ~ round(.x / pob_nac_2010 * 100000, 2)
    ),
    .names = "{gsub('_est_sum$', '', .col)}_{.fn}"
  ))


## Tasas provinciales por año ENFR y sexo, ajustadas por edad
tasas_prov_ge10_aj <- AVAD_ge10 %>%

  # Calculo recuentos estandarizados para tasas ajustadas
  mutate(across(
    .cols = c(AVD, AVP, AVAD),
    .fns = list(
      n_est = ~ (.x / proy_pob * pob_est_2010) # recuentos estandarizados
    )
  )) |>

  # Calculo numerador (recuento absoluto) de cada indicador para población estándar
  group_by(anio_enfr, prov_id, prov_nombre, sexo) %>%
  summarise(across(
    .cols = c(AVAD_n_est, AVP_n_est, AVD_n_est),
    .fns = list(
      abs = ~ sum(.x)
    ),
    .names = "{gsub('_n_est$', '', .col)}_est_sum"
  )) %>%

  # Calculo y uno población 2010 nacional (denominador)
  left_join(
    proy_ge10 %>%
      filter(anio == 2005 & prov_id == 2) %>% # como la población estándar se repite por cada año y prov, filtro para quedarme sin duplicados
      group_by(sexo) %>%
      summarise(pob_nac_2010 = sum(pob_est_2010)),
    by = join_by(sexo == sexo)
  ) %>%

  # Calculo tasa ajustada por edad, según año, provincia y sexo
  group_by(anio_enfr, prov_id, prov_nombre, sexo) %>%
  mutate(across(
    .cols = c(AVAD_est_sum, AVP_est_sum, AVD_est_sum),
    .fns = list(
      tasa_aj = ~ round(.x / pob_nac_2010 * 100000, 2)
    ),
    .names = "{gsub('_est_sum$', '', .col)}_{.fn}"
  ))


## Tasas regionales por año ENFR y sexo, ajustadas por edad
tasas_reg_ge10_aj <- AVAD_ge10_reg %>%

  # Calculo recuentos estandarizados para tasas ajustadas
  mutate(across(
    .cols = c(AVD, AVP, AVAD),
    .fns = list(
      n_est = ~ (.x / proy_pob * pob_est_2010) # recuentos estandarizados
    )
  )) |>

  # Calculo numerador (recuento absoluto) de cada indicador para población estándar
  group_by(anio_enfr, region, sexo) %>%
  summarise(across(
    .cols = c(AVAD_n_est, AVP_n_est, AVD_n_est),
    .fns = list(
      abs = ~ sum(.x)
    ),
    .names = "{gsub('_n_est$', '', .col)}_est_sum"
  )) %>%

  # Calculo y uno población 2010 nacional (denominador)
  left_join(
    proy_ge10_reg %>%
      filter(anio == 2005 & region == "Centro") %>% # como la población estándar se repite por cada año y región, filtro para quedarme sin duplicados
      group_by(sexo) %>%
      summarise(pob_nac_2010 = sum(pob_est_2010)),
    by = join_by(sexo == sexo)
  ) %>%

  # Calculo tasa ajustada por edad, según año, región y sexo
  group_by(anio_enfr, region, sexo) %>%
  mutate(across(
    .cols = c(AVAD_est_sum, AVP_est_sum, AVD_est_sum),
    .fns = list(
      tasa_aj = ~ round(.x / pob_nac_2010 * 100000, 2)
    ),
    .names = "{gsub('_est_sum$', '', .col)}_{.fn}"
  ))


# Gráficos exploratorios --------------------------------------------------
# Escala de colores:
"#A0BBF2" #varones
"#D94423" #mujeres
"#F2BA52" #AVAD
"#F2B3B3" #AVD
"#B0D9BA" #AVP

## Tasas ajustadas: Tendencia nacional por año y sexo ----
tasas_nac_ge10_aj |>
  pivot_longer(
    cols = c(AVD_tasa_aj, AVP_tasa_aj, AVAD_tasa_aj),
    names_to = "indicador",
    values_to = "tasas"
  ) %>%

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
  scale_x_continuous(
    breaks = c(2005, 2009, 2013, 2018),
    labels = c("2005", "2009", "2013", "2018")
  ) +
  scale_y_continuous(
    labels = label_number(
      big.mark = ".", # separador de miles
      decimal.mark = ","
    )
  ) +

  scale_colour_manual(
    values = c("#F2BA52", "#F2B3B3", "#B0D9BA"),
    labels = c(
      "AVAD_tasa_aj" = "AVAD",
      "AVD_tasa_aj" = "AVD",
      "AVP_tasa_aj" = "AVP"
    )
  )

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
tasas_nac_ge10 |>
  # Pirámide
  age_pyramid(
    age_group = grupo_edad_10,
    split_by = sexo,
    stack_by = sexo,
    count = AVAD_tasa_gral,
    show_midpoint = FALSE
  ) +

  # Dividir por año ENFR
  facet_wrap(~anio_enfr) +

  # Colores
  scale_fill_manual(
    values = c(
      "Varón" = "#A0BBF2",
      "Mujer" = "#D94423"
    ),
    labels = c("Mujer" = "Mujeres", "Varón" = "Varones")
  ) +

  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Tasa AVAD (c/100.000 hab)", fill = "") +
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


## Pirámide por sexo y grupo edad decenal: mostrando valores relativos de AVAD ----
tasas_nac_ge10 |>
  group_by(anio_enfr) %>%
  mutate(AVAD_prop = AVAD_abs / (sum(AVAD_abs))) %>%

  # Pirámide
  age_pyramid(
    age_group = grupo_edad_10,
    split_by = sexo,
    stack_by = sexo,
    count = AVAD_prop,
    proportional = T,
    show_midpoint = FALSE
  ) +

  # Dividir por año ENFR
  facet_wrap(~anio_enfr) +

  # Colores
  scale_fill_manual(
    values = c(
      "Varón" = "#A0BBF2",
      "Mujer" = "#D94423"
    ),
    labels = c("Mujer" = "Mujeres", "Varón" = "Varones")
  ) +

  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Proporción de AVAD (%)", fill = "") +
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


## Pirámide por sexo y grupo edad decenal: mostrando tasas AVD/AVP ----
tasas_nac_ge10 |>
  pivot_longer(
    cols = c(AVD_tasa_gral, AVP_tasa_gral),
    names_to = "indicador",
    values_to = "valor"
  ) %>%
  #mutate(sexo_indicador = interaction(sexo, indicador, sep = " - ")) %>%

  # Pirámide
  age_pyramid(
    age_group = grupo_edad_10,
    split_by = sexo,
    stack_by = indicador,
    count = valor,
    #fill_by = sexo_indicador,
    show_midpoint = FALSE
  ) +

  # Dividir por año ENFR
  facet_wrap(~anio_enfr) +

  # Colores
  scale_fill_manual(
    values = c(
      "AVD_tasa_gral" = "#F2B3B3",
      "AVP_tasa_gral" = "#B0D9BA"
    ),
    labels = c("AVD_tasa_gral" = "AVD", "AVP_tasa_gral" = "AVP")
  ) +

  # scale_fill_scico_d(palette = "hawaii") +

  # Etiquetas ejes
  labs(x = "Grupo etario", y = "Tasa (c/100.000 hab)", fill = "") +
  # Tema
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


## Otra opción de pirámide mostrando AVD y AVP ----
# tasas_nac_plot_abs <- tasas_nac_ge10 %>%
#   pivot_longer(cols = c(AVD_abs, AVP_abs, AVAD_abs),
#                names_to = "indicador_abs",
#                values_to = "valor_abs") %>%
#   mutate(valor_plot_abs = ifelse(sexo == "Varón", -valor_abs, valor_abs))
#
# tasas_nac_plot_perc <- tasas_nac_ge10 %>%
#   mutate(across(.cols = c(AVP_abs, AVD_abs),
#                 .fns = list(
#                   perc = ~ round(.x / AVAD_abs, 2)),
#                 .names = "{gsub('_abs$', '', .col)}_{.fn}")) %>%
#   select(anio_enfr, grupo_edad_10, sexo, AVP_perc, AVD_perc) %>%
#   pivot_longer(cols = c(AVD_perc, AVP_perc),
#                names_to = "indicador_perc",
#                values_to = "valor_perc") %>%
#   mutate(valor_plot_perc = ifelse(sexo == "Varón", -valor_perc, valor_perc))
#
# # Pirámide con valores absolutos
# tasas_nac_plot_abs %>%
#   filter(indicador_abs != "AVAD_abs") %>%
#   mutate(sexo_indicador = interaction(indicador_abs, sexo, sep = " - ")) %>%
#
#   ggplot() +
#
#   # pirámide de AVD y AVP
#   geom_col(
#     mapping = aes(x = grupo_edad_10, y = valor_plot_abs, fill = sexo_indicador),
#     colour = "black",  # color negro alrededor de las barras
#     alpha = 1,
#     width = 1) +
#
#   coord_flip() +
#   facet_wrap(~anio_enfr) +
#   scale_fill_manual(
#     values = c(
#       "AVD_abs - Varón" = "#297567",
#       "AVP_abs - Varón" = "#8ACABF",
#       "AVD_abs - Mujer" = "#732975",
#       "AVP_abs - Mujer" = "#C88ACA"),
#     labels = c(
#       "AVD_abs - Varón"  = "Varones · AVD",
#       "AVP_abs - Varón"  = "Varones · AVP",
#       "AVD_abs - Mujer"  = "Mujeres · AVD",
#       "AVP_abs - Mujer"  = "Mujeres · AVP")) +
#   labs(x = "Grupos de edad",
#        y = "Valor absoluto",
#        fill = "") +
#   theme_minimal()
#
# # Pirámide con valores relativos
# tasas_nac_plot_perc %>%
#   mutate(sexo_indicador = interaction(indicador_perc, sexo, sep = " - ")) %>%
#   ggplot() +
#
#   # pirámide de AVD y AVP
#   geom_col(
#     mapping = aes(x = grupo_edad_10, y = valor_plot_perc, fill = sexo_indicador),
#     colour = "black",  # color negro alrededor de las barras
#     alpha = 1,
#     width = 1) +
#
#   coord_flip() +
#   facet_wrap(~anio_enfr) +
#   scale_fill_manual(
#     values = c(
#       "AVD_perc - Varón" = "#297567",
#       "AVP_perc - Varón" = "#8ACABF",
#       "AVD_perc - Mujer" = "#732975",
#       "AVP_perc - Mujer" = "#C88ACA"),
#     labels = c(
#       "AVD_perc - Varón"  = "Varones · AVD",
#       "AVP_perc - Varón"  = "Varones · AVP",
#       "AVD_perc - Mujer"  = "Mujeres · AVD",
#       "AVP_perc - Mujer"  = "Mujeres · AVP")) +
#   labs(x = "Grupos de edad",
#        y = "Proporción (%)",
#        fill = "") +
#   theme_minimal()

## Gráficos por provincia ----
### Ranking de tasas ajustadas de AVAD por provincia y sexo ----

# 1) Calcular ranking por año
df_rank <- tasas_prov_ge10_aj %>%
  filter(!is.na(AVAD_tasa_aj)) %>%
  group_by(anio_enfr, sexo) %>%
  arrange(desc(AVAD_tasa_aj)) %>%
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
  left_join(
    top_provs %>% mutate(destacar = TRUE),
    by = c("sexo", "prov_nombre")
  ) %>%
  mutate(destacar = if_else(is.na(destacar), FALSE, destacar))


# 3) Gráfico estilo bump chart
df_rank %>%

  # Opción con color por provincia
  # ggplot(aes(x = anio_enfr, y = rank, group = prov_nombre, color = prov_nombre)) +
  # geom_line(size = 1) +
  # geom_point(size = 3) +
  # geom_text(
  #   data = df_rank %>% filter(anio_enfr == max(anio_enfr)),
  #   aes(label = prov_nombre),
  #   hjust = -0.1,
  #   size = 3
  # ) +

  # Opción resaltando primeras 10
  ggplot(aes(x = anio_enfr, y = rank, group = prov_nombre)) +
  geom_line(aes(color = destacar), size = 1) +
  geom_point(aes(color = destacar), size = 1) +
  geom_text(
    data = df_rank %>% filter(anio_enfr == max(anio_enfr) & rank < 6),
    aes(label = prov_nombre),
    hjust = -0.1,
    size = 3
  ) +
  scale_color_manual(values = c("grey80", "#F2BA52")) +
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

  facet_wrap(~sexo, nrow = 2) +

  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  )


### Otras opciones de rankings ----
# Centro
tasas_prov_ge10_aj %>%
  filter(sexo == "Mujer") %>%

  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +

  # Gráfico de región
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "Centro"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "Centro" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  ggtitle("Centro") +
  theme_minimal()

# NOA
tasas_prov_ge10_aj %>%
  filter(sexo == "Mujer") %>%
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +

  # Gráfico de región
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "NOA"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "NOA" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +

  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  ggtitle("NOA") +
  theme_minimal()

# NEA
tasas_prov_ge10_aj %>%
  filter(sexo == "Mujer") %>%
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +

  # Gráfico de región
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "NEA"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "NEA" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  ggtitle("NEA") +
  theme_minimal()

# Cuyo
tasas_prov_ge10_aj %>%
  filter(sexo == "Mujer") %>%
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +

  # Gráfico de región
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "Cuyo"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "Cuyo" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  ggtitle("Cuyo") +
  theme_minimal()

# Sur
tasas_prov_ge10_aj %>%
  filter(sexo == "Mujer") %>%
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +

  # Gráfico de región
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "Sur"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "Sur" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
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
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "Centro"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "Centro" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  ggtitle("Centro") +
  theme_minimal()

# NOA
tasas_prov_ge10_aj %>%
  filter(sexo == "Varón") %>%
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +

  # Gráfico de región
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "NOA"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "NOA" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +

  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  ggtitle("NOA") +
  theme_minimal()

# NEA
tasas_prov_ge10_aj %>%
  filter(sexo == "Varón") %>%
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +

  # Gráfico de región
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "NEA"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "NEA" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  ggtitle("NEA") +
  theme_minimal()

# Cuyo
tasas_prov_ge10_aj %>%
  filter(sexo == "Varón") %>%
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +

  # Gráfico de región
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "Cuyo"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "Cuyo" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  ggtitle("Cuyo") +
  theme_minimal()

# Sur
tasas_prov_ge10_aj %>%
  filter(sexo == "Varón") %>%
  # Gráfico base
  ggplot(aes(x = anio_enfr, y = AVAD_tasa_aj)) +
  geom_point(color = "gray90", size = 2) +

  # Gráfico de región
  geom_point(
    aes(color = prov_nombre),
    size = 2,
    data = ~ . |> filter(region == "Sur"),
    show.legend = F
  ) +

  geom_text_repel(
    aes(label = prov_nombre),
    data = tasas_prov_ge10_aj |>
      filter(region == "Sur" & anio_enfr == 2018 & sexo == "Mujer"),
    nudge_x = 0.5,
    direction = "y",
    size = 3
  ) +
  scale_color_manual(values = met.brewer("Juarez")) +
  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +
  scale_y_reverse(
    breaks = c(25, 20, 15, 10, 5, 1),
    expand = c(0.02, 0),
    labels = number_format(suffix = ".")
  ) +
  labs(x = "Año", y = "Tasa ajustada de AVAD (c/100.000 hab)") +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8)
  ) +
  ggtitle("Sur") +
  theme_minimal()


### Tasas AVAD, AVD y AVP ajustadas: Tendencia por provincia y año, mujeres ----
tasas_prov_ge10_aj |>
  filter(sexo == "Mujer") %>%
  pivot_longer(
    cols = c(AVAD_tasa_aj, AVD_tasa_aj, AVP_tasa_aj),
    names_to = "indicador",
    values_to = "tasas"
  ) %>%

  ggplot(aes(x = anio_enfr, y = tasas, color = indicador)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    limits = c(2004.8, 2018.2),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +

  # Dividir por provincia
  facet_wrap(~prov_nombre, ncol = 6) +

  # Colores
  scale_color_manual(
    values = c(
      "AVAD_tasa_aj" = "#F2BA52",
      "AVD_tasa_aj" = "#F2B3B3",
      "AVP_tasa_aj" = "#B0D9BA"
    ),
    labels = c(
      "AVAD_tasa_aj" = "AVAD",
      "AVD_tasa_aj" = "AVD",
      "AVP_tasa_aj" = "AVP"
    )
  ) +

  # Etiquetas ejes
  labs(x = "Provincia", y = "Tasa ajustada (c/100.000 hab)", color = "") +

  # Tema
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  ) +
  ggtitle("Mujeres")


### Tasas AVD y AVP ajustadas: Tendencia por provincia y año, varones ----
tasas_prov_ge10_aj |>
  filter(sexo == "Varón") %>%
  pivot_longer(
    cols = c(AVAD_tasa_aj, AVD_tasa_aj, AVP_tasa_aj),
    names_to = "indicador",
    values_to = "tasas"
  ) %>%

  ggplot(aes(x = anio_enfr, y = tasas, color = indicador)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    limits = c(2004.8, 2018.2),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +

  # Dividir por provincia
  facet_wrap(~prov_nombre, ncol = 6) +

  # Colores
  scale_color_manual(
    values = c(
      "AVAD_tasa_aj" = "#F2BA52",
      "AVD_tasa_aj" = "#F2B3B3",
      "AVP_tasa_aj" = "#B0D9BA"
    ),
    labels = c(
      "AVAD_tasa_aj" = "AVAD",
      "AVD_tasa_aj" = "AVD",
      "AVP_tasa_aj" = "AVP"
    )
  ) +

  # Etiquetas ejes
  labs(x = "Año", y = "Tasa ajustada (c/100.000 hab)", color = "") +

  # Tema
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  ) +
  ggtitle("Varones")


## Gráficos por región ----
### Ranking de tasas ajustadas de AVAD por provincia y sexo ----

# 1) Calcular ranking por año
df_rank_reg <- tasas_reg_ge10_aj %>%
  filter(!is.na(AVAD_tasa_aj)) %>%
  group_by(anio_enfr, sexo) %>%
  arrange(desc(AVAD_tasa_aj)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# 2) Resaltar primeras 10
top_reg <- df_rank_reg %>%
  group_by(sexo) %>%
  filter(anio_enfr == max(anio_enfr)) %>%
  slice_min(rank, n = 1) %>%
  ungroup() %>%
  select(sexo, region)

df_rank_reg <- df_rank_reg %>%
  left_join(top_reg %>% mutate(destacar = TRUE), by = c("sexo", "region")) %>%
  mutate(
    destacar = if_else(is.na(destacar), FALSE, destacar),
    sexo = case_when(sexo == "Mujer" ~ "Mujeres", sexo == "Varón" ~ "Varones")
  )


# 3) Gráfico estilo bump chart
df_rank_reg %>%

  # Opción con color por región
  # ggplot(aes(x = anio_enfr, y = rank, group = region, color = region)) +
  # geom_line(size = 1) +
  # geom_point(size = 3) +
  # geom_text(
  #   data = df_rank_reg %>% filter(anio_enfr == max(anio_enfr)),
  #   aes(label = region),
  #   hjust = -0.1,
  #   size = 3
  # ) +

  # Opción resaltando primera
  ggplot(aes(x = anio_enfr, y = rank, group = region)) +
  geom_line(aes(color = destacar), linewidth = 1) +
  geom_point(aes(color = destacar)) +
  geom_text(
    data = df_rank_reg %>% filter(anio_enfr == max(anio_enfr) & rank < 6),
    aes(label = region),
    hjust = -0.1,
    size = 3
  ) +
  scale_color_manual(values = c("grey80", "#F2BA52")) +
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

  facet_wrap(~sexo, nrow = 2) +

  scale_x_continuous(
    limits = c(2004.8, 2020),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  )


## Tasas AVAD, AVD y AVP ajustadas: Tendencia por región, año y sexo ----
tasas_reg_ge10_aj %>%
  mutate(
    sexo = case_when(sexo == "Mujer" ~ "Mujeres", sexo == "Varón" ~ "Varones")
  ) %>%

  pivot_longer(
    cols = c(AVAD_tasa_aj, AVD_tasa_aj, AVP_tasa_aj),
    names_to = "indicador",
    values_to = "tasas"
  ) %>%

  ggplot(aes(x = anio_enfr, y = tasas, color = indicador)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    limits = c(2004.8, 2018.2),
    expand = c(0.01, 0),
    breaks = c(2005, 2009, 2013, 2018)
  ) +

  # Dividir por región y sexo
  facet_grid(rows = vars(sexo), cols = vars(region), switch = "y") +

  # Colores
  scale_color_manual(
    values = c(
      "AVAD_tasa_aj" = "#F2BA52",
      "AVD_tasa_aj" = "#F2B3B3",
      "AVP_tasa_aj" = "#B0D9BA"
    ),
    labels = c(
      "AVAD_tasa_aj" = "AVAD",
      "AVD_tasa_aj" = "AVD",
      "AVP_tasa_aj" = "AVP"
    )
  ) +

  # Etiquetas ejes
  labs(x = "Año", y = "Tasa ajustada (c/100.000 hab)", color = "") +

  # Tema
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )


## Proporción de AVD/AVP por año, región y sexo
AVAD_ge10_reg %>%
  mutate(
    anio_enfr = as.factor(anio_enfr),
    sexo = case_when(sexo == "Mujer" ~ "Mujeres", sexo == "Varón" ~ "Varones")
  ) %>%

  group_by(anio_enfr, region, sexo) %>%
  summarise(
    AVP_prop = round(AVP / AVAD * 100, digits = 2),
    AVD_prop = round(AVD / AVAD * 100, digits = 2)
  ) %>%

  pivot_longer(
    cols = c(AVD_prop, AVP_prop),
    names_to = "indicador",
    values_to = "prop"
  ) %>%

  ggplot(aes(x = anio_enfr, y = prop, fill = indicador)) +
  geom_bar(stat = "identity", position = "fill") +
  # scale_x_continuous(limits = c(2003, 2020), expand = c(0.01,0),
  #                    breaks = c(2005, 2009, 2013, 2018)) +
  #
  scale_x_discrete() +
  scale_y_continuous(labels = scales::label_percent()) +

  # Dividir por región y sexo
  facet_grid(rows = vars(sexo), cols = vars(region), switch = "y") +

  # Colores
  scale_fill_manual(
    values = c(
      "AVD_prop" = "#F2B3B3",
      "AVP_prop" = "#B0D9BA"
    ),
    labels = c(
      "AVD_prop" = "AVD",
      "AVP_prop" = "AVP"
    )
  ) +

  # Etiquetas ejes
  labs(x = "Año", y = "Proporción (%)", fill = "") +

  # Tema
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, size = 8),
    axis.text.y = element_text(size = 8)
  )

# # Calcular intervalos de incertidumbre ------------------------------------
# ## Simular disability weights con distribución normal truncada
# dw_sim <- comp_dm  |>
#   mutate(sim_id = 1) |>
#   uncount(weights = 1000, .id = "sim_id") |>
#   mutate(dw_sim = rnorm(n(), mean = dw, sd = (upper - lower) / 3.92),
#          dw_sim = pmin(pmax(dw_sim, 0), 1)) |>
#   # Ponderar
#   mutate(weighted_dw = dw_sim * frec_wandurranga) |>
#
#   # Calcular DW ponderada total por simulación
#   group_by(sim_id) |>
#   summarise(dw_total_sim = sum(weighted_dw),
#             .groups = "drop")
#
#
# ## AVAD con intervalo de incertidumbre
# avad_sim <- prev_dm_10 |>
#   # Corrección mínima para prevalencias 0
#   mutate(dm_prev = if_else(dm_prev == 0, 1e-6, dm_prev)) |>
#
#   # Simular prevalencia
#   mutate(sim_id = 1) |>
#   uncount(weights = 1000, .id = "sim_id") |>
#   mutate(dm_prev_sim = rlnorm(n(),
#                               meanlog = log(dm_prev),
#                               sdlog = dm_prev_se / dm_prev)) |>
#
#   # Calcular la prevalencia media simulada
#   group_by(sim_id, anio_enfr, prov_nombre, grupo_edad_10, sexo) |>
#   summarise(dm_prev_sim = mean(dm_prev_sim, na.rm = TRUE),
#             .groups = "drop") |>
#
#   # Añadir disability weights simulados
#   left_join(dw_sim) |>
#   # Calcular AVD simulados
#   mutate(AVD_sim = dm_prev_sim * dw_total_sim) |>
#
#   # Añadir AVP
#   left_join(AVP_10) |>
#
#   # Calcular AVAD simulados
#   mutate(AVAD_sim = AVD_sim + AVP) |>
#
#   # Obtener AVAD y AVD con intervalo de incertidumbre
#   group_by(anio_enfr, prov_id, prov_nombre, grupo_edad_10, sexo) |>
#   summarise(across(.cols = c(AVP, AVD_sim, AVAD_sim),
#                    .fns = ~ list(mean(.x),
#                                  quantile(.x, 0.025),
#                                  quantile(.x, 0.075))))
