### Cargar y limpiar datos ---- 
# source("02_codigo/cargar_limpiar_datos.R")

### Gráfica del número acumulado de CI iniciadas por año ----
bd_cdmx %>% 
  arrange(fecha_hechos) %>% 
  group_by(ano) %>% 
  mutate(dia_ano = yday(fecha_hechos)) %>% 
  ungroup() %>% 
  group_by(ano, dia_ano, categoria_de_delito) %>% 
  summarise(num_carpetas_diarias = n()) %>% 
  ungroup() %>% 
  group_by(ano, categoria_de_delito) %>% 
  mutate(num_acumulado_carpetas = cumsum(num_carpetas_diarias), 
         etiqueta_19 = ifelse(dia_ano == max(dia_ano) & ano == 2019, ano, ""),
         etiqueta_dif_19 = ifelse(dia_ano == max(dia_ano) & ano != 2019, ano, ""),
         etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2016" & str_detect(categoria_de_delito, "microbus"), "", etiqueta_dif_19),
         etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2017" & str_detect(categoria_de_delito, "arma"), "\n2017", etiqueta_dif_19),
         etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2017" & str_detect(categoria_de_delito, "cuenta"), "\n2017", etiqueta_dif_19),
         etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2016" & str_detect(categoria_de_delito, "negocio"), "\n2016", etiqueta_dif_19),
         etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2016" & str_detect(categoria_de_delito, "no d"), "\n2016", etiqueta_dif_19),
         etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2017" & str_detect(categoria_de_delito, "vehículo"), "", etiqueta_dif_19),
         etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2018" & str_detect(categoria_de_delito, "vehículo"), "\n2018", etiqueta_dif_19),
         color_linea = ifelse(ano == 2019, "sí", "no")) %>%
  ungroup() %>% 
  ggplot(aes(dia_ano, num_acumulado_carpetas, group = ano, color = color_linea)) +
  geom_line(size = 1) +
  geom_text_repel(aes(label = etiqueta_19), hjust = -0.15, color = "grey40", fontface = "bold", size = 5) +
  geom_text(aes(label = etiqueta_dif_19), color = "grey70", fontface = "bold", size = 3.5, hjust = -0.17) +
  facet_wrap(~ str_wrap(str_to_upper(categoria_de_delito), width = 30), scales = "free_y") +
  scale_x_continuous(breaks = c(1, seq(45, 360, 45)), limits = c(0, 390)) +
  scale_y_continuous(label = comma) +
  scale_color_manual(values = c("grey70", "#a50300")) +
  labs(title = str_wrap(str_to_upper("número de carpetas de investigación acumulado diariamente en la CDMX, por categoría de delito"), width = 125),
       subtitle = "Datos del 1 enero de 2016 al 31 de mayo de 2019. Los datos NO son comparables entre categorías de delito.",
       x = "\nDías del año transcurridos",
       y = "Número\n", 
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: ADIP y PGJCDMX.") +
  tema +
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 20),
        axis.title = element_text(family = "Trebuchet MS Bold", size = 20),
        axis.text = element_text(size = 15),
        strip.text = element_text(family = "Trebuchet MS Bold"),
        legend.position = "none") +
  ggsave("03_graficas/num_acumulado_ci_por_categoria.png", width = 20, height = 14, dpi = 200)


### Cambio porcentaul en el número de CI iniciadas en los primeros cinco meses de 2019 vs. el promedio de los primeros cinco meses de 206-2018 ----
bd_cdmx %>% 
  filter(mes %in% c("Enero", "Febrero", "Marzo", "Abril", "Mayo")) %>% 
  group_by(ano, categoria_de_delito) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>% 
  arrange(categoria_de_delito, ano) %>% 
  group_by(categoria_de_delito) %>%
  mutate(dummy_anio = ifelse(ano != 2019, "2016-2018", "2019")) %>% 
  ungroup() %>% 
  group_by(categoria_de_delito, dummy_anio) %>% 
  summarise(promedio_ci = mean(num_ci)) %>% 
  ungroup() %>% 
  group_by(categoria_de_delito) %>%
  mutate(cambio_porcentual = round(((promedio_ci - lag(promedio_ci))/lag(promedio_ci))*100, 1)) %>% 
  ungroup() %>% 
  mutate(etiqueta_pos_grandes = ifelse(cambio_porcentual > 30, str_c(comma(cambio_porcentual), "%", sep = ""), ""),
         etiqueta_pos_pequenios = ifelse(cambio_porcentual < 30 & cambio_porcentual >= 0, str_c(cambio_porcentual, "%", sep = ""), ""),
         etiqueta_neg_grandes = ifelse(cambio_porcentual < -10, str_c(cambio_porcentual, "%", sep = ""), ""),
         etiqueta_neg_pequenios = ifelse(cambio_porcentual < 0 & cambio_porcentual > -10, str_c(cambio_porcentual, "%", sep = ""), "")) %>% 
  ungroup() %>% 
  filter(dummy_anio == 2019) %>% 
  ggplot(aes(fct_reorder(str_wrap(categoria_de_delito, width = 25), cambio_porcentual), cambio_porcentual)) +
  geom_col(fill = "#a50300") +
  geom_text(aes(label = etiqueta_pos_grandes), color = "white", size = 5, fontface = "bold", hjust = 1.1) +
  geom_text(aes(label = etiqueta_pos_pequenios), color = "grey50", size = 5, fontface = "bold", hjust = -0.2) +
  geom_text(aes(label = etiqueta_neg_grandes), color = "white", size = 5, fontface = "bold", hjust = -0.2) +
  geom_text(aes(label = etiqueta_neg_pequenios), color = "grey50", size = 5, fontface = "bold", hjust = 1.1) +
  coord_flip() +
  labs(title = str_wrap(str_to_upper("cambio porcentual del número de carpetas de investigación en los primeros cinco meses de 2019 vs. el promedio del mismo período en 2016, 2017 y 2018"), width = 60),
       subtitle = "La gráfica muestra el cambio porcentual en el número de carpetas de investigación acumuladas entre enero y mayo de\n2019, respecto al PROMEDIO de carpetas acumuladas en el mismo periodo en los tres años previos.",
       x = "",
       y = "\nCambio porcentual",
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: ADIP y PGJCDMX.") +
  tema +
  theme(plot.title = element_text(size = 24),
        plot.caption = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 18)) +
  ggsave("03_graficas/cambio_porcentual_ci_5_meses_2019_vs_promedio_5_meses_2016_2018_por_categoria.png", width = 14, height = 15, dpi = 200)


### Número de carpetas de investigación iniciadas mensualmente en la CDMX, por categoría de delito ----
bd_cdmx %>% 
  arrange(fecha_hechos) %>% 
  mutate(fecha_techo_mes = ceiling_date(fecha_hechos, unit = "month") - 1) %>% 
  select(fecha_hechos, fecha_techo_mes, categoria_de_delito) %>% 
  group_by(fecha_techo_mes, categoria_de_delito) %>% 
  summarise(num_ci = n()) %>% 
  ungroup() %>%  
  mutate(año = year(fecha_techo_mes), 
         mes = month(fecha_techo_mes)) %>% 
  filter(año != 2015) %>% 
  left_join(datos_poblacion_cdmx, by = c("año" = "ano")) %>% 
  mutate(tasa_x_100k = (num_ci/pob_tot)*100000*12) %>% 
  mutate(color_linea = ifelse(año == 2019, "sí", "no")) %>%
  group_by(año) %>% 
  mutate(etiqueta_19 = ifelse(fecha_techo_mes == max(fecha_techo_mes) & año == 2019, año, ""),
         etiqueta_dif_19 = ifelse(fecha_techo_mes == max(fecha_techo_mes) & año != 2019, año, ""),
         etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2018" & str_detect(categoria_de_delito, "transpor"), "2018\n\n", etiqueta_dif_19),
         # etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2017" & str_detect(categoria_de_delito, "transpor"), "2017", etiqueta_dif_19),
         etiqueta_dif_19 = ifelse(etiqueta_dif_19 == "2017" & str_detect(categoria_de_delito, "transpor"), "\n2017", etiqueta_dif_19)) %>% 
  ungroup() %>% 
  ggplot(aes(x = mes, y = num_ci, group = año, color = color_linea)) +
  geom_line(size = 1) +
  geom_text_repel(aes(label = etiqueta_19), hjust = -0.15, color = "grey40", fontface = "bold", size = 5) +
  geom_text(aes(label = etiqueta_dif_19), color = "grey60", fontface = "bold", size = 3.5, hjust = -0.17) +
  scale_x_continuous(limits = c(1, 12.5), breaks = 1:12, labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")) +
  scale_y_continuous(label = comma) +
  scale_color_manual(values = c("grey70", "#a50300")) +
  facet_wrap(~ str_wrap(str_to_upper(categoria_de_delito), width = 30), scales = "free_y") +
  labs(title = str_wrap(str_to_upper("número de carpetas de investigación iniciadas mensualmente en la CDMX, por categoría de delito"), width = 125),
       subtitle = "Datos del 1 enero de 2016 al 31 de mayo de 2019. Los datos NO son comparables entre categorías de delito.",
       x = "\n",
       y = "Número\n", 
       caption = "\nSebastián Garrido de Sierra / @segasi / Fuente: ADIP y PGJCDMX.") +
  tema +
  theme(plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 20),
        axis.title = element_text(family = "Trebuchet MS Bold", size = 20),
        axis.text = element_text(size = 15),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.text = element_text(family = "Trebuchet MS Bold"),
        legend.position = "none") +
  ggsave("03_graficas/num_mensual_ci_por_categoria.png", width = 20, height = 14, dpi = 200)

