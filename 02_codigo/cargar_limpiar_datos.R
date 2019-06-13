### Paquetes ----
library(pacman)
p_load(cowplot, extrafont, ggcal, ggrepel, grid, gridExtra, ineq, janitor, kableExtra, knitr, lubridate, readxl, rmarkdown, scales, sf, tidyverse, treemapify, wesanderson, zoo)

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

### Definir tema de gráficas ----
tema <- 
  theme_minimal() +
  theme(text = element_text(family = "Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), family = "Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family = "Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title.align = 0.5,
        axis.title = element_text(size = 14, hjust = 1, face = "bold", margin = margin(0,0,0,0)),
        axis.text = element_text(size = 12),
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 14))

### Importar datos de carpetas de investigación ----
bd <- read_delim("01_datos/adip/carpetas-de-investigacion-pgj-cdmx.csv", 
                 ";", 
                 col_types = cols(fecha_hechos = col_character(), 
                                  fecha_inicio = col_character()), 
                 escape_double = FALSE,
                 trim_ws = TRUE) %>% 
  clean_names() # "Limpiar" nombre de columnas

problems(bd)

### Corregir problemas con formatos en fechas ----
bd <- 
  bd %>% 
  mutate(fecha_hechos = parse_date_time(fecha_hechos, 
                                            orders = c("Ymd HMS", "d/m/y HM")),
         fecha_inicio = parse_date_time(fecha_inicio, 
                                            orders = c("Ymd HMS", "d/m/y HM")))

### Renombrar variables ----
bd <- 
  bd %>% 
  rename(ano = ano_hechos,
         mes = mes_hechos)

### Reordenar niveles de la variable mes ----
bd <- 
  bd %>% 
  mutate(mes = fct_relevel(mes, 
                           "Enero", "Febrero", "Marzo", "Abril",
                           "Mayo", "Junio", "Julio", "Agosto",
                           "Septiembre", "Octubre", "Noviembre", "Diciembre"))

### Cambiar valores de diversas variables a mayúscula (primera letra) y nminúsculas (resto de las letras) ----
bd <- 
  bd %>% 
  mutate(alcaldia = str_to_title(alcaldia_hechos),
         categoria_de_delito = str_to_sentence(categoria_delito),
         delito = str_to_sentence(delito))

### Generar variable para registrar en qué día de la semana ocurrió el hecho denunciado, y otra para registrar el día del año al que corresponde cada día calendario ----

# Tomamos dia_semana como proxy de la fecha en que ocurrió el presunto delito
bd <- 
  bd %>% 
  mutate(dia_semana = wday(fecha_inicio, 
                           week_start = getOption("lubridate.week.start", 1), # Especificar que semana empieza en lunes, no en domingo (el default)
                           locale = Sys.getlocale("LC_TIME")),
         dia_ano = yday(fecha_inicio)) 

### Generar versión en texto del día de la semana se abrió la carpeta de investigación ----
bd <- 
  bd %>% 
  mutate(dia_semana_texto = case_when(dia_semana == 1 ~ "Lun",
                                      dia_semana == 2 ~ "Mar",
                                      dia_semana == 3 ~ "Mié",
                                      dia_semana == 4 ~ "Jue",
                                      dia_semana == 5 ~ "Vie",
                                      dia_semana == 6 ~ "Sáb",
                                      dia_semana == 7 ~ "Dom"),
         dia_semana_texto = fct_relevel(dia_semana_texto, "Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom"))


### Generar dataframe con presuntos delitos cometidos en las alcaldías de la CDMX ----
bd_cdmx <- 
  bd %>% 
  filter(alcaldia %in% c("Alvaro Obregon", "Azcapotzalco", "Benito Juarez", "Coyoacan", "Cuajimalpa De Morelos", "Cuauhtemoc", "Gustavo A Madero", "Iztacalco", "Iztapalapa", "La Magdalena Contreras", "Miguel Hidalgo", "Milpa Alta", "Tlahuac", "Tlalpan", "Venustiano Carranza", "Xochimilco"))

### Corregir nombres de alcaldías de la CDMX ----
bd_cdmx <- 
  bd_cdmx %>% 
  mutate(alcaldia = case_when(alcaldia == "Alvaro Obregon" ~ "Álvaro Obregón",
                              alcaldia == "Benito Juarez" ~ "Benito Juárez",
                              alcaldia == "Coyoacan" ~ "Coyoacán",
                              alcaldia == "Cuajimalpa De Morelos" ~ "Cuajimalpa",
                              alcaldia == "Cuauhtemoc" ~ "Cuauhtémoc",
                              alcaldia == "Gustavo A Madero" ~ "Gustavo A. Madero",
                              alcaldia == "La Magdalena Contreras" ~ "Magdalena Contreras",
                              alcaldia == "Tlahuac" ~ "Tláhuac",
                              TRUE ~ alcaldia))

### Construir una versión de la fecha de inicio de las carpetas en texto ----
bd_cdmx <- 
  bd_cdmx %>% 
  mutate(dia = day(fecha_inicio), 
         fecha_texto = str_c(dia, "de", str_to_lower(mes), "de", ano, sep = " "))

### Agregar comas a algunos nombres de categorías de delito ----
bd_cdmx <- 
  bd_cdmx %>% 
  mutate(categoria_de_delito = str_replace(categoria_de_delito, " con y sin v", ", con y sin v"),
         categoria_de_delito = str_replace(categoria_de_delito, " con v", ", con v"))



### Filtrar datos para solo mantener CI de delitos con timestamp entre el 1 de enero de 2016 y el 31 de mayo de 2019 ----
bd_cdmx <- 
  bd_cdmx %>% 
  filter(ano > 2015, 
         fecha_inicio < as_datetime("2019-06-01 00:00:00"))