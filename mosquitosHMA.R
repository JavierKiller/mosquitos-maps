library(sf)
library(leaflet)
library(dplyr)
library(lubridate)
library(readxl)
library(htmlwidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)

library(tidyverse)
library(gganimate)
library(RColorBrewer)
library(gifski)
library(forcats)

library(gganimate)
library(gifski)



# Especifica el camino al archivo Excel
file_path <- "mosquitos 2019-2023.xlsx"

# Cargar una hoja específica
df <- read_excel(file_path, sheet = "Hoja1")
tail(df)
df <- df %>%
  mutate(
    fecha_colecta_str = as.character(`Fecha de colecta`), # Asegurarse que es texto
    fecha_colecta_fmt = paste0("20", fecha_colecta_str),  # Agregar el siglo (asume 2000+)
    fecha_colecta_date = ymd(fecha_colecta_fmt)           # Convertir a Date
  )
#titulos de variables
variable_names <- names(df)
print(variable_names)

# Especifica el camino al archivo Excel 2024
file_path24 <- "Base DEMA 2024.xlsx"

# Cargar hoja específica DEMA
df24 <- read_excel(file_path24, sheet = "DEMA")
head(df24)
df24 <- df24 %>%
  mutate(
    fecha_colecta_str = as.character(`Fecha de colecta`), # Asegurarse que es texto
    fecha_colecta_fmt = paste0("20", fecha_colecta_str),  # Agregar el siglo (asume 2000+)
    fecha_colecta_date = ymd(fecha_colecta_fmt)           # Convertir a Date
  )
#titulos de variables

variable_n24 <- names(df24)
#print(variable_n24)


# Reemplazar valores NA con el valor anterior
df24 <- df24 %>%
  tidyr::fill(everything(), .direction = "down")%>%
  mutate(Sector = as.character(Sector)) %>%
  mutate(Manzana = as.character(Manzana)) %>%
  mutate(Jurisdicción = as.double(Jurisdicción)) %>%
  dplyr::select(all_of(variable_names))  # Selecciona solo las columnas necesarias


# Especifica el camino al archivo Excel 2025
file_path25 <- "Base DEMA 2025.xlsx"

# Cargar hoja específica DEMA
df25 <- read_excel(file_path25, sheet = "DEMA")
head(df25)

df25 <- df25 %>%
  mutate(
    fecha_colecta_str = as.character(`Fecha de colecta`), # Asegurarse que es texto
    fecha_colecta_fmt = paste0("20", fecha_colecta_str),  # Agregar el siglo (asume 2000+)
    fecha_colecta_date = ymd(fecha_colecta_fmt)           # Convertir a Date
  )
#titulos de variables
variable_n25 <- names(df25)
print(variable_n25)

# Reemplazar valores NA con el valor anterior
df25 <- df25 %>%
  tidyr::fill(everything(), .direction = "down")%>%
  mutate(Sector = as.character(Sector)) %>%
  mutate(Manzana = as.character(Manzana)) %>%
  mutate(`Fecha de colecta` = as.double(`Fecha de colecta`)) %>%
  mutate(Jurisdicción = as.double(Jurisdicción)) %>%
  dplyr::select(all_of(variable_names))  # Selecciona solo las columnas necesarias

df_full <- bind_rows(df, df24, df25)
df_full

df_hillo <- df_full %>%
  filter(`Municipio y Localidad` == "Hermosillo")

tail(df_hillo)
# homologacion de Diagnostico
df_hillo <- df_hillo %>%
  mutate(
    Diagnóstico = str_squish(str_to_lower(Diagnóstico)),  # quita espacios dobles y pone en minúsculas
    Diagnóstico = case_when(
      Diagnóstico %in% c("culex quinquefasciatus", "culex quinQuefasciatus") ~ "Culex quinquefasciatus",
      Diagnóstico %in% c("culex tarsalis", "culex Tarsalis") ~ "Culex tarsalis",
      Diagnóstico %in% c("culex sp.", "culex sp") ~ "Culex sp.",
      Diagnóstico %in% c("psorophora connfinnis", "psorophora confinnis") ~ "Psorophora confinnis",
      TRUE ~ str_to_sentence(Diagnóstico)  # pone en formato capitalizado (tipo nombre científico)
    )
  )


#filtrar datos de diagnosticos inesesarios 
df_hillosm <- df_hillo %>%
  filter(!`Diagnóstico` %in% c("Macho",
                          "Fam. sin imp. méd.",
                          "Fam. sin Imp. Méd.",
                          "Negativo",
                          "MUESTRA RECHAZADA",
                          "0")
         )

tail(df_hillosm)

#sort(unique(df_hillosm$Diagnóstico))


#fecha_colecta_date

# Graficas de cada año de especies incidencia de especies de mosquitos a lo largo de los meses


df_plot <- df_hillosm %>%
  mutate(
    año = year(fecha_colecta_date),
    mes = month(fecha_colecta_date, label = TRUE, abbr = TRUE)  # mes con nombre abreviado
  ) %>%
  group_by(año, mes, Diagnóstico ) %>%
  summarise(total_individuos = sum(Individuos, na.rm = TRUE), .groups = "drop")  # suma por grupo

# Graficar con ggplot
ggplot(df_plot, aes(x = mes, y = total_individuos, color = Diagnóstico , group = Diagnóstico )) +
  geom_line(size = 1) +
  facet_wrap(~ año, ncol = 2) +
  labs(
    title = "Abundancia mensual por especie de mosquito (2019–2025)",
    x = "Mes",
    y = "Cantidad de individuos",
    color = "Especie (Diagnóstico)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Crear una lista de años únicos
años <- unique(df_plot$año)
# 
# # Crear un gráfico por cada año y guardarlo como imagen
for (a in años) {
  df_anual <- df_plot %>% filter(año == a)
  
  # Calcular totales por especie
  especie_totales <- df_anual %>%
    group_by(Diagnóstico) %>%
    summarise(total = sum(total_individuos), .groups = "drop") %>%
    mutate(label = paste0(Diagnóstico, " (n=", total, ")"))
  
  etiquetas <- setNames(especie_totales$label, especie_totales$Diagnóstico)
  
  # Añadir etiquetas y limpieza
  df_anual <- df_anual %>%
    mutate(
      Diagnóstico_label = etiquetas[Diagnóstico],
      total_individuos = ifelse(total_individuos == 0, NA, total_individuos)
    )
  
  # Reordenar por abundancia
  df_anual <- df_anual %>%
    mutate(
      Diagnóstico_label = fct_reorder(Diagnóstico_label, total_individuos, .fun = sum, .desc = TRUE)
    )
  
  # Crear paleta para las etiquetas de este año
  niveles_labels <- levels(df_anual$Diagnóstico_label)
  
  colores_labels <- setNames(
    sapply(niveles_labels, function(label) {
      especie <- sub(" \\(n=.*", "", label)
      if (especie %in% names(colores_fijos)) {
        colores_fijos[especie]
      } else {
        colores_extra_index <- match(especie, especies_restantes)
        colores_extra[colores_extra_index]
      }
    }),
    niveles_labels
  )
  
  # Gráfico estático
  p <- ggplot(df_anual, aes(x = mes, y = total_individuos, color = Diagnóstico_label, group = Diagnóstico_label)) +
    geom_point(size = 3, alpha = 0.8, na.rm = TRUE) +
    geom_line(alpha = 0.5, na.rm = TRUE) +
    scale_color_manual(values = colores_labels) +
    labs(
      title = paste("Abundancia mensual por especie de mosquito -", a),
      x = "Mes",
      y = "Cantidad de individuos",
      color = "Especie (total individuos)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  # Guardar imagen
  ggsave(filename = paste0("abundancia_mosquitos_", a, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}
# 
# #Nuevo grafico 
# for (a in años) {
#   df_anual <- df_plot %>% filter(año == a)
#   
#   # Paso 1: Calcular totales por especie
#   especie_totales <- df_anual %>%
#     group_by(Diagnóstico) %>%
#     summarise(total = sum(total_individuos), .groups = "drop") %>%
#     mutate(label = paste0(Diagnóstico, " (n=", total, ")"))
#   
#   # Paso 2: Crear una tabla de reemplazo
#   etiquetas <- setNames(especie_totales$label, especie_totales$Diagnóstico)
#   
#   # Paso 3: Aplicar las etiquetas al dataframe anual
#   df_anual <- df_anual %>%
#     mutate(Diagnóstico_label = etiquetas[Diagnóstico])
#   
#   # Graficar usando la nueva variable con etiqueta
#   p <- ggplot(df_anual, aes(x = mes, y = total_individuos, color = Diagnóstico_label)) +
#     geom_point(size = 3, alpha = 0.8) +
#     geom_line(aes(group = Diagnóstico), alpha = 0.5) +
#     labs(
#       title = paste("Abundancia mensual por especie de mosquito -", a),
#       x = "Mes",
#       y = "Cantidad de individuos",
#       color = "Especie (total individuos)"
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       plot.title = element_text(size = 14, face = "bold")
#     )
#   
#   # Guardar imagen
#   ggsave(filename = paste0("abundancia_mosquitos_", a, ".png"), plot = p, width = 10, height = 6, dpi = 300)
# }
#
###### Generando gif
#

for (a in años) {
  df_anual <- df_plot %>% filter(año == a)

  # Paso 1: Calcular totales por especie
  especie_totales <- df_anual %>%
    group_by(Diagnóstico) %>%
    summarise(total = sum(total_individuos), .groups = "drop") %>%
    mutate(label = paste0(Diagnóstico, " (n=", total, ")"))

  # Paso 2: Crear una tabla de reemplazo
  etiquetas <- setNames(especie_totales$label, especie_totales$Diagnóstico)

  # Paso 3: Aplicar las etiquetas al dataframe anual
  df_anual <- df_anual %>%
    mutate(
      Diagnóstico_label = etiquetas[Diagnóstico],
      mes_num = as.numeric(mes)  # Para usar en transition_reveal
    )

  # Gráfico con transición acumulativa
  p_anim <- ggplot(df_anual, aes(x = mes_num, y = total_individuos, color = Diagnóstico_label, group = Diagnóstico_label)) +
    geom_line(size = 1, alpha = 0.6) +
    geom_point(size = 3, alpha = 0.8) +
    scale_x_continuous(
      breaks = 1:12,
      labels = month.abb
    ) +
    labs(
      title = paste("Abundancia mensual acumulada por especie -", a),
      subtitle = 'Mes: {frame_along}',
      x = "Mes",
      y = "Cantidad de individuos",
      color = "Especie (total individuos)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    transition_reveal(mes_num)

  # Guardar animación como GIF
  anim_save(
    filename = paste0("abundancia_A_mosquitos_", a, ".gif"),
    animation = animate(p_anim, width = 1000, height = 600, fps = 3, duration = 12, renderer = gifski_renderer())
  )
}

### Nuevo gif 

# Paleta base fija
colores_fijos <- c(
  "Aedes aegypti" = "black",
  "Culex quinquefasciatus" = "red",
  "Anopheles pseudopunctipennis" = "forestgreen"
)

# Lista total de especies
todas_las_especies <- sort(unique(df_hillosm$Diagnóstico))

# Especies no asignadas todavía
especies_restantes <- setdiff(todas_las_especies, names(colores_fijos))

# Colores adicionales
colores_extra <- RColorBrewer::brewer.pal(n = max(length(especies_restantes), 8), name = "Dark2")
if (length(especies_restantes) > length(colores_extra)) {
  colores_extra <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)][1:length(especies_restantes)]
}

# Bucle por año
for (a in años) {
  df_anual <- df_plot %>% filter(año == a)
  
  # Totales por especie
  especie_totales <- df_anual %>%
    group_by(Diagnóstico) %>%
    summarise(total = sum(total_individuos), .groups = "drop") %>%
    mutate(label = paste0(Diagnóstico, " (n=", total, ")"))
  
  etiquetas <- setNames(especie_totales$label, especie_totales$Diagnóstico)
  
  # Etiquetas y limpieza
  df_anual <- df_anual %>%
    mutate(
      Diagnóstico_label = etiquetas[Diagnóstico],
      mes_num = as.numeric(mes),
      total_individuos = ifelse(total_individuos == 0, NA, total_individuos)
    )
  
  # Ordenar por abundancia
  df_anual <- df_anual %>%
    mutate(
      Diagnóstico_label = fct_reorder(Diagnóstico_label, total_individuos, .fun = sum, .desc = TRUE)
    )
  
  # Crear paleta exacta según niveles actuales
  niveles_labels <- levels(df_anual$Diagnóstico_label)
  
  colores_labels <- setNames(
    sapply(niveles_labels, function(label) {
      especie <- sub(" \\(n=.*", "", label)
      if (especie %in% names(colores_fijos)) {
        colores_fijos[especie]
      } else {
        colores_extra_index <- match(especie, especies_restantes)
        colores_extra[colores_extra_index]
      }
    }),
    niveles_labels
  )
  
  # Gráfico animado
  p_anim <- ggplot(df_anual, aes(x = mes_num, y = total_individuos, color = Diagnóstico_label, group = Diagnóstico_label)) +
    geom_line(size = 1.2, alpha = 0.6, na.rm = TRUE) +
    geom_point(size = 3, alpha = 0.8, na.rm = TRUE) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_color_manual(values = colores_labels) +
    labs(
      title = paste("Abundancia mensual acumulada por especie -", a),
      subtitle = 'Mes: {frame_along}',
      x = "Mes",
      y = "Cantidad de individuos",
      color = "Especie (total individuos)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    transition_reveal(mes_num)
  
  # Guardar animación
  anim_save(
    filename = paste0("abundancia_mosquitos_", a, ".gif"),
    animation = animate(p_anim, width = 1000, height = 600, fps = 3, duration = 12, renderer = gifski_renderer())
  )
}