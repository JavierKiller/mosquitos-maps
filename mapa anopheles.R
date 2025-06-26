library(sf)
library(leaflet)
library(leaflet.extras2)    # para brújula
library(leaflet.extras)     # para timeline slider
library(lubridate)
library(readxl)
library(htmlwidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(htmltools)
library(tidyverse)
library(gganimate)
library(RColorBrewer)
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


df_full <- df_full %>%
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
df_fullm <- df_full %>%
  filter(!`Diagnóstico` %in% c("Macho",
                               "Fam. sin imp. méd.",
                               "Fam. sin Imp. Méd.",
                               "Negativo",
                               "MUESTRA RECHAZADA",
                               "Muestra rechazada",
                               "0")
  )

df_fullm <- df_fullm %>%
  filter(
    `Diagnóstico` %in% c(
      "Anopheles pseudopunctipennis",
      "Anopheles albimanus",
      "Anopheles sp.",
      "Anopheles francicanus"
    )
  )


tail(df_fullm)

 #Filtrar datos con coordenadas válidas
 df_coords <- df_fullm %>%
   filter(!is.na(coord_x), !is.na(coord_y)) %>%
   st_as_sf(coords = c("coord_x", "coord_y"), crs = 4326)

shape_pathm <- "Sectormanzana/MANZANAS.shp"
shape_datam <- st_read(shape_pathm)
shape_datam <- st_transform(shape_datam, 4326)

shape_datam <- st_make_valid(shape_datam)

shape_datam <- shape_datam %>%
  rename(Sector = SECCION) %>%
  mutate(Sector = as.factor(Sector),
         MANZANA = as.factor(MANZANA),
         `Sector-manzana` = paste(Sector, MANZANA, sep = "-"))

 # Asignar a cada punto la manzana en que cae
 df_coords_joined <- st_join(df_coords, shape_datam, left = FALSE)  # Solo puntos que caen dentro de una manzana
 # Combinar ambos conjuntos de datos (los que ya tenían Sector-manzana + los asignados por coordenada)
 df_filtradom_coords <- df_coords_joined %>%
   st_drop_geometry()  # Quitamos geometría para unir como dataframe

# Asegurarnos de que df_filtrado tenga la columna 'Sector' como factor
df_filtrado <- df_fullm %>%
  mutate(Sector = as.factor(Sector)) %>% 
  mutate(Manzana = as.factor(Manzana))

# Crear variable Sector-manzana
df_filtradom <- df_filtrado %>%
  mutate(`Sector-manzana` = paste(Sector, Manzana, sep = "-"))

head(df_filtradom)

# # Combinar los datos originales y los asignados por GPS
 df_completo <- bind_rows(df_filtradom, df_filtradom_coords)
df_completo <- df_filtradom


# Realizar el right join
joined_data <- shape_datam %>%
  right_join(df_filtradom, by = "Sector-manzana") %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(año = year(fecha_colecta_date)) 

# jkfdjkfd
joined_data <- joined_data %>% filter(!st_is_empty(geometry))

# Asegurarnos de que las geometrías sean válidas
joined_data <- st_make_valid(joined_data)

# Convertir GEOMETRYCOLLECTION a POLYGON/MULTIPOLYGON si es necesario
joined_data <- st_cast(joined_data, "MULTIPOLYGON")


#geometría válida para shape de contornos
shape_contornos <- shape_datam %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")

# Crear columnas año y mes
joined_data <- joined_data %>%
  st_transform(4326) %>%
  mutate(
    año = year(fecha_colecta_date),
    mes = month(fecha_colecta_date),
    mes_label = month(fecha_colecta_date, label = TRUE, abbr = TRUE),
    time = as.numeric(mes) # para slider
  )

# Años disponibles
años <- sort(unique(joined_data$año))

# Datos agregados por sector-manzana, geometría, especie y fecha
datos_poligono <- joined_data %>%
  group_by(`Sector-manzana`, geometry, fecha_colecta_date, Diagnóstico) %>%
  summarise(total = sum(Individuos, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(`Sector-manzana`))

# Datos resumidos para cálculo de colores (total de individuos por polígono)
datos_resumen <- datos_poligono %>%
  group_by(`Sector-manzana`, geometry) %>%
  summarise(
    total_individuos = sum(total, na.rm = TRUE),
    num_especies = n_distinct(Diagnóstico),
    .groups = "drop"
  )

# Crear HTML para los popups
popup_info <- lapply(1:nrow(datos_resumen), function(i) {
  sector_manzana <- datos_resumen$`Sector-manzana`[i]
  total_ind <- datos_resumen$total_individuos[i]
  
  # Filtrar las filas que pertenecen a ese polígono
  datos_detalle <- datos_poligono %>%
    filter(`Sector-manzana` == sector_manzana) %>%
    arrange(fecha_colecta_date, Diagnóstico)
  
  # Construir la tabla como HTML
  tabla_html <- paste0(
    "<strong>Sector-manzana:</strong> ", sector_manzana,  "<br>",
    "<strong>No. de individuos:</strong> ", total_ind, "<br><br>",
    "<strong>Fecha : Especies - Individuos</strong><br>",
    paste0(
      format(datos_detalle$fecha_colecta_date, "%d/%m/%Y"), ": ",
      datos_detalle$Diagnóstico, " (", datos_detalle$total, ")",
      collapse = "<br>"
    )
  )
  
  HTML(tabla_html)
})

# Paleta de colores para intensidad
pal <- colorNumeric(palette = "YlOrRd", domain = datos_resumen$total_individuos)

icono_rosa <- makeIcon(
  iconUrl = "Imagen/compass-rose.png",
  iconWidth = 80,     # ajusta tamaño según necesites
  iconHeight = 80
)

# Crear mapa
mapa <- leaflet(datos_resumen) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng = -110.9747, lat = 29.0728, zoom = 11) %>%
  addPolygons(
    fillColor = ~pal(total_individuos),
    fillOpacity = 0.7,
    color = "black",
    weight = 2,
    label = popup_info,
    highlightOptions = highlightOptions(weight = 2, color = "blue", bringToFront = TRUE)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~total_individuos,
    title = "Total de Individuos",
    opacity = 1
  )

mapa <- mapa %>%
  addControl(
    html = '<img src="compass-rose.png" style="width:80px;height:80px;">',
    position = "topright"
  )

# Guardar como HTML
saveWidget(mapa, "mapas_interactivos/mapa_calor_anopheles.html", selfcontained = TRUE)


#otro mapa con filtros 
joined_data <- joined_data %>%
  mutate(año = lubridate::year(fecha_colecta_date))

años <- sort(unique(joined_data$año))

# Paleta con mejor contraste
pal <- colorNumeric(palette = "inferno", domain = datos_resumen$total_individuos)


#crear mapa 
mapa <- leaflet(datos_resumen) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng = -110.9747, lat = 29.0728, zoom = 11)

#agregar un loop
for (a in años) {
  datos_anio <- datos_poligono %>%
    filter(lubridate::year(fecha_colecta_date) == a) %>%
    group_by(`Sector-manzana`, geometry) %>%
    summarise(total_individuos = sum(total, na.rm = TRUE), .groups = "drop")
  
  mapa <- mapa %>%
    addPolygons(
      data = datos_anio,
      fillColor = ~pal(total_individuos),
      fillOpacity = 0.85,
      color = "#333333",
      weight = 2,
      label = lapply(
        paste0("Año: ", a, "<br>Total: ", datos_anio$total_individuos),
        htmltools::HTML
      ),
      group = paste0("Año ", a),
      highlightOptions = highlightOptions(
        weight = 3,
        color = "yellow",
        bringToFront = TRUE
      )
    )
}

#agregar control
mapa <- mapa %>%
  addLayersControl(
    overlayGroups = paste0("Año ", años),
    options = layersControlOptions(collapsed = FALSE)
  )

#

mapa <- mapa %>%
  addControl(
    html = '<img src="compass-rose.png" style="width:80px;height:80px;">',
    position = "topright"
  )

saveWidget(mapa, "mapas_interactivos/mapa_calor_anopheles_por_año.html", selfcontained = TRUE)

