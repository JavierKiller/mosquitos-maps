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

library(ggplot2)
library(gganimate)


#library(tmap)



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
#variable_n24 <- names(df24)
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


# Ruta del archivo shape
shape_pathm <- "sectormanzana/MANZANAS.shp"

# Leer el archivo shape
shape_datam <- st_read(shape_pathm)
shape_datam <- st_transform(shape_datam, 4326)

# Cambiar el nombre de la columna SECCION a Sector
shape_datam <- shape_datam %>%
  rename(Sector = SECCION) %>% 
  mutate(Sector = as.factor(Sector)) %>% 
  mutate(MANZANA = as.factor(MANZANA))

# Crear variable sector-manzana
shape_datam <- shape_datam %>%
  mutate(`sector-manzana` = paste(Sector, MANZANA, sep = "-"))

head(shape_datam)

# Asegurarnos de que df_filtrado tenga la columna 'Sector' como factor
df_filtrado <- df_hillosm %>%
  mutate(Sector = as.factor(Sector)) %>% 
  mutate(Manzana = as.factor(Manzana))

# Crear variable sector-manzana
df_filtradom <- df_filtrado %>%
  mutate(`sector-manzana` = paste(Sector, Manzana, sep = "-"))

head(df_filtradom)

# Realizar el right join
joined_data <- shape_datam %>%
  right_join(df_filtradom, by = "sector-manzana") %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(año = year(fecha_colecta_date)) 


# joined_data <- shape_datam %>%
#   right_join(df_filtradom, by = "sector-manzana") %>%
#   group_by(`sector-manzana`) %>%
#   summarize(Total = sum(Individuos, na.rm = TRUE))

head(joined_data)

# Asegurarnos de que las geometrías sean válidas
joined_data <- st_make_valid(joined_data)

# Convertir GEOMETRYCOLLECTION a POLYGON/MULTIPOLYGON si es necesario
joined_data <- st_cast(joined_data, "MULTIPOLYGON")


#geometría válida para shape de contornos
shape_contornos <- shape_datam %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON")


# Asegura CRS y columna de año
joined_data <- joined_data %>%
  st_transform(4326) %>%
  mutate(año = year(fecha_colecta_date))

# Extrae años disponibles
años <- sort(unique(joined_data$año))

#bucle para crear mapas con titulo 

for (a in años) {
  
  datos_anio <- joined_data %>%
    filter(año == a) %>%
    group_by(`sector-manzana`, geometry) %>%
    summarise(
      total_individuos = sum(Individuos, na.rm = TRUE),
      num_especies = n_distinct(Diagnóstico),
      especies = paste(sort(unique(Diagnóstico)), collapse = ", "),
      .groups = "drop"
    )
  
  # Paleta de colores para abundancia
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = datos_anio$total_individuos
  )
  
  # Crear etiquetas emergentes
  etiquetas <- paste0(
    "<strong>Sector-Blokc: </strong>", datos_anio$`sector-manzana`, "<br>",
    "<strong>Total Individuals: </strong>", datos_anio$total_individuos, "<br>",
    "<strong>N° Different species: </strong>", datos_anio$num_especies,"<br>",
    "<strong>Species: </strong>", datos_anio$especies 
  )
  
  # Centro aproximado de Hermosillo
  centro <- c(lng = -110.9747, lat = 29.0728)
  
  # Crear título como control HTML
  titulo_html <- htmltools::tags$div(
    htmltools::HTML(paste0("<h3 style='text-align:center;'>Mosquito Abundance Map by Block - Year ", a, "</h3>"))
  )
  
  # Mapa leaflet
  mapa <- leaflet(datos_anio) %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    setView(lng = centro["lng"], lat = centro["lat"], zoom = 12) %>%
    addPolygons(
      fillColor = ~pal(total_individuos),
      fillOpacity = 0.7,
      color = "black",
      weight = 0.5,
      label = lapply(etiquetas, htmltools::HTML)
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~total_individuos,
      title = paste("Total Individuals -", a),
      opacity = 1
    ) %>%
    addLabelOnlyMarkers(
      data = st_centroid(datos_anio),
      lng = ~st_coordinates(geometry)[,1],
      lat = ~st_coordinates(geometry)[,2],
      label = ~as.character(num_especies),
      labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, 
                                  style = list("color" = "black", "font-weight" = "bold"))
    ) %>%
    addControl(titulo_html, position = "topright", className = "map-title")
  
  # Guardar como HTML
  saveWidget(mapa, paste0("mapa_calor_manzanas_", a, ".html"), selfcontained = TRUE)
}

##### pendiente ya no se usa 
  
# Bucle para crear un mapa por año
for (a in años) {
  
  datos_anio <- joined_data %>%
    filter(año == a) %>%
    group_by(`sector-manzana`, geometry) %>%
    summarise(
      total_individuos = sum(Individuos, na.rm = TRUE),
      num_especies = n_distinct(Diagnóstico),
      especies = paste(sort(unique(Diagnóstico)), collapse = ", "),
      .groups = "drop"
    )
  
  # Paleta de colores para abundancia
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = datos_anio$total_individuos
  )
  
  # Crear etiquetas emergentes
  etiquetas <- paste0(
    "<strong>Manzana: </strong>", datos_anio$`sector-manzana`, "<br>",
    "<strong>Total de individuos: </strong>", datos_anio$total_individuos, "<br>",
    "<strong>N° especies distintas: </strong>", datos_anio$num_especies,"<br>",
    "<strong>N° especies: </strong>", datos_anio$especies 
  )
  
  # Centro aproximado de Hermosillo
  centro <- c(lng = -110.9747, lat = 29.0728)
  
  # Mapa leaflet
  mapa <- leaflet(datos_anio) %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    setView(lng = centro["lng"], lat = centro["lat"], zoom = 12) %>%
    addPolygons(
      fillColor = ~pal(total_individuos),
      fillOpacity = 0.7,
      color = "black",
      weight = 0.5,
      label = lapply(etiquetas, htmltools::HTML)
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = ~total_individuos,
      title = paste("Abundancia total en", a),
      opacity = 1
    ) %>%
    addLabelOnlyMarkers(
      data = st_centroid(datos_anio),
      lng = ~st_coordinates(geometry)[,1],
      lat = ~st_coordinates(geometry)[,2],
      label = ~as.character(num_especies),
      labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, style = list("color" = "black", "font-weight" = "bold"))
    )
  
  # Guardar como HTML
  saveWidget(mapa, paste0("mapa_calor_manzanas_", a, ".html"), selfcontained = TRUE)
}



###otro bucle para crear mapas

for (a in años) {
  
  datos_anio <- df_filtradom %>%
    filter(year(fecha_colecta_date) == a) %>%
    group_by(`sector-manzana`, Diagnóstico) %>%
    summarise(Individuos = sum(Individuos, na.rm = TRUE), .groups = "drop") %>%
    group_by(`sector-manzana`) %>%
    summarise(
      total_individuos = sum(Individuos, na.rm = TRUE),
      num_especies = n_distinct(Diagnóstico),
      especies = paste(sort(unique(Diagnóstico)), collapse = ", "),
      .groups = "drop"
    )
  
  # Unir con shape para conservar todos los polígonos
  datos_map <- shape_datam %>%
    left_join(datos_anio, by = "sector-manzana")
  
  # Etiquetas emergentes
  etiquetas <- paste0(
    "<strong>Manzana: </strong>", datos_map$`sector-manzana`, "<br>",
    "<strong>Total de individuos: </strong>", ifelse(is.na(datos_map$total_individuos), "0", datos_map$total_individuos), "<br>",
    "<strong>N° especies distintas: </strong>", ifelse(is.na(datos_map$num_especies), "0", datos_map$num_especies), "<br>",
    "<strong>Especies: </strong>", ifelse(is.na(datos_map$especies), "Sin datos", datos_map$especies)
  )
  
  # Paleta de colores (poner dominio estático si deseas consistencia entre años)
  pal <- colorNumeric("YlOrRd", domain = datos_map$total_individuos, na.color = "transparent")
  
  mapa <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # Agregar contornos
    addPolylines(
      data = shape_contornos,
      color = "gray40",
      weight = 0.6,
      opacity = 0.5,
      group = "Contornos"
    ) %>%
    
    # Agregar polígonos (todos, aunque estén vacíos)
    addPolygons(
      data = datos_map,
      fillColor = ~pal(total_individuos),
      color = "black",
      weight = 0.4,
      fillOpacity = 0.6,
      label = lapply(etiquetas, htmltools::HTML)
    ) %>%
    
    addLegend(
      "bottomright",
      pal = pal,
      values = datos_map$total_individuos,
      title = paste("Total individuos -", a),
      opacity = 1
    )
  
  saveWidget(mapa, paste0("lineas_&_mapa_manzanas_mosquitos_", a, ".html"), selfcontained = TRUE)
}

