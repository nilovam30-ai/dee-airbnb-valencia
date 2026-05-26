# ============================================================
# 02_limpieza_datos.R
# Limpieza y preparación de datos para el análisis
# Proyecto: Airbnb y presión residencial en València
# ============================================================


# 1. Paquetes -------------------------------------------------------------

library(tidyverse)
library(sf)
library(readxl)


# 2. Lectura de Airbnb ----------------------------------------------------

archivo_airbnb <- "data/airbnb/listings.csv.gz"

airbnb <- read_csv(archivo_airbnb, show_col_types = FALSE)


# Limpieza básica de Airbnb

airbnb_limpio <- airbnb %>%
  mutate(
    price_num = parse_number(price),
    vivienda_completa = if_else(room_type == "Entire home/apt", 1, 0),
    habitacion_privada = if_else(room_type == "Private room", 1, 0)
  ) %>%
  select(
    id,
    host_id,
    neighbourhood_cleansed,
    neighbourhood_group_cleansed,
    latitude,
    longitude,
    room_type,
    price,
    price_num,
    minimum_nights,
    number_of_reviews,
    availability_365,
    calculated_host_listings_count,
    vivienda_completa,
    habitacion_privada
  )

dim(airbnb_limpio)
glimpse(airbnb_limpio)


# Convertir Airbnb a puntos espaciales

airbnb_sf <- airbnb_limpio %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )


# 3. Lectura de barrios oficiales ----------------------------------------

barrios <- st_read("data/cartografia/query.geojson", quiet = TRUE)

barrios <- barrios %>%
  select(coddistrit, codbarrio, coddistbar, nombre, geometry)

dim(barrios)
names(barrios)


# 4. Unir Airbnb con barrios ---------------------------------------------

airbnb_barrios <- st_join(airbnb_sf, barrios)

dim(airbnb_barrios)

airbnb_barrios %>%
  st_drop_geometry() %>%
  count(nombre, sort = TRUE)


# 5. Airbnb por barrio ----------------------------------------------------

airbnb_por_barrio <- airbnb_barrios %>%
  st_drop_geometry() %>%
  group_by(coddistrit, codbarrio, coddistbar, nombre) %>%
  summarise(
    airbnb_total = n(),
    airbnb_vivienda_completa = sum(vivienda_completa, na.rm = TRUE),
    airbnb_habitacion_privada = sum(habitacion_privada, na.rm = TRUE),
    precio_medio_airbnb = mean(price_num, na.rm = TRUE),
    disponibilidad_media = mean(availability_365, na.rm = TRUE),
    resenas_media = mean(number_of_reviews, na.rm = TRUE),
    host_listings_medio = mean(calculated_host_listings_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(airbnb_total))

airbnb_por_barrio


# 6. Superficie de barrios ------------------------------------------------

# Se transforma a EPSG:25830 para calcular áreas en metros.
# Después se vuelve a EPSG:4326 para trabajar y representar mapas.

barrios_area <- barrios %>%
  st_transform(25830) %>%
  mutate(
    area_m2 = as.numeric(st_area(geometry)),
    area_km2 = area_m2 / 1000000
  ) %>%
  st_transform(4326)

barrios_area %>%
  st_drop_geometry() %>%
  select(coddistbar, nombre, area_km2) %>%
  arrange(desc(area_km2))


# 7. Indicadores Airbnb por barrio ---------------------------------------

barrios_airbnb <- barrios_area %>%
  left_join(
    airbnb_por_barrio,
    by = c("coddistrit", "codbarrio", "coddistbar", "nombre")
  ) %>%
  mutate(
    airbnb_total = replace_na(airbnb_total, 0),
    airbnb_vivienda_completa = replace_na(airbnb_vivienda_completa, 0),
    airbnb_habitacion_privada = replace_na(airbnb_habitacion_privada, 0),
    airbnb_por_km2 = airbnb_total / area_km2,
    vivienda_completa_por_km2 = airbnb_vivienda_completa / area_km2
  )

barrios_airbnb %>%
  st_drop_geometry() %>%
  arrange(desc(airbnb_total)) %>%
  select(nombre, airbnb_total, airbnb_vivienda_completa, area_km2, airbnb_por_km2)


# Mapa de comprobación: total Airbnb por barrio

ggplot() +
  geom_sf(data = barrios_airbnb, aes(fill = airbnb_total), color = "white") +
  theme_minimal()


# Mapa de comprobación: densidad Airbnb por km2

ggplot() +
  geom_sf(data = barrios_airbnb, aes(fill = airbnb_por_km2), color = "white") +
  theme_minimal()


# 8. Viviendas turísticas oficiales GVA ----------------------------------

vut_gva <- read_csv2(
  "data/vivienda/listaviviendas_20250126.csv",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

vut_valencia <- vut_gva %>%
  filter(Municipio == "VALÈNCIA") %>%
  filter(Estado == "ALTA") %>%
  mutate(
    fecha_alta = as.Date(`Fecha alta`, format = "%d/%m/%Y"),
    anio_alta = year(fecha_alta)
  )

dim(vut_valencia)

vut_valencia %>%
  count(Tipo, sort = TRUE)

vut_valencia_anual <- vut_valencia %>%
  count(anio_alta, sort = FALSE) %>%
  arrange(anio_alta)

vut_valencia_anual


ggplot(vut_valencia_anual, aes(x = anio_alta, y = n)) +
  geom_line() +
  geom_point() +
  theme_minimal()


# 9. Alquiler municipal ---------------------------------------------------

alquiler <- st_read("data/vivienda/alquiler_georreferenciado.geojson", quiet = TRUE)

alquiler_valencia <- alquiler %>%
  filter(COD_PROVINCIA == 46) %>%
  filter(str_detect(NOMBRE_MUNICIPIO,
                    regex("Valencia|València", ignore_case = TRUE)))

dim(alquiler_valencia)

alquiler_valencia %>%
  st_drop_geometry() %>%
  count(NOMBRE_MUNICIPIO, sort = TRUE)

alquiler_municipal_anual <- alquiler_valencia %>%
  st_drop_geometry() %>%
  group_by(AÑO) %>%
  summarise(
    alquiler_medio = mean(VALOR, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(AÑO)

alquiler_municipal_anual


ggplot(alquiler_municipal_anual, aes(x = AÑO, y = alquiler_medio)) +
  geom_line() +
  geom_point() +
  theme_minimal()


# 10. Renta media ---------------------------------------------------------

renta <- read_csv2(
  "data/cartografia/30824.csv",
  show_col_types = FALSE
)

renta_valencia <- renta %>%
  filter(str_detect(Municipios, "^46250"))


# Se trabaja con la renta neta media por persona en el último año disponible.

renta_persona_valencia <- renta_valencia %>%
  filter(`Indicadores de renta media` == "Renta neta media por persona")

ultimo_anio_renta <- max(renta_persona_valencia$Periodo, na.rm = TRUE)

ultimo_anio_renta

renta_persona_2023 <- renta_persona_valencia %>%
  filter(Periodo == ultimo_anio_renta) %>%
  mutate(
    renta_neta_persona = parse_number(
      Total,
      locale = locale(grouping_mark = ".", decimal_mark = ",")
    )
  )

dim(renta_persona_2023)
head(renta_persona_2023)


# Se separan datos municipales, distritales y de secciones censales.

renta_municipal_2023 <- renta_persona_2023 %>%
  filter(is.na(Distritos), is.na(Secciones))

renta_distritos_2023 <- renta_persona_2023 %>%
  filter(!is.na(Distritos), is.na(Secciones))

renta_secciones_2023 <- renta_persona_2023 %>%
  filter(!is.na(Secciones))

renta_municipal_2023
head(renta_distritos_2023)
head(renta_secciones_2023)


# 11. Población por barrios ----------------------------------------------

archivos_barrios2024 <- list.files(
  "data/vivienda/Barrios2024",
  pattern = "\\.xlsx$",
  full.names = TRUE
)

archivos_barrios2024 <- archivos_barrios2024[
  !str_detect(archivos_barrios2024, "~\\$")
]

length(archivos_barrios2024)


# Función para extraer población 2024 de cada Excel.
# Busca el año 2024 en las filas cercanas y toma el valor de la fila inferior.

leer_poblacion_barrio <- function(archivo) {
  
  tabla <- read_xlsx(
    archivo,
    col_names = FALSE,
    .name_repair = "minimal"
  )
  
  tabla_texto <- tabla %>%
    mutate(across(everything(), as.character))
  
  filas_busqueda <- 7:15
  
  tabla_busqueda <- tabla_texto[filas_busqueda, ]
  
  posicion_2024 <- which(
    tabla_busqueda == "2024" | tabla_busqueda == "2024.0",
    arr.ind = TRUE
  )
  
  if (nrow(posicion_2024) == 0) {
    
    poblacion_2024 <- NA_real_
    
  } else {
    
    fila_2024 <- filas_busqueda[posicion_2024[1, "row"]]
    columna_2024 <- posicion_2024[1, "col"]
    
    valor <- tabla_texto[fila_2024 + 1, columna_2024] %>%
      unlist(use.names = FALSE)
    
    poblacion_2024 <- parse_number(
      valor,
      locale = locale(grouping_mark = ".", decimal_mark = ",")
    )
  }
  
  tibble(
    archivo = basename(archivo),
    poblacion_2024 = poblacion_2024
  )
}


poblacion_barrios <- map_dfr(archivos_barrios2024, leer_poblacion_barrio) %>%
  mutate(
    distrito = str_extract(archivo, "(?<=Distrito_)\\d+"),
    barrio = str_extract(archivo, "(?<=Barrio_)\\d+"),
    coddistrit = as.character(as.numeric(distrito)),
    codbarrio = as.character(as.numeric(barrio))
  ) %>%
  select(coddistrit, codbarrio, poblacion_2024, archivo)

poblacion_barrios

summary(poblacion_barrios$poblacion_2024)


# Comprobar archivos donde no se pudo leer población

poblacion_barrios %>%
  filter(is.na(poblacion_2024))


# 12. Añadir población a barrios -----------------------------------------

barrios_indicadores <- barrios_airbnb %>%
  left_join(
    poblacion_barrios,
    by = c("coddistrit", "codbarrio")
  ) %>%
  mutate(
    airbnb_por_1000_hab = airbnb_total / poblacion_2024 * 1000,
    vivienda_completa_por_1000_hab = airbnb_vivienda_completa / poblacion_2024 * 1000,
    densidad_poblacion = poblacion_2024 / area_km2
  )

barrios_indicadores %>%
  st_drop_geometry() %>%
  arrange(desc(airbnb_por_1000_hab)) %>%
  select(nombre, poblacion_2024, airbnb_total, airbnb_por_1000_hab)


# Comprobar si algún barrio quedó sin población

barrios_indicadores %>%
  st_drop_geometry() %>%
  filter(is.na(poblacion_2024)) %>%
  select(coddistrit, codbarrio, nombre)


# 13. Mapas de comprobación ----------------------------------------------

ggplot() +
  geom_sf(data = barrios_indicadores, aes(fill = airbnb_por_1000_hab), color = "white") +
  theme_minimal()


ggplot() +
  geom_sf(data = barrios_indicadores, aes(fill = vivienda_completa_por_1000_hab), color = "white") +
  theme_minimal()


ggplot() +
  geom_sf(data = barrios_indicadores, aes(fill = densidad_poblacion), color = "white") +
  theme_minimal()


# 14. Guardar datos limpios ----------------------------------------------

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

if (!dir.exists("outputs/tablas")) {
  dir.create("outputs/tablas", recursive = TRUE)
}

if (!dir.exists("outputs/geodatos")) {
  dir.create("outputs/geodatos", recursive = TRUE)
}


# Tablas

write_csv(
  st_drop_geometry(barrios_indicadores),
  "outputs/tablas/barrios_indicadores_limpios.csv"
)

write_csv(
  airbnb_por_barrio,
  "outputs/tablas/airbnb_por_barrio_limpio.csv"
)

write_csv(
  vut_valencia_anual,
  "outputs/tablas/vut_valencia_anual_limpio.csv"
)

write_csv(
  alquiler_municipal_anual,
  "outputs/tablas/alquiler_municipal_anual_limpio.csv"
)

write_csv(
  renta_distritos_2023,
  "outputs/tablas/renta_distritos_2023_limpio.csv"
)

write_csv(
  renta_secciones_2023,
  "outputs/tablas/renta_secciones_2023_limpio.csv"
)


# Geodato principal para mapas

if (file.exists("outputs/geodatos/barrios_indicadores_limpios.gpkg")) {
  file.remove("outputs/geodatos/barrios_indicadores_limpios.gpkg")
}

st_write(
  barrios_indicadores,
  "outputs/geodatos/barrios_indicadores_limpios.gpkg"
)


print("Limpieza de datos terminada.")

