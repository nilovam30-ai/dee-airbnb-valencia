# ============================================================
# 01_revision_datos.R
# Revisión inicial de los datos
# Proyecto: Airbnb y presión residencial en València
# ============================================================


# 1. Paquetes 
library(tidyverse)
library(sf)
library(readxl)


# 3. Airbnb: lectura del archivo 

archivo_airbnb <- "data/airbnb/listings.csv.gz"

airbnb <- read_csv(archivo_airbnb, show_col_types = FALSE)

dim(airbnb)
names(airbnb)
glimpse(airbnb)


# Tipos de alojamiento

airbnb %>%
  count(room_type, sort = TRUE)


# Variables principales

airbnb %>%
  select(id, name, host_id,
         neighbourhood_cleansed,
         neighbourhood_group_cleansed,
         latitude, longitude,
         room_type, price,
         minimum_nights,
         number_of_reviews,
         availability_365,
         calculated_host_listings_count) %>%
  glimpse()


# 4. Convertir Airbnb a puntos espaciales 

airbnb_sf <- airbnb %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326,
           remove = FALSE)

class(airbnb_sf)
st_crs(airbnb_sf)
dim(airbnb_sf)


# Mapa simple de puntos Airbnb

ggplot() +
  geom_sf(data = airbnb_sf, size = 0.4) +
  theme_minimal()


# 5. Barrios de Inside Airbnb 

barrios_airbnb <- st_read("data/airbnb/neighbourhoods.geojson")

class(barrios_airbnb)
dim(barrios_airbnb)
names(barrios_airbnb)
st_crs(barrios_airbnb)

ggplot() +
  geom_sf(data = barrios_airbnb, fill = "white", color = "black") +
  theme_minimal()


# 6. Barrios oficiales del Geoportal de València 

barrios_geoportal <- st_read("data/cartografia/query.geojson")

class(barrios_geoportal)
dim(barrios_geoportal)
names(barrios_geoportal)
st_crs(barrios_geoportal)

barrios_geoportal %>%
  select(coddistrit, codbarrio, coddistbar, nombre) %>%
  head(20)

ggplot() +
  geom_sf(data = barrios_geoportal, fill = "white", color = "black") +
  theme_minimal()


# 7. Airbnb dentro de los barrios oficiales 

airbnb_valencia <- st_filter(airbnb_sf, barrios_geoportal)

dim(airbnb_sf)
dim(airbnb_valencia)

ggplot() +
  geom_sf(data = barrios_geoportal, fill = "white", color = "grey60") +
  geom_sf(data = airbnb_valencia, size = 0.4, alpha = 0.4) +
  theme_minimal()


# 8. Conteo de Airbnb por barrio 

airbnb_barrios <- st_join(airbnb_valencia, barrios_geoportal)

names(airbnb_barrios)

airbnb_por_barrio <- airbnb_barrios %>%
  st_drop_geometry() %>%
  count(coddistrit, codbarrio, coddistbar, nombre, sort = TRUE)

airbnb_por_barrio


# Conteo por barrio y tipo de alojamiento

airbnb_tipo_barrio <- airbnb_barrios %>%
  st_drop_geometry() %>%
  count(nombre, room_type, sort = TRUE)

airbnb_tipo_barrio


# Conteo de viviendas completas por barrio

airbnb_vivienda_entera_barrio <- airbnb_barrios %>%
  st_drop_geometry() %>%
  filter(room_type == "Entire home/apt") %>%
  count(coddistrit, codbarrio, coddistbar, nombre, sort = TRUE)

airbnb_vivienda_entera_barrio


# 9. Alquiler residencial: lectura 

alquiler <- st_read("data/vivienda/alquiler_georreferenciado.geojson")

class(alquiler)
dim(alquiler)
names(alquiler)
st_crs(alquiler)
glimpse(alquiler)


# Mapa general del archivo original.
# Este archivo contiene datos de toda España.

ggplot() +
  geom_sf(data = alquiler, size = 0.3) +
  theme_minimal()


# 10. Alquiler residencial en València 

# El archivo contiene datos de toda España.
# Para València, los registros aparecen a escala municipal,
# no repartidos por barrios. Por eso se usa como contexto municipal.

alquiler_valencia <- alquiler %>%
  filter(COD_PROVINCIA == 46) %>%
  filter(str_detect(NOMBRE_MUNICIPIO,
                    regex("Valencia|València", ignore_case = TRUE)))

dim(alquiler)
dim(alquiler_valencia)


alquiler_valencia %>%
  st_drop_geometry() %>%
  count(NOMBRE_MUNICIPIO, sort = TRUE)


alquiler_valencia %>%
  st_drop_geometry() %>%
  count(COD_POSTAL, sort = TRUE)


alquiler_valencia %>%
  st_drop_geometry() %>%
  count(AÑO, sort = TRUE)


alquiler_valencia %>%
  st_drop_geometry() %>%
  count(TIPO_VIVIENDA, sort = TRUE)


# Comprobación de coordenadas.
# Si todos los registros tienen la misma coordenada,
# el dato no sirve para hacer análisis por barrio.

coords_alquiler <- st_coordinates(alquiler_valencia)

alquiler_valencia <- alquiler_valencia %>%
  mutate(
    lon = coords_alquiler[, 1],
    lat = coords_alquiler[, 2]
  )

alquiler_valencia %>%
  st_drop_geometry() %>%
  count(lon, lat, sort = TRUE)


# Mapa de comprobación del alquiler municipal

ggplot() +
  geom_sf(data = barrios_geoportal, fill = "white", color = "grey60") +
  geom_sf(data = alquiler_valencia, size = 1, alpha = 0.5) +
  theme_minimal()


# Evolución del alquiler medio municipal

alquiler_municipal_anual <- alquiler_valencia %>%
  st_drop_geometry() %>%
  group_by(AÑO) %>%
  summarise(
    alquiler_medio = mean(VALOR, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(AÑO)

alquiler_municipal_anual


ggplot(alquiler_municipal_anual, aes(x = AÑO, y = alquiler_medio)) +
  geom_line() +
  geom_point() +
  theme_minimal()


# 11. Viviendas turísticas oficiales GVA 

# Este archivo usa separador ; y codificación Latin1.

vut_gva <- read_csv2(
  "data/vivienda/listaviviendas_20250126.csv",
  locale = locale(encoding = "Latin1"),
  show_col_types = FALSE
)

dim(vut_gva)
names(vut_gva)
glimpse(vut_gva)
head(vut_gva)


# Provincias disponibles

vut_gva %>%
  count(Provincia, sort = TRUE)


# Municipios que contienen VAL en el nombre.
# Esto sirve solo para revisar cómo aparece escrito València.

vut_gva %>%
  filter(str_detect(Municipio, regex("VAL", ignore_case = TRUE))) %>%
  count(Municipio, sort = TRUE)


# Filtrar registros de València ciudad

vut_valencia <- vut_gva %>%
  filter(str_detect(Municipio,
                    regex("^VALÈNCIA|^VALENCIA", ignore_case = TRUE)))

dim(vut_valencia)

vut_valencia %>%
  count(Municipio, sort = TRUE)

vut_valencia %>%
  count(Estado, sort = TRUE)

vut_valencia %>%
  count(Tipo, sort = TRUE)


# Solo registros en alta

vut_valencia_alta <- vut_valencia %>%
  filter(Estado == "ALTA")

dim(vut_valencia_alta)

vut_valencia_alta %>%
  count(Tipo, sort = TRUE)


# Evolución por fecha de alta

vut_valencia_alta <- vut_valencia_alta %>%
  mutate(
    fecha_alta = as.Date(`Fecha alta`, format = "%d/%m/%Y"),
    anio_alta = year(fecha_alta)
  )

vut_valencia_alta %>%
  count(anio_alta, sort = TRUE)


# 12. Población por barrios 

# Los archivos de población están dentro de data/vivienda/Barrios2024.
# Se excluye el archivo temporal de Excel que empieza por ~$.

archivos_barrios2024 <- list.files(
  "data/vivienda/Barrios2024",
  pattern = "\\.xlsx$",
  full.names = TRUE
)

archivos_barrios2024 <- archivos_barrios2024[
  !str_detect(archivos_barrios2024, "~\\$")
]

length(archivos_barrios2024)
head(archivos_barrios2024)


# Lectura de un archivo de ejemplo

pob_ejemplo <- read_xlsx(
  "data/vivienda/Barrios2024/Distrito_01_Barrio_1.xlsx",
  col_names = FALSE
)

dim(pob_ejemplo)
head(pob_ejemplo, 30)


# 13. Renta media 

# Este archivo también usa separador ;.

renta <- read_csv2(
  "data/cartografia/30824.csv",
  show_col_types = FALSE
)

dim(renta)
names(renta)
glimpse(renta)
head(renta)


# Filtrar València municipio por código INE 46250

renta_valencia <- renta %>%
  filter(str_detect(Municipios, "^46250"))

dim(renta_valencia)

renta_valencia %>%
  count(Municipios)

renta_valencia %>%
  count(`Indicadores de renta media`, sort = TRUE)

renta_valencia %>%
  count(Periodo, sort = TRUE)


# Renta neta media por persona para el último año disponible

renta_persona_valencia <- renta_valencia %>%
  filter(`Indicadores de renta media` == "Renta neta media por persona")

ultimo_anio_renta <- max(renta_persona_valencia$Periodo, na.rm = TRUE)

ultimo_anio_renta

renta_persona_ultimo_anio <- renta_persona_valencia %>%
  filter(Periodo == ultimo_anio_renta)

head(renta_persona_ultimo_anio)


# 14. Resumen de revisión 

resumen <- tibble(
  dataset = c(
    "Airbnb original",
    "Airbnb dentro de València",
    "Barrios Airbnb",
    "Barrios Geoportal",
    "Alquiler original",
    "Alquiler València municipal",
    "VUT GVA original",
    "VUT GVA València",
    "VUT GVA València en alta",
    "Renta original",
    "Renta València",
    "Excel población barrios"
  ),
  filas = c(
    nrow(airbnb),
    nrow(airbnb_valencia),
    nrow(barrios_airbnb),
    nrow(barrios_geoportal),
    nrow(alquiler),
    nrow(alquiler_valencia),
    nrow(vut_gva),
    nrow(vut_valencia),
    nrow(vut_valencia_alta),
    nrow(renta),
    nrow(renta_valencia),
    length(archivos_barrios2024)
  ),
  columnas = c(
    ncol(airbnb),
    ncol(airbnb_valencia),
    ncol(barrios_airbnb),
    ncol(barrios_geoportal),
    ncol(alquiler),
    ncol(alquiler_valencia),
    ncol(vut_gva),
    ncol(vut_valencia),
    ncol(vut_valencia_alta),
    ncol(renta),
    ncol(renta_valencia),
    NA
  )
)

resumen


# 15. Guardar tablas de revisión 

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

if (!dir.exists("outputs/tablas")) {
  dir.create("outputs/tablas", recursive = TRUE)
}

write_csv(resumen, "outputs/tablas/resumen_revision_datos.csv")
write_csv(airbnb_por_barrio, "outputs/tablas/airbnb_por_barrio_revision.csv")
write_csv(airbnb_tipo_barrio, "outputs/tablas/airbnb_tipo_barrio_revision.csv")
write_csv(airbnb_vivienda_entera_barrio, "outputs/tablas/airbnb_vivienda_entera_barrio_revision.csv")
write_csv(alquiler_municipal_anual, "outputs/tablas/alquiler_municipal_anual_revision.csv")







