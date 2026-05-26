# ============================================================
# 03_analisis_descriptivo.R
# Análisis descriptivo y primeros resultados
# Proyecto: Airbnb y presión residencial en València
# ============================================================


# 1. Paquetes -------------------------------------------------------------

library(tidyverse)
library(sf)


# 2. Leer datos limpios ---------------------------------------------------

barrios_indicadores <- st_read(
  "outputs/geodatos/barrios_indicadores_limpios.gpkg",
  quiet = TRUE
)

airbnb_por_barrio <- read_csv(
  "outputs/tablas/airbnb_por_barrio_limpio.csv",
  show_col_types = FALSE
)

vut_valencia_anual <- read_csv(
  "outputs/tablas/vut_valencia_anual_limpio.csv",
  show_col_types = FALSE
)

alquiler_municipal_anual <- read_csv(
  "outputs/tablas/alquiler_municipal_anual_limpio.csv",
  show_col_types = FALSE
)

renta_distritos_2023 <- read_csv(
  "outputs/tablas/renta_distritos_2023_limpio.csv",
  show_col_types = FALSE
)


# 3. Crear carpetas de salida --------------------------------------------

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

if (!dir.exists("outputs/tablas")) {
  dir.create("outputs/tablas", recursive = TRUE)
}

if (!dir.exists("outputs/figuras")) {
  dir.create("outputs/figuras", recursive = TRUE)
}


# 4. Resumen general del análisis ----------------------------------------

resumen_general <- barrios_indicadores %>%
  st_drop_geometry() %>%
  summarise(
    barrios = n(),
    airbnb_total = sum(airbnb_total, na.rm = TRUE),
    airbnb_vivienda_completa = sum(airbnb_vivienda_completa, na.rm = TRUE),
    airbnb_habitacion_privada = sum(airbnb_habitacion_privada, na.rm = TRUE),
    poblacion_total = sum(poblacion_2024, na.rm = TRUE),
    airbnb_por_1000_hab_media = mean(airbnb_por_1000_hab, na.rm = TRUE),
    airbnb_por_km2_media = mean(airbnb_por_km2, na.rm = TRUE)
  )

resumen_general


# Porcentaje de viviendas completas dentro de Airbnb

resumen_general <- resumen_general %>%
  mutate(
    porcentaje_vivienda_completa = airbnb_vivienda_completa / airbnb_total * 100,
    porcentaje_habitacion_privada = airbnb_habitacion_privada / airbnb_total * 100
  )

resumen_general


# Guardar resumen

write_csv(
  resumen_general,
  "outputs/tablas/resumen_general_analisis.csv"
)


# 5. Ranking de barrios por número total de Airbnb ------------------------

ranking_airbnb_total <- barrios_indicadores %>%
  st_drop_geometry() %>%
  arrange(desc(airbnb_total)) %>%
  select(
    nombre,
    airbnb_total,
    airbnb_vivienda_completa,
    airbnb_habitacion_privada,
    poblacion_2024,
    area_km2
  )

ranking_airbnb_total

ranking_airbnb_total_10 <- ranking_airbnb_total %>%
  slice_head(n = 10)

ranking_airbnb_total_10


write_csv(
  ranking_airbnb_total,
  "outputs/tablas/ranking_airbnb_total.csv"
)


# Gráfico: barrios con más Airbnb

grafico_airbnb_total <- ranking_airbnb_total_10 %>%
  mutate(nombre = reorder(nombre, airbnb_total)) %>%
  ggplot(aes(x = nombre, y = airbnb_total)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Barrios con mayor número de alojamientos Airbnb",
    x = "Barrio",
    y = "Número de alojamientos"
  ) +
  theme_minimal()

grafico_airbnb_total

ggsave(
  "outputs/figuras/ranking_airbnb_total.png",
  grafico_airbnb_total,
  width = 9,
  height = 6
)


# 6. Ranking de barrios por Airbnb por km2 -------------------------------

ranking_airbnb_km2 <- barrios_indicadores %>%
  st_drop_geometry() %>%
  arrange(desc(airbnb_por_km2)) %>%
  select(
    nombre,
    airbnb_total,
    area_km2,
    airbnb_por_km2,
    airbnb_vivienda_completa,
    vivienda_completa_por_km2
  )

ranking_airbnb_km2

ranking_airbnb_km2_10 <- ranking_airbnb_km2 %>%
  slice_head(n = 10)

ranking_airbnb_km2_10


write_csv(
  ranking_airbnb_km2,
  "outputs/tablas/ranking_airbnb_km2.csv"
)


# Gráfico: Airbnb por km2

grafico_airbnb_km2 <- ranking_airbnb_km2_10 %>%
  mutate(nombre = reorder(nombre, airbnb_por_km2)) %>%
  ggplot(aes(x = nombre, y = airbnb_por_km2)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Barrios con mayor densidad de Airbnb por km²",
    x = "Barrio",
    y = "Airbnb por km²"
  ) +
  theme_minimal()

grafico_airbnb_km2

ggsave(
  "outputs/figuras/ranking_airbnb_km2.png",
  grafico_airbnb_km2,
  width = 9,
  height = 6
)


# 7. Ranking de barrios por Airbnb por 1.000 habitantes -------------------

ranking_airbnb_1000 <- barrios_indicadores %>%
  st_drop_geometry() %>%
  arrange(desc(airbnb_por_1000_hab)) %>%
  select(
    nombre,
    poblacion_2024,
    airbnb_total,
    airbnb_vivienda_completa,
    airbnb_por_1000_hab,
    vivienda_completa_por_1000_hab
  )

ranking_airbnb_1000

ranking_airbnb_1000_10 <- ranking_airbnb_1000 %>%
  slice_head(n = 10)

ranking_airbnb_1000_10


write_csv(
  ranking_airbnb_1000,
  "outputs/tablas/ranking_airbnb_1000_habitantes.csv"
)


# Gráfico: Airbnb por 1.000 habitantes

grafico_airbnb_1000 <- ranking_airbnb_1000_10 %>%
  mutate(nombre = reorder(nombre, airbnb_por_1000_hab)) %>%
  ggplot(aes(x = nombre, y = airbnb_por_1000_hab)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Barrios con mayor presión de Airbnb por 1.000 habitantes",
    x = "Barrio",
    y = "Airbnb por 1.000 habitantes"
  ) +
  theme_minimal()

grafico_airbnb_1000

ggsave(
  "outputs/figuras/ranking_airbnb_1000_habitantes.png",
  grafico_airbnb_1000,
  width = 9,
  height = 6
)


# 8. Peso de vivienda completa -------------------------------------------

ranking_vivienda_completa <- barrios_indicadores %>%
  st_drop_geometry() %>%
  filter(airbnb_total > 0) %>%
  mutate(
    porcentaje_vivienda_completa = airbnb_vivienda_completa / airbnb_total * 100
  ) %>%
  arrange(desc(porcentaje_vivienda_completa)) %>%
  select(
    nombre,
    airbnb_total,
    airbnb_vivienda_completa,
    porcentaje_vivienda_completa
  )

ranking_vivienda_completa

write_csv(
  ranking_vivienda_completa,
  "outputs/tablas/ranking_vivienda_completa.csv"
)


# Gráfico: barrios con más Airbnb y peso de vivienda completa

grafico_vivienda_completa <- ranking_airbnb_total_10 %>%
  mutate(
    porcentaje_vivienda_completa = airbnb_vivienda_completa / airbnb_total * 100,
    nombre = reorder(nombre, porcentaje_vivienda_completa)
  ) %>%
  ggplot(aes(x = nombre, y = porcentaje_vivienda_completa)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Peso de la vivienda completa en los principales barrios Airbnb",
    x = "Barrio",
    y = "% de viviendas completas"
  ) +
  theme_minimal()

grafico_vivienda_completa

ggsave(
  "outputs/figuras/porcentaje_vivienda_completa_top10.png",
  grafico_vivienda_completa,
  width = 9,
  height = 6
)


# 9. Evolución del alquiler municipal ------------------------------------


alquiler_municipal_anual

fila_alquiler_inicio <- alquiler_municipal_anual %>%
  arrange(AÑO) %>%
  slice(1)

fila_alquiler_final <- alquiler_municipal_anual %>%
  arrange(AÑO) %>%
  slice(n())

crecimiento_alquiler <- tibble(
  anio_inicial = fila_alquiler_inicio$AÑO,
  alquiler_inicial = fila_alquiler_inicio$alquiler_medio,
  anio_final = fila_alquiler_final$AÑO,
  alquiler_final = fila_alquiler_final$alquiler_medio,
  crecimiento_absoluto = fila_alquiler_final$alquiler_medio - fila_alquiler_inicio$alquiler_medio,
  crecimiento_porcentual = (fila_alquiler_final$alquiler_medio / fila_alquiler_inicio$alquiler_medio - 1) * 100
)

crecimiento_alquiler

write_csv(
  crecimiento_alquiler,
  "outputs/tablas/crecimiento_alquiler_municipal.csv"
)


grafico_alquiler <- ggplot(alquiler_municipal_anual,
                           aes(x = AÑO, y = alquiler_medio)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Evolución del alquiler residencial medio en València",
    x = "Año",
    y = "Alquiler medio"
  ) +
  theme_minimal()

grafico_alquiler

ggsave(
  "outputs/figuras/evolucion_alquiler_municipal.png",
  grafico_alquiler,
  width = 9,
  height = 6
)


# 10. Evolución de viviendas turísticas oficiales -------------------------

vut_valencia_anual

grafico_vut <- ggplot(vut_valencia_anual,
                      aes(x = anio_alta, y = n)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Altas anuales de viviendas turísticas oficiales en València",
    x = "Año de alta",
    y = "Número de altas"
  ) +
  theme_minimal()

grafico_vut

ggsave(
  "outputs/figuras/evolucion_vut_valencia.png",
  grafico_vut,
  width = 9,
  height = 6
)


# 11. Renta por distritos -------------------------------------------------

renta_distritos_2023 <- renta_distritos_2023 %>%
  mutate(
    codigo_distrito = str_extract(Distritos, "46250\\d{2}"),
    coddistrit = as.character(as.numeric(str_sub(codigo_distrito, 6, 7)))
  )

renta_distritos_2023

grafico_renta_distritos <- renta_distritos_2023 %>%
  mutate(Distritos = reorder(Distritos, renta_neta_persona)) %>%
  ggplot(aes(x = Distritos, y = renta_neta_persona)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Renta neta media por persona en los distritos de València, 2023",
    x = "Distrito",
    y = "Renta neta media por persona"
  ) +
  theme_minimal()

grafico_renta_distritos

ggsave(
  "outputs/figuras/renta_distritos_2023.png",
  grafico_renta_distritos,
  width = 9,
  height = 7
)


# 12. Relación descriptiva entre Airbnb y variables de barrio -------------

tabla_correlaciones <- barrios_indicadores %>%
  st_drop_geometry() %>%
  summarise(
    cor_airbnb_poblacion = cor(airbnb_total, poblacion_2024, use = "complete.obs"),
    cor_airbnb_densidad_poblacion = cor(airbnb_total, densidad_poblacion, use = "complete.obs"),
    cor_airbnb_area = cor(airbnb_total, area_km2, use = "complete.obs"),
    cor_airbnb_por_1000_y_densidad = cor(airbnb_por_1000_hab, densidad_poblacion, use = "complete.obs")
  )

tabla_correlaciones

write_csv(
  tabla_correlaciones,
  "outputs/tablas/correlaciones_descriptivas.csv"
)


# Gráfico de dispersión: Airbnb total y población

grafico_airbnb_poblacion <- barrios_indicadores %>%
  st_drop_geometry() %>%
  ggplot(aes(x = poblacion_2024, y = airbnb_total)) +
  geom_point() +
  labs(
    title = "Relación entre población y número de Airbnb por barrio",
    x = "Población 2024",
    y = "Número de Airbnb"
  ) +
  theme_minimal()

grafico_airbnb_poblacion

ggsave(
  "outputs/figuras/dispersion_airbnb_poblacion.png",
  grafico_airbnb_poblacion,
  width = 8,
  height = 6
)


# Gráfico de dispersión: Airbnb por 1000 habitantes y densidad de población

grafico_airbnb_densidad <- barrios_indicadores %>%
  st_drop_geometry() %>%
  ggplot(aes(x = densidad_poblacion, y = airbnb_por_1000_hab)) +
  geom_point() +
  labs(
    title = "Airbnb por 1.000 habitantes y densidad de población",
    x = "Densidad de población",
    y = "Airbnb por 1.000 habitantes"
  ) +
  theme_minimal()

grafico_airbnb_densidad

ggsave(
  "outputs/figuras/dispersion_airbnb_densidad_poblacion.png",
  grafico_airbnb_densidad,
  width = 8,
  height = 6
)


# 13. Mapas finales -------------------------------------------------------

mapa_airbnb_total <- ggplot() +
  geom_sf(data = barrios_indicadores,
          aes(fill = airbnb_total),
          color = "white") +
  labs(
    title = "Distribución de alojamientos Airbnb por barrio",
    fill = "Airbnb"
  ) +
  theme_minimal()

mapa_airbnb_total

ggsave(
  "outputs/figuras/mapa_airbnb_total.png",
  mapa_airbnb_total,
  width = 8,
  height = 8
)


mapa_airbnb_km2 <- ggplot() +
  geom_sf(data = barrios_indicadores,
          aes(fill = airbnb_por_km2),
          color = "white") +
  labs(
    title = "Densidad de Airbnb por km²",
    fill = "Airbnb/km²"
  ) +
  theme_minimal()

mapa_airbnb_km2

ggsave(
  "outputs/figuras/mapa_airbnb_km2.png",
  mapa_airbnb_km2,
  width = 8,
  height = 8
)


mapa_airbnb_1000 <- ggplot() +
  geom_sf(data = barrios_indicadores,
          aes(fill = airbnb_por_1000_hab),
          color = "white") +
  labs(
    title = "Airbnb por 1.000 habitantes",
    fill = "Airbnb/1000 hab."
  ) +
  theme_minimal()

mapa_airbnb_1000

ggsave(
  "outputs/figuras/mapa_airbnb_1000_habitantes.png",
  mapa_airbnb_1000,
  width = 8,
  height = 8
)


mapa_vivienda_completa <- ggplot() +
  geom_sf(data = barrios_indicadores,
          aes(fill = vivienda_completa_por_1000_hab),
          color = "white") +
  labs(
    title = "Viviendas completas de Airbnb por 1.000 habitantes",
    fill = "Viv. completas/1000 hab."
  ) +
  theme_minimal()

mapa_vivienda_completa

ggsave(
  "outputs/figuras/mapa_vivienda_completa_1000_habitantes.png",
  mapa_vivienda_completa,
  width = 8,
  height = 8
)


# 14. Tabla de resultados principales ------------------------------------

resultados_principales <- barrios_indicadores %>%
  st_drop_geometry() %>%
  select(
    nombre,
    airbnb_total,
    airbnb_vivienda_completa,
    airbnb_habitacion_privada,
    poblacion_2024,
    area_km2,
    airbnb_por_km2,
    airbnb_por_1000_hab,
    vivienda_completa_por_1000_hab,
    densidad_poblacion
  ) %>%
  arrange(desc(airbnb_por_1000_hab))

resultados_principales

write_csv(
  resultados_principales,
  "outputs/tablas/resultados_principales_barrios.csv"
)




