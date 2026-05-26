
# Visualizaciones finales para informe y anexo



# 1. Paquetes

library(tidyverse)
library(sf)
library(spdep)
library(spatstat)
library(scales)
library(ggrepel)
library(patchwork)


# 2. Leer datos

barrios_indicadores <- st_read(
  "outputs/geodatos/barrios_indicadores_limpios.gpkg",
  quiet = TRUE
) %>%
  st_make_valid()

renta_distritos_2023 <- read_csv(
  "outputs/tablas/renta_distritos_2023_limpio.csv",
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


# 3. Leer o crear puntos Airbnb

if (file.exists("outputs/geodatos/airbnb_sf_limpios.gpkg")) {
  
  airbnb_sf <- st_read(
    "outputs/geodatos/airbnb_sf_limpios.gpkg",
    quiet = TRUE
  )
  
} else {
  
  archivo_airbnb <- "data/airbnb/listings.csv.gz"
  
  if (!file.exists(archivo_airbnb)) {
    archivo_airbnb <- "data/airbnb/listings.csv"
  }
  
  airbnb_raw <- read_csv(
    archivo_airbnb,
    show_col_types = FALSE
  )
  
  airbnb_sf <- airbnb_raw %>%
    filter(!is.na(longitude), !is.na(latitude)) %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    )
  
  if (!dir.exists("outputs/geodatos")) {
    dir.create("outputs/geodatos", recursive = TRUE)
  }
  
  st_write(
    airbnb_sf,
    "outputs/geodatos/airbnb_sf_limpios.gpkg",
    quiet = TRUE,
    append = FALSE
  )
}


# 4. Carpetas de salida

if (!dir.exists("outputs/figuras_finales")) {
  dir.create("outputs/figuras_finales", recursive = TRUE)
}

if (!dir.exists("outputs/figuras_anexo")) {
  dir.create("outputs/figuras_anexo", recursive = TRUE)
}

if (!dir.exists("outputs/tablas")) {
  dir.create("outputs/tablas", recursive = TRUE)
}


# 6. Colores comunes

color_principal  <- "#C0392B"
color_secundario <- "#2980B9"
color_neutro     <- "#ECF0F1"
color_texto      <- "#2C3E50"


# 7. Datos proyectados y zoom urbano

barrios_25830 <- barrios_indicadores %>%
  st_transform(25830)

airbnb_25830 <- airbnb_sf %>%
  st_transform(25830)

bbox_urbano <- st_bbox(c(
  xmin = 720000,
  xmax = 735000,
  ymin = 4367000,
  ymax = 4382000
), crs = st_crs(barrios_25830))


# 1
# Mapa de presión Airbnb por 1.000 habitantes


g1_mapa_1000 <- ggplot() +
  geom_sf(
    data = barrios_25830,
    aes(fill = airbnb_por_1000_hab),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    trans = "sqrt",
    labels = label_number(big.mark = ".", decimal.mark = ",", accuracy = 1),
    name = "Airbnb por\n1.000 hab."
  ) +
  coord_sf(
    xlim = c(bbox_urbano["xmin"], bbox_urbano["xmax"]),
    ylim = c(bbox_urbano["ymin"], bbox_urbano["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Presión Airbnb por 1.000 habitantes",
    subtitle = "Alojamientos Airbnb en relación con la población residente por barrio",
    caption = "Fuente: elaboración propia a partir de Inside Airbnb, padrón municipal y cartografía municipal."
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = color_texto, size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey55", size = 8, hjust = 0),
    legend.position = c(0.87, 0.45),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

g1_mapa_1000

ggsave(
  "outputs/figuras_finales/01_mapa_airbnb_1000_habitantes.png",
  g1_mapa_1000,
  width = 7.5,
  height = 7.5,
  dpi = 300
)


# 2
# Lollipop: top 15 barrios por Airbnb por 1.000 habitantes


media_global <- mean(
  barrios_indicadores$airbnb_por_1000_hab,
  na.rm = TRUE
)

umbral_presion <- mean(
  barrios_indicadores$airbnb_por_1000_hab,
  na.rm = TRUE
) +
  sd(
    barrios_indicadores$airbnb_por_1000_hab,
    na.rm = TRUE
  )

top15 <- barrios_indicadores %>%
  st_drop_geometry() %>%
  arrange(desc(airbnb_por_1000_hab)) %>%
  slice_head(n = 15) %>%
  mutate(
    nombre_label = str_to_title(str_to_lower(nombre)),
    nombre_label = forcats::fct_reorder(
      nombre_label,
      airbnb_por_1000_hab
    ),
    presion = if_else(
      airbnb_por_1000_hab > umbral_presion,
      "Muy alta",
      "Alta"
    ),
    presion = factor(presion, levels = c("Muy alta", "Alta"))
  )

g2_lollipop <- ggplot(
  top15,
  aes(x = airbnb_por_1000_hab, y = nombre_label, color = presion)
) +
  geom_vline(
    xintercept = media_global,
    color = "grey55",
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = airbnb_por_1000_hab,
      y = nombre_label,
      yend = nombre_label
    ),
    linewidth = 0.7,
    alpha = 0.55
  ) +
  geom_point(size = 4, alpha = 0.95) +
  geom_text(
    aes(label = round(airbnb_por_1000_hab, 1)),
    hjust = -0.35,
    size = 3.2,
    color = color_texto
  ) +
  scale_color_manual(
    values = c(
      "Muy alta" = color_principal,
      "Alta" = "#E67E22"
    ),
    name = "Nivel de presión"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Barrios con mayor presión turística relativa",
    subtitle = paste0(
      "Top 15 barrios según alojamientos Airbnb por 1.000 habitantes. ",
      "La línea discontinua indica la media municipal: ",
      round(media_global, 1)
    ),
    x = "Airbnb por 1.000 habitantes",
    y = NULL,
    caption = "Fuente: elaboración propia a partir de Inside Airbnb y padrón municipal."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = color_texto, size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey55", size = 8, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 30, 10, 10)
  )

g2_lollipop

ggsave(
  "outputs/figuras_finales/02_lollipop_top15_airbnb_1000.png",
  g2_lollipop,
  width = 9,
  height = 7,
  dpi = 300
)


# 3
# Mapa bivariante: presión Airbnb + renta


if ("renta_neta_persona" %in% names(renta_distritos_2023)) {
  
  renta_distritos <- renta_distritos_2023 %>%
    mutate(
      renta_neta = as.numeric(renta_neta_persona)
    )
  
} else {
  
  renta_distritos <- renta_distritos_2023 %>%
    mutate(
      renta_neta = parse_number(
        as.character(Total),
        locale = locale(grouping_mark = ".", decimal_mark = ",")
      )
    )
}

renta_distritos <- renta_distritos %>%
  mutate(
    codigo_distrito = str_extract(Distritos, "46250\\d{2}"),
    coddistrit = as.character(
      as.numeric(str_sub(codigo_distrito, 6, 7))
    ),
    clase_renta_num = ntile(renta_neta, 3),
    clase_renta = factor(
      clase_renta_num,
      levels = c(1, 2, 3),
      labels = c("Baja", "Media", "Alta")
    )
  ) %>%
  select(coddistrit, Distritos, renta_neta, clase_renta)

barrios_bivar <- barrios_indicadores %>%
  left_join(renta_distritos, by = "coddistrit") %>%
  mutate(
    clase_airbnb_num = ntile(airbnb_por_1000_hab, 3),
    clase_airbnb = factor(
      clase_airbnb_num,
      levels = c(1, 2, 3),
      labels = c("Baja", "Media", "Alta")
    ),
    clase_bivar = paste(clase_airbnb, clase_renta, sep = " - ")
  )

paleta_bivar <- c(
  "Baja - Baja"   = "#e8e8e8",
  "Media - Baja"  = "#b0d5df",
  "Alta - Baja"   = "#64acbe",
  "Baja - Media"  = "#e4cfcf",
  "Media - Media" = "#a5add3",
  "Alta - Media"  = "#627fba",
  "Baja - Alta"   = "#de9d9b",
  "Media - Alta"  = "#985356",
  "Alta - Alta"   = "#574249"
)

barrios_bivar_25830 <- barrios_bivar %>%
  st_transform(25830)

g3_bivar_mapa <- ggplot() +
  geom_sf(
    data = barrios_bivar_25830,
    aes(fill = clase_bivar),
    color = "white",
    linewidth = 0.20
  ) +
  scale_fill_manual(
    values = paleta_bivar,
    drop = FALSE,
    na.value = "grey90"
  ) +
  coord_sf(
    xlim = c(bbox_urbano["xmin"], bbox_urbano["xmax"]),
    ylim = c(bbox_urbano["ymin"], bbox_urbano["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Presión Airbnb y renta en València",
    subtitle = "Mapa bivariante: Airbnb por 1.000 habitantes y renta neta media por persona",
    caption = "La renta se incorpora a nivel distrital. Fuente: elaboración propia."
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = color_texto, size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey55", size = 8, hjust = 0),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

leyenda_bivar <- expand_grid(
  clase_airbnb = factor(
    c("Baja", "Media", "Alta"),
    levels = c("Baja", "Media", "Alta")
  ),
  clase_renta = factor(
    c("Baja", "Media", "Alta"),
    levels = c("Baja", "Media", "Alta")
  )
) %>%
  mutate(
    clase_bivar = paste(clase_airbnb, clase_renta, sep = " - ")
  )

g3_bivar_leyenda <- ggplot(
  leyenda_bivar,
  aes(x = clase_renta, y = clase_airbnb, fill = clase_bivar)
) +
  geom_tile(color = "white", linewidth = 0.7) +
  scale_fill_manual(values = paleta_bivar, guide = "none") +
  labs(
    x = "Renta",
    y = "Presión Airbnb"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.title = element_text(face = "bold", size = 8),
    axis.text = element_text(size = 7),
    panel.grid = element_blank(),
    plot.margin = margin(60, 5, 60, 5)
  )

g3_bivar <- g3_bivar_mapa + g3_bivar_leyenda +
  plot_layout(widths = c(5.5, 1))

g3_bivar

ggsave(
  "outputs/figuras_finales/03_mapa_bivariante_airbnb_renta.png",
  g3_bivar,
  width = 10,
  height = 7,
  dpi = 300
)

tabla_bivar <- barrios_bivar %>%
  st_drop_geometry() %>%
  count(clase_airbnb, clase_renta, clase_bivar, sort = TRUE)

write_csv(
  tabla_bivar,
  "outputs/tablas/tabla_bivariante_airbnb_renta.csv"
)


# A1
# KDE: densidad de kernel de puntos Airbnb


bbox_sf <- st_as_sfc(bbox_urbano)

airbnb_urbano <- airbnb_25830 %>%
  st_filter(bbox_sf)

coords_airbnb <- st_coordinates(airbnb_urbano)

ventana <- spatstat.geom::owin(
  xrange = c(bbox_urbano["xmin"], bbox_urbano["xmax"]),
  yrange = c(bbox_urbano["ymin"], bbox_urbano["ymax"])
)

airbnb_ppp <- spatstat.geom::ppp(
  x = coords_airbnb[, 1],
  y = coords_airbnb[, 2],
  window = ventana
)

sigma_kde <- 300

densidad_kde <- spatstat.explore::density.ppp(
  airbnb_ppp,
  sigma = sigma_kde,
  eps = 75
)

kde_df <- as.data.frame(densidad_kde)
names(kde_df) <- c("x", "y", "densidad")

kde_df <- kde_df %>%
  mutate(
    densidad_relativa = densidad / max(densidad, na.rm = TRUE) * 100
  )

valencia_union <- st_union(barrios_25830)

kde_sf <- kde_df %>%
  st_as_sf(coords = c("x", "y"), crs = 25830) %>%
  st_filter(valencia_union)

kde_plot_df <- st_drop_geometry(kde_sf) %>%
  bind_cols(
    st_coordinates(kde_sf) %>%
      as.data.frame() %>%
      rename(x = X, y = Y)
  )

gA1_kde <- ggplot() +
  geom_sf(
    data = barrios_25830,
    fill = "#F7F7F7",
    color = "white",
    linewidth = 0.25
  ) +
  geom_tile(
    data = kde_plot_df,
    aes(x = x, y = y, fill = densidad_relativa),
    alpha = 0.65
  ) +
  geom_contour(
    data = kde_plot_df,
    aes(x = x, y = y, z = densidad_relativa),
    color = "#2C3E50",
    linewidth = 0.25,
    alpha = 0.7
  ) +
  scale_fill_gradientn(
    colors = c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
    labels = label_number(accuracy = 1, suffix = "%"),
    name = "Concentración\nrelativa"
  ) +
  coord_sf(
    xlim = c(bbox_urbano["xmin"], bbox_urbano["xmax"]),
    ylim = c(bbox_urbano["ymin"], bbox_urbano["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Superficie de concentración de alojamientos Airbnb",
    subtitle = paste0(
      "Densidad de kernel a partir de puntos individuales. Sigma = ",
      sigma_kde,
      " metros"
    ),
    caption = "Fuente: elaboración propia a partir de Inside Airbnb. Proyección: EPSG:25830."
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = color_texto, size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey55", size = 8, hjust = 0),
    legend.position = c(0.88, 0.45),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

gA1_kde

ggsave(
  "outputs/figuras_anexo/A1_kde_airbnb.png",
  gA1_kde,
  width = 7.5,
  height = 7.5,
  dpi = 300
)


# A2
# Mapa LISA: clústeres locales


vecinos <- poly2nb(
  barrios_25830,
  queen = TRUE
)

pesos <- nb2listw(
  vecinos,
  style = "W",
  zero.policy = TRUE
)

x <- barrios_25830$airbnb_por_1000_hab

lisa <- localmoran(
  x,
  pesos,
  zero.policy = TRUE
)

columna_p <- grep("Pr", colnames(lisa), value = TRUE)[1]

x_std <- scale(x)[, 1]

wx_std <- lag.listw(
  pesos,
  x_std,
  zero.policy = TRUE
)

alpha_sig <- 0.05

barrios_lisa <- barrios_25830 %>%
  mutate(
    x_std = x_std,
    wx_std = wx_std,
    p_value = lisa[, columna_p],
    cluster = case_when(
      p_value > alpha_sig ~ "No significativo",
      x_std > 0 & wx_std > 0 ~ "Alto-Alto",
      x_std < 0 & wx_std < 0 ~ "Bajo-Bajo",
      x_std > 0 & wx_std < 0 ~ "Alto-Bajo",
      x_std < 0 & wx_std > 0 ~ "Bajo-Alto",
      TRUE ~ "No significativo"
    ),
    cluster = factor(
      cluster,
      levels = c(
        "Alto-Alto",
        "Bajo-Bajo",
        "Alto-Bajo",
        "Bajo-Alto",
        "No significativo"
      )
    )
  )

paleta_lisa <- c(
  "Alto-Alto" = "#C0392B",
  "Bajo-Bajo" = "#2980B9",
  "Alto-Bajo" = "#E67E22",
  "Bajo-Alto" = "#27AE60",
  "No significativo" = "grey85"
)

gA2_lisa <- ggplot() +
  geom_sf(
    data = barrios_lisa,
    aes(fill = cluster),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values = paleta_lisa,
    name = "Tipo de clúster",
    drop = TRUE
  ) +
  coord_sf(
    xlim = c(bbox_urbano["xmin"], bbox_urbano["xmax"]),
    ylim = c(bbox_urbano["ymin"], bbox_urbano["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Clústeres locales de presión Airbnb",
    subtitle = "I de Moran local para Airbnb por 1.000 habitantes",
    caption = "Alto-Alto: barrios con presión elevada rodeados de barrios también elevados. Fuente: elaboración propia."
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = color_texto, size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey55", size = 8, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

gA2_lisa

ggsave(
  "outputs/figuras_anexo/A2_mapa_lisa_airbnb.png",
  gA2_lisa,
  width = 7.5,
  height = 7.5,
  dpi = 300
)

tabla_lisa <- barrios_lisa %>%
  st_drop_geometry() %>%
  filter(cluster != "No significativo") %>%
  select(nombre, airbnb_por_1000_hab, cluster, p_value) %>%
  arrange(cluster, p_value)

write_csv(
  tabla_lisa,
  "outputs/tablas/tabla_lisa_airbnb.csv"
)


# A3
# Diagrama de Moran


moran_df <- tibble(
  x_std = x_std,
  wx_std = wx_std,
  nombre = barrios_25830$nombre
) %>%
  mutate(
    cuadrante = case_when(
      x_std > 0 & wx_std > 0 ~ "Alto-Alto",
      x_std < 0 & wx_std < 0 ~ "Bajo-Bajo",
      x_std > 0 & wx_std < 0 ~ "Alto-Bajo",
      x_std < 0 & wx_std > 0 ~ "Bajo-Alto",
      TRUE ~ "Sin clasificar"
    ),
    cuadrante = factor(
      cuadrante,
      levels = c(
        "Alto-Alto",
        "Bajo-Bajo",
        "Alto-Bajo",
        "Bajo-Alto",
        "Sin clasificar"
      )
    )
  )

mi <- moran.test(
  x,
  pesos,
  zero.policy = TRUE
)

mi_I <- round(mi$estimate["Moran I statistic"], 3)
mi_p <- round(mi$p.value, 4)

outliers <- moran_df %>%
  filter(abs(x_std) > 2 | abs(wx_std) > 2)

paleta_moran <- c(
  "Alto-Alto" = "#C0392B",
  "Bajo-Bajo" = "#2980B9",
  "Alto-Bajo" = "#E67E22",
  "Bajo-Alto" = "#27AE60",
  "Sin clasificar" = "grey70"
)

gA3_moran <- ggplot(moran_df, aes(x = x_std, y = wx_std)) +
  geom_hline(
    yintercept = 0,
    color = "grey60",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 0,
    color = "grey60",
    linewidth = 0.5,
    linetype = "dashed"
  ) +
  geom_point(
    aes(color = cuadrante),
    size = 2.5,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    color = color_texto,
    linewidth = 0.9,
    se = TRUE,
    fill = "grey85",
    alpha = 0.4
  ) +
  geom_label_repel(
    data = outliers,
    aes(label = str_to_title(str_to_lower(nombre))),
    size = 2.5,
    fill = "white",
    alpha = 0.85,
    label.size = 0,
    color = color_texto,
    max.overlaps = 10,
    segment.color = "grey50"
  ) +
  scale_color_manual(
    values = paleta_moran,
    name = "Cuadrante"
  ) +
  labs(
    title = "Diagrama de dispersión de Moran",
    subtitle = paste0(
      "Airbnb por 1.000 habitantes. I de Moran = ",
      mi_I,
      " (p = ",
      mi_p,
      ")"
    ),
    x = "Airbnb por 1.000 habitantes estandarizado",
    y = "Lag espacial estandarizado",
    caption = "Pesos espaciales: contigüidad queen, normalización W por fila."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = color_texto, size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey55", size = 8, hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank()
  )

gA3_moran

ggsave(
  "outputs/figuras_anexo/A3_moran_plot_airbnb.png",
  gA3_moran,
  width = 8,
  height = 7,
  dpi = 300
)


# A4
# Evolución VUT y alquiler municipal


vut_year_col <- if ("anio_alta" %in% names(vut_valencia_anual)) {
  "anio_alta"
} else {
  names(vut_valencia_anual)[1]
}

vut_value_col <- names(vut_valencia_anual)[
  names(vut_valencia_anual) != vut_year_col &
    map_lgl(vut_valencia_anual, is.numeric)
][1]

vut_serie <- vut_valencia_anual %>%
  transmute(
    anio = as.integer(.data[[vut_year_col]]),
    vut = as.numeric(.data[[vut_value_col]])
  ) %>%
  filter(!is.na(anio), !is.na(vut))

alq_year_col <- if ("AÑO" %in% names(alquiler_municipal_anual)) {
  "AÑO"
} else {
  names(alquiler_municipal_anual)[1]
}

alq_value_col <- if ("alquiler_medio" %in% names(alquiler_municipal_anual)) {
  "alquiler_medio"
} else {
  names(alquiler_municipal_anual)[
    names(alquiler_municipal_anual) != alq_year_col &
      map_lgl(alquiler_municipal_anual, is.numeric)
  ][1]
}

alq_serie <- alquiler_municipal_anual %>%
  transmute(
    anio = as.integer(.data[[alq_year_col]]),
    alquiler_medio = as.numeric(.data[[alq_value_col]])
  ) %>%
  filter(!is.na(anio), !is.na(alquiler_medio))

datos_evolucion <- inner_join(
  vut_serie,
  alq_serie,
  by = "anio"
)

vut_rango <- range(datos_evolucion$vut, na.rm = TRUE)
alq_rango <- range(datos_evolucion$alquiler_medio, na.rm = TRUE)

escala_alq_a_vut <- function(x) {
  (x - alq_rango[1]) / diff(alq_rango) * diff(vut_rango) + vut_rango[1]
}

escala_vut_a_alq <- function(x) {
  (x - vut_rango[1]) / diff(vut_rango) * diff(alq_rango) + alq_rango[1]
}

datos_evolucion <- datos_evolucion %>%
  mutate(
    alquiler_escalado = escala_alq_a_vut(alquiler_medio)
  )

gA4_evolucion <- ggplot(datos_evolucion, aes(x = anio)) +
  geom_area(
    aes(y = vut),
    fill = color_principal,
    alpha = 0.15
  ) +
  geom_line(
    aes(y = vut, color = "Altas VUT"),
    linewidth = 1.1
  ) +
  geom_point(
    aes(y = vut, color = "Altas VUT"),
    size = 2.5
  ) +
  geom_line(
    aes(y = alquiler_escalado, color = "Alquiler medio"),
    linewidth = 1.1,
    linetype = "dashed"
  ) +
  geom_point(
    aes(y = alquiler_escalado, color = "Alquiler medio"),
    size = 2.5
  ) +
  scale_y_continuous(
    name = "Altas anuales de VUT",
    labels = label_number(big.mark = ".", decimal.mark = ","),
    sec.axis = sec_axis(
      transform = ~ escala_vut_a_alq(.),
      name = "Alquiler medio",
      labels = label_number(big.mark = ".", decimal.mark = ",")
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      min(datos_evolucion$anio, na.rm = TRUE),
      max(datos_evolucion$anio, na.rm = TRUE),
      by = 1
    )
  ) +
  scale_color_manual(
    values = c(
      "Altas VUT" = color_principal,
      "Alquiler medio" = color_secundario
    ),
    name = NULL
  ) +
  labs(
    title = "Evolución de VUT y alquiler residencial en València",
    subtitle = "Comparación temporal de las altas de viviendas turísticas y el alquiler medio municipal",
    x = NULL,
    caption = "Fuente: elaboración propia a partir de datos de viviendas turísticas y alquiler municipal."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = color_texto, size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey55", size = 8, hjust = 0),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

gA4_evolucion

ggsave(
  "outputs/figuras_anexo/A4_evolucion_vut_alquiler.png",
  gA4_evolucion,
  width = 10,
  height = 6,
  dpi = 300
)


# A5
# Mapa de densidad Airbnb por km2


gA5_mapa_km2 <- ggplot() +
  geom_sf(
    data = barrios_25830,
    aes(fill = airbnb_por_km2),
    color = "white",
    linewidth = 0.25
  ) +
  scale_fill_gradientn(
    colors = c("#FFF7EC", "#FDD49E", "#FC8D59", "#E34A33", "#B30000"),
    trans = "sqrt",
    labels = label_number(big.mark = ".", decimal.mark = ",", accuracy = 1),
    name = "Airbnb\npor km²"
  ) +
  coord_sf(
    xlim = c(bbox_urbano["xmin"], bbox_urbano["xmax"]),
    ylim = c(bbox_urbano["ymin"], bbox_urbano["ymax"]),
    expand = FALSE
  ) +
  labs(
    title = "Densidad de alojamientos Airbnb por km²",
    subtitle = "Alojamientos por kilómetro cuadrado en el área urbana de València",
    caption = "Fuente: elaboración propia a partir de Inside Airbnb y cartografía municipal."
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = color_texto, size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption = element_text(color = "grey55", size = 8, hjust = 0),
    legend.position = c(0.88, 0.45),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    plot.margin = margin(10, 10, 10, 10)
  )

gA5_mapa_km2

ggsave(
  "outputs/figuras_anexo/A5_mapa_densidad_airbnb_km2.png",
  gA5_mapa_km2,
  width = 7.5,
  height = 7.5,
  dpi = 300
)

