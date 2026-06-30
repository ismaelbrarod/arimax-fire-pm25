# ==============================================================================
# 03_descriptive.R
# ==============================================================================

source("code/00_settings.R")

load(
  file.path(
    DIR_PROCESSED,
    "processed_data.RData"
  )
)

# ==============================================================================
# TEMA COMÚN PARA TODOS LOS GRÁFICOS
# ==============================================================================

tema_base <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle    = element_text(hjust = 0.5, color = "grey40"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    axis.title       = element_text(color = "grey20"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

COLOR_PRINCIPAL <- "#2c3e50"

# ==============================================================================
# TABLA: ESTADÍSTICAS DESCRIPTIVAS PM2.5
# ==============================================================================

tab_descriptiva <- tibble(
  Media             = mean(pm_ts),
  Mediana           = median(pm_ts),
  Mínimo            = min(pm_ts),
  Máximo            = max(pm_ts),
  Rango             = diff(range(pm_ts)),
  Varianza          = var(pm_ts),
  `Desv. Estándar`  = sd(pm_ts),
  Q1                = quantile(pm_ts, 0.25),
  Q3                = quantile(pm_ts, 0.75),
  `Coef. Variación` = sd(pm_ts) / mean(pm_ts)
)

tab_descriptiva %>%
  kable(
    digits = 2,
    caption = "Estadísticas descriptivas de PM2.5"
  )

write.csv(
  tab_descriptiva,
  file = file.path(
    DIR_TABLES,
    "descriptiva_pm25.csv"
  ),
  row.names = FALSE
)

# ==============================================================================
# FIGURA: HISTOGRAMA PM2.5
# ==============================================================================

png(
  file.path(DIR_FIGURES, "histograma_pm25.png"),
  width = 10,
  height = 5,
  units = "in",
  res = 300
)

par(mar = c(4,4,3,1))

hist(
  pm_ts,
  breaks = 30,
  col = COLOR_PRINCIPAL,
  border = "white",
  main = "Histograma de PM2.5",
  xlab = expression(PM[2.5]~(µg/m^3)),
  ylab = "Frecuencia",
  las = 1
)

dev.off()

# ==============================================================================
# FIGURA: SERIE PM2.5
# ==============================================================================

p_pm25 <-
  
  ggplot(
    datos_modelo,
    aes(fecha, pm25)
  ) +
  
  geom_line(
    color = COLOR_PRINCIPAL,
    linewidth = 0.4
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  labs(
    title = "Serie diaria de PM2.5",
    x = NULL,
    y = expression(PM[2.5]~(µg/m^3))
  ) +
  
  tema_base

ggsave(
  file.path(
    DIR_FIGURES,
    "serie_pm25.png"
  ),
  p_pm25,
  width = 10,
  height = 4,
  dpi = 300
)

# ==============================================================================
# FIGURA: SERIES METEOROLÓGICAS
# ==============================================================================

df_predictores <-
  
  datos_modelo %>%
  
  select(
    fecha,
    temp,
    hum_rel,
    prec,
    pres_atm,
    vel_vien
  ) %>%
  
  rename(
    Temperatura = temp,
    `Humedad relativa` = hum_rel,
    Precipitación = prec,
    `Presión atmosférica` = pres_atm,
    `Velocidad del viento` = vel_vien
  ) %>%
  
  pivot_longer(
    -fecha,
    names_to = "Variable",
    values_to = "Valor"
  )

p_predictores <-
  
  ggplot(
    df_predictores,
    aes(fecha, Valor)
  ) +
  
  geom_line(
    color = COLOR_PRINCIPAL,
    linewidth = 0.4
  ) +
  
  facet_wrap(
    ~Variable,
    scales = "free_y",
    ncol = 1
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  labs(
    title = "Series diarias de predictores meteorológicos",
    x = NULL,
    y = NULL
  ) +
  
  tema_base

ggsave(
  file.path(
    DIR_FIGURES,
    "series_predictores.png"
  ),
  p_predictores,
  width = 10,
  height = 16,
  dpi = 300
)

# ==============================================================================
# FIGURA: SERIES REGIONALES (VERANO)
# ==============================================================================

preparar_verano <- function(df){
  
  df %>%
    filter(
      year(fecha) %in% 2023:2024,
      month(fecha) %in% c(1,2)
    ) %>%
    mutate(
      anio = year(fecha),
      fecha_verano = as.Date(
        paste0(
          "2001-",
          month(fecha),
          "-",
          day(fecha)
        )
      )
    )
  
}

graficar_verano <- function(df_verano, estacion){
  
  max_registro <- max(df_verano$registro, na.rm = TRUE)
  
  ggplot(
    df_verano,
    aes(
      fecha_verano,
      registro,
      color = factor(anio)
    )
  ) +
    
    geom_line(linewidth = 0.6) +
    
    geom_vline(
      xintercept = as.Date("2001-02-02"),
      colour = "#2980b9",
      linetype = 2
    ) +
    
    geom_vline(
      xintercept = as.Date("2001-02-05"),
      colour = "#2980b9",
      linetype = 2
    ) +
    
    geom_vline(
      xintercept = as.Date("2001-02-01"),
      colour = "#c0392b",
      linetype = 2
    ) +
    
    geom_vline(
      xintercept = as.Date("2001-02-15"),
      colour = "#c0392b",
      linetype = 2
    ) +
    
    scale_color_manual(
      values = c(
        "2023" = "#e74c3c",
        "2024" = "#2c3e50"
      )
    ) +
    
    scale_x_date(
      date_labels = "%d-%b",
      date_breaks = "5 days"
    ) +
    
    labs(
      title = paste(
        "Concentración estival de PM2.5 —",
        estacion
      ),
      x = NULL,
      y = expression(PM[2.5]~(µg/m^3)),
      color = "Año"
    ) +
    
    tema_base
  
}

rm_verano <- preparar_verano(rm)

vina_verano <- preparar_verano(vina)

p_rm <- graficar_verano(
  rm_verano,
  "Parque O'Higgins"
)

p_vina <- graficar_verano(
  vina_verano,
  "Viña del Mar"
)

ggsave(
  file.path(
    DIR_FIGURES,
    "verano_rm.png"
  ),
  p_rm,
  width = 10,
  height = 5,
  dpi = 300
)

ggsave(
  file.path(
    DIR_FIGURES,
    "verano_vina.png"
  ),
  p_vina,
  width = 10,
  height = 5,
  dpi = 300
)