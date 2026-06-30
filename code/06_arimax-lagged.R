# ==============================================================================
# 06_arimax_lagged.R
# ==============================================================================

source("code/00_settings.R")

load(file.path(DIR_PROCESSED, "processed_data.RData"))
load(file.path(DIR_PROCESSED, "timeseries_objects.RData"))

# ==============================================================================
# DATA BASE
# ==============================================================================

df_arimax_lagged <- tibble(
  fecha = datos_modelo$fecha,
  pm25  = datos_modelo$pm25,
  
  temp_lag0     = temp_ts,
  hum_rel_lag1  = dplyr::lag(as.numeric(hum_rel_ts), 1),
  prec_lag0     = prec_ts,
  pres_atm_lag0 = pres_atm_ts,
  vel_vien_lag0 = vel_vien_ts
) %>%
  tidyr::drop_na()

# ==============================================================================
# SERIE DEPENDIENTE
# ==============================================================================

pm_arimax_lagged_ts <- ts(df_arimax_lagged$pm25, start = c(2019, 1), frequency = 365)

# ==============================================================================
# REGRESORES
# ==============================================================================

xreg_arimax_lagged <- as.matrix(
  df_arimax_lagged %>%
    dplyr::select(
      temp_lag0,
      hum_rel_lag1,
      prec_lag0,
      pres_atm_lag0,
      vel_vien_lag0
    )
)

# ==============================================================================
# MODELO
# ==============================================================================

modelo_arimax_lagged <- auto.arima(
  y = pm_arimax_lagged_ts,
  xreg = xreg_arimax_lagged,
  d = 0,
  lambda = 0,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(modelo_arimax_lagged)
TS.summary(pm_arimax_lagged_ts, modelo_arimax_lagged, fixed = c(NA, NA, NA))

# ==============================================================================
# DIAGNÓSTICOS
# ==============================================================================

res_arimax_lagged <- residuals(modelo_arimax_lagged)

ts.diag(res_arimax_lagged, lag = 20)
acf(res_arimax_lagged, lag.max = 365)
pacf(res_arimax_lagged, lag.max = 365)
periodogram(res_arimax_lagged)

# ==============================================================================
# PREDICCIÓN
# ==============================================================================

fc_arimax_lagged <- forecast(
  modelo_arimax_lagged,
  xreg = xreg_arimax_lagged,
  h = 0,
  level = 95
)

pred_arimax_lagged <- as_tibble(fc_arimax_lagged) %>%
  mutate(fecha = df_arimax_lagged$fecha) %>%
  select(fecha,
         fitted = `Point Forecast`,
         lower  = `Lo 95`,
         upper  = `Hi 95`)

# ==============================================================================
# COMPARACIÓN
# ==============================================================================

df_comparacion_arimax_lagged <- df_arimax_lagged %>%
  select(fecha, pm25) %>%
  left_join(pred_arimax_lagged, by = "fecha") %>%
  mutate(
    error = pm25 - fitted,
    exceso_incendio = if_else(pm25 > upper, pm25 - upper, 0),
    fraccion_dia = if_else(pm25 > upper,
                           exceso_incendio / pm25,
                           0)
  )

fraccion_atribuible_arimax_lagged <- sum(df_comparacion_arimax_lagged$exceso_incendio, na.rm = TRUE) /
  sum(df_comparacion_arimax_lagged$pm25, na.rm = TRUE)

# ==============================================================================
# FIGURA
# ==============================================================================

p_arimax_lagged <- ggplot(df_comparacion_arimax_lagged, aes(x = fecha)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "IC 95%"), alpha = 0.3) +
  geom_line(aes(y = pm25, color = "Observado"), linewidth = 0.4) +
  geom_line(aes(y = fitted, color = "Ajustado"), linewidth = 0.5) +
  geom_point(
    data = subset(df_comparacion_arimax_lagged, exceso_incendio > 0),
    aes(y = pm25, shape = "Exceso por incendio"),
    color = "red", size = 1
  ) +
  labs(
    title = "ARIMAX (3,0,2) con Rezagos",
    x = NULL,
    y = expression(PM[2.5]~(µg/m^3)),
    color = "Serie",
    fill = "",
    shape = ""
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Observado" = "black", "Ajustado" = "red2")) +
  scale_fill_manual(values = c("IC 95%" = "steelblue")) +
  scale_shape_manual(values = c("Exceso por incendio" = 2)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(DIR_FIGURES, "ajuste_arimax_lagged.png"),
  plot = p_arimax_lagged,
  width = 10,
  height = 4,
  dpi = 300
)

# ==============================================================================
# GUARDADO
# ==============================================================================

save(
  modelo_arimax_lagged,
  xreg_arimax_lagged,
  pm_arimax_lagged_ts,
  df_arimax_lagged,
  pred_arimax_lagged,
  df_comparacion_arimax_lagged,
  fraccion_atribuible_arimax_lagged,
  file = file.path(DIR_PROCESSED, "arimax_lagged_results.RData")
)