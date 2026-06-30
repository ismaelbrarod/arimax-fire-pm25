# ==============================================================================
# 05_arimax.R
# ==============================================================================

source("code/00_settings.R")

load(file.path(DIR_PROCESSED, "processed_data.RData"))
load(file.path(DIR_PROCESSED, "timeseries_objects.RData"))

# ==============================================================================
# DATA BASE
# ==============================================================================

df_arimax <- datos_modelo

# ==============================================================================
# REGRESORES
# ==============================================================================

xreg_arimax <- cbind(
  incendio = df_arimax$incendio,
  temp     = temp_ts,
  hum_rel  = hum_rel_ts,
  prec     = prec_ts,
  pres_atm = pres_atm_ts,
  vel_vien = vel_vien_ts
)

pm_arimax_ts <- pm_ts

# ==============================================================================
# MODELO
# ==============================================================================

modelo_arimax <- auto.arima(
  y = pm_arimax_ts,
  xreg = xreg_arimax,
  d = 0,
  lambda = 0,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(modelo_arimax)
TS.summary(pm_arimax_ts, modelo_arimax, fixed = c(NA, NA, NA))

# ==============================================================================
# DIAGNÓSTICOS
# ==============================================================================

res_arimax <- residuals(modelo_arimax)

ts.diag(res_arimax, lag = 20)
acf(res_arimax, lag.max = 365)
pacf(res_arimax, lag.max = 365)
periodogram(res_arimax)

# ==============================================================================
# PREDICCIÓN
# ==============================================================================

fc_arimax <- forecast(
  modelo_arimax,
  xreg = xreg_arimax,
  h = 0,
  level = 95
)

pred_arimax <- as_tibble(fc_arimax) %>%
  mutate(fecha = df_arimax$fecha) %>%
  select(fecha,
         fitted = `Point Forecast`,
         lower  = `Lo 95`,
         upper  = `Hi 95`)

# ==============================================================================
# AJUSTE + RESIDUOS
# ==============================================================================

df_arimax <- df_arimax %>%
  mutate(
    ajustado = as.numeric(fitted(modelo_arimax)),
    residuo  = as.numeric(residuals(modelo_arimax))
  )

# ==============================================================================
# COMPARACIÓN
# ==============================================================================

df_comparacion_arimax <- df_arimax %>%
  select(fecha, pm25) %>%
  left_join(pred_arimax, by = "fecha") %>%
  mutate(
    error = pm25 - fitted,
    exceso_incendio = if_else(pm25 > upper, pm25 - upper, 0),
    fraccion_dia = if_else(pm25 > upper, exceso_incendio / pm25, 0)
  )

fraccion_atribuible_arimax <- sum(df_comparacion_arimax$exceso_incendio, na.rm = TRUE) /
  sum(df_comparacion_arimax$pm25, na.rm = TRUE)

# ==============================================================================
# FIGURA
# ==============================================================================

p_arimax <- ggplot(df_comparacion_arimax, aes(x = fecha)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "IC 95%"), alpha = 0.3) +
  geom_line(aes(y = pm25, color = "Observado"), linewidth = 0.4) +
  geom_line(aes(y = fitted, color = "Ajustado"), linewidth = 0.5) +
  geom_point(
    data = subset(df_comparacion_arimax, exceso_incendio > 0),
    aes(y = pm25, shape = "Exceso por incendio"),
    color = "red", size = 1
  ) +
  labs(
    title = "ARIMAX (2,0,2) Sin Rezagos",
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
  filename = file.path(DIR_FIGURES, "ajuste_arimax.png"),
  plot = p_arimax,
  width = 10,
  height = 4,
  dpi = 300
)

# ==============================================================================
# GUARDADO
# ==============================================================================

save(
  modelo_arimax,
  xreg_arimax,
  pm_arimax_ts,
  df_arimax,
  pred_arimax,
  df_comparacion_arimax,
  fraccion_atribuible_arimax,
  file = file.path(DIR_OUTPUT, "arimax_results.RData")
)