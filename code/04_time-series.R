# ==============================================================================
# 04_time-series.R
# ==============================================================================

source("code/00_settings.R")

load(
  file.path(
    DIR_PROCESSED,
    "processed_data.RData"
  )
)

# ==============================================================================
# DESCOMPOSICIÓN STL
# ==============================================================================

pm_stl <- stl(
  pm_ts,
  s.window = "periodic"
)

png(
  file.path(DIR_FIGURES, "stl_pm25.png"),
  width  = 10,
  height = 8,
  units  = "in",
  res    = 300
)

par(mar = c(3, 4, 2, 1))

plot(
  pm_stl,
  main = "Descomposición STL — PM2.5"
)

dev.off()

# ==============================================================================
# ESTACIONARIEDAD
# ==============================================================================

ndiffs_kpss <- ndiffs(
  pm_ts,
  test = "kpss"
)

ndiffs_adf <- ndiffs(
  pm_ts,
  test = "adf"
)

ndiffs_pp <- ndiffs(
  pm_ts,
  test = "pp"
)

nsdiffs_pm <- nsdiffs(pm_ts)

# ==============================================================================
# TRANSFORMACIÓN BOX-COX
# ==============================================================================

lambda_guerrero <- BoxCox.lambda(
  pm_ts,
  method = "guerrero"
)

png(
  file.path(DIR_FIGURES, "boxcox_pm25.png"),
  width  = 10,
  height = 5,
  units  = "in",
  res    = 300
)

par(
  mar = c(4,4,3,1),
  bg = "white"
)

MASS::boxcox(
  pm_ts ~ 1,
  lambda = seq(-2, 2, by = 0.1)
)

abline(
  v = lambda_guerrero,
  col = "#e74c3c",
  lwd = 1.5,
  lty = 2
)

mtext(
  paste0(
    "Box-Cox — PM2.5 (λ = ",
    round(lambda_guerrero,3),
    ")"
  ),
  side = 3,
  line = 0.8,
  cex = 1,
  font = 2
)

dev.off()

# ==============================================================================
# SERIE DIFERENCIADA
# ==============================================================================

pm_ts_diff <- diff(pm_ts)

# ==============================================================================
# ACF, PACF Y PERIODOGRAMA
# ==============================================================================

png(
  file.path(
    DIR_FIGURES,
    "acf_pacf_periodograma.png"
  ),
  width  = 14,
  height = 8,
  units  = "in",
  res    = 300
)

par(
  mfrow = c(2,3),
  mar = c(4,4,3,1),
  bg = "white"
)

acf(
  pm_ts,
  lag.max = 365,
  main = "",
  xlab = "Rezago",
  ylab = "Autocorrelación"
)

mtext(
  "ACF — Original",
  side = 3,
  line = 0.5,
  font = 2
)

pacf(
  pm_ts,
  lag.max = 365,
  main = "",
  xlab = "Rezago",
  ylab = "Autocorrelación parcial"
)

mtext(
  "PACF — Original",
  side = 3,
  line = 0.5,
  font = 2
)

TSA::periodogram(
  pm_ts,
  main = "",
  xlab = "Frecuencia",
  ylab = "Densidad espectral"
)

mtext(
  "Periodograma — Original",
  side = 3,
  line = 0.5,
  font = 2
)

acf(
  pm_ts_diff,
  lag.max = 365,
  main = "",
  xlab = "Rezago",
  ylab = "Autocorrelación"
)

mtext(
  "ACF — Diferenciada",
  side = 3,
  line = 0.5,
  font = 2
)

pacf(
  pm_ts_diff,
  lag.max = 365,
  main = "",
  xlab = "Rezago",
  ylab = "Autocorrelación parcial"
)

mtext(
  "PACF — Diferenciada",
  side = 3,
  line = 0.5,
  font = 2
)

TSA::periodogram(
  pm_ts_diff,
  main = "",
  xlab = "Frecuencia",
  ylab = "Densidad espectral"
)

mtext(
  "Periodograma — Diferenciada",
  side = 3,
  line = 0.5,
  font = 2
)

par(mfrow = c(1,1))

dev.off()

# ==============================================================================
# CCF PM2.5 VS COVARIABLES
# ==============================================================================

png(
  file.path(
    DIR_FIGURES,
    "ccf_pm25_covariables.png"
  ),
  width  = 12,
  height = 10,
  units  = "in",
  res    = 300
)

par(
  mfrow = c(3,2),
  mar = c(4,4,3,1),
  oma = c(0,0,3,0),
  bg = "white"
)

ccf(
  pm_ts,
  temp_ts,
  lag.max = 10,
  main = "",
  xlab = "Rezago",
  ylab = "CCF"
)

mtext(
  "PM2.5 — Temperatura",
  side = 3,
  line = 0.5,
  font = 2
)

ccf(
  pm_ts,
  hum_rel_ts,
  lag.max = 10,
  main = "",
  xlab = "Rezago",
  ylab = "CCF"
)

mtext(
  "PM2.5 — Humedad relativa",
  side = 3,
  line = 0.5,
  font = 2
)

ccf(
  pm_ts,
  prec_ts,
  lag.max = 10,
  main = "",
  xlab = "Rezago",
  ylab = "CCF"
)

mtext(
  "PM2.5 — Precipitación",
  side = 3,
  line = 0.5,
  font = 2
)

ccf(
  pm_ts,
  pres_atm_ts,
  lag.max = 10,
  main = "",
  xlab = "Rezago",
  ylab = "CCF"
)

mtext(
  "PM2.5 — Presión atmosférica",
  side = 3,
  line = 0.5,
  font = 2
)

ccf(
  pm_ts,
  vel_vien_ts,
  lag.max = 10,
  main = "",
  xlab = "Rezago",
  ylab = "CCF"
)

mtext(
  "PM2.5 — Velocidad del viento",
  side = 3,
  line = 0.5,
  font = 2
)

plot.new()

mtext(
  "Correlación cruzada PM2.5 vs covariables meteorológicas",
  outer = TRUE,
  cex = 1.1,
  font = 2,
  line = 1
)

par(mfrow = c(1,1))

dev.off()

# ==============================================================================
# GUARDAR OBJETOS
# ==============================================================================

save(
  pm_stl,
  pm_ts_diff,
  lambda_guerrero,
  ndiffs_kpss,
  ndiffs_adf,
  ndiffs_pp,
  nsdiffs_pm,
  file = file.path(
    DIR_PROCESSED,
    "timeseries_objects.RData"
  )
)