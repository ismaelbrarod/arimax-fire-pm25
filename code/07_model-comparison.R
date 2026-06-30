# ==============================================================================
# 07_model-comparison.R
# COMPARACIÓN ARIMAX VS ARIMAX LAGGED
# ==============================================================================

source("code/00_settings.R")

load(file.path(DIR_PROCESSED, "arimax_results.RData"))
load(file.path(DIR_PROCESSED, "arimax_lagged_results.RData"))

# ==============================================================================
# RESUMEN MODELOS
# ==============================================================================

cat("\n================ ARIMAX SIN REZAGO ================\n")
print(summary(modelo_arimax))
TS.summary(pm_arimax_ts, modelo_arimax, fixed = c(NA, NA, NA))

cat("\n================ ARIMAX LAGGED ================\n")
print(summary(modelo_arimax_lagged))
TS.summary(pm_arimax_lagged_ts, modelo_arimax_lagged, fixed = c(NA, NA, NA))

# ==============================================================================
# FRACCIÓN ATRIBUIBLE
# ==============================================================================

cat(
  glue("\nFracción atribuible (sin rezago): {round(100 * fraccion_atribuible_arimax, 2)}%\n")
)

cat(
  glue("Fracción atribuible (con rezago): {round(100 * fraccion_atribuible_arimax_lagged, 2)}%\n")
)

# ==============================================================================
# DÍAS CON EXCESO
# ==============================================================================

fechas_arimax <- df_comparacion_arimax %>%
  filter(exceso_incendio > 0) %>%
  pull(fecha)

fechas_lagged <- df_comparacion_arimax_lagged %>%
  filter(exceso_incendio > 0) %>%
  pull(fecha)

fechas_comunes <- intersect(fechas_arimax, fechas_lagged)
fechas_solo_arimax <- setdiff(fechas_arimax, fechas_lagged)
fechas_solo_lagged <- setdiff(fechas_lagged, fechas_arimax)

cat("\n================ COINCIDENCIA DE EVENTOS ================\n")
print(list(
  comunes = fechas_comunes,
  solo_arimax = fechas_solo_arimax,
  solo_lagged = fechas_solo_lagged
))

# ==============================================================================
# TABLA RESUMEN EPIDEMIOLÓGICO
# ==============================================================================

resumen_modelos <- tibble(
  Modelo = c("ARIMAX", "ARIMAX lagged"),
  
  Días = c(
    nrow(df_comparacion_arimax %>% filter(exceso_incendio > 0)),
    nrow(df_comparacion_arimax_lagged %>% filter(exceso_incendio > 0))
  ),
  
  Abs = c(
    sum(df_comparacion_arimax$exceso_incendio, na.rm = TRUE),
    sum(df_comparacion_arimax_lagged$exceso_incendio, na.rm = TRUE)
  ),
  
  Rel = c(
    fraccion_atribuible_arimax,
    fraccion_atribuible_arimax_lagged
  ),
  
  Prom = c(
    mean(df_comparacion_arimax$fraccion_dia[df_comparacion_arimax$fraccion_dia > 0], na.rm = TRUE),
    mean(df_comparacion_arimax_lagged$fraccion_dia[df_comparacion_arimax_lagged$fraccion_dia > 0], na.rm = TRUE)
  ),
  
  Max = c(
    max(df_comparacion_arimax$fraccion_dia, na.rm = TRUE),
    max(df_comparacion_arimax_lagged$fraccion_dia, na.rm = TRUE)
  )
)

write.csv(
  resumen_modelos,
  file = file.path(DIR_TABLES, "resumen_modelos_arimax.csv"),
  row.names = FALSE
)

# ==============================================================================
# TOP EVENTOS
# ==============================================================================

top_arimax <- df_comparacion_arimax %>%
  arrange(desc(fraccion_dia)) %>%
  slice_head(n = 5) %>%
  mutate(modelo = "ARIMAX")

top_lagged <- df_comparacion_arimax_lagged %>%
  arrange(desc(fraccion_dia)) %>%
  slice_head(n = 5) %>%
  mutate(modelo = "ARIMAX lagged")

tabla_top <- bind_rows(top_arimax, top_lagged) %>%
  arrange(desc(fraccion_dia)) %>%
  select(modelo, everything())

write.csv(
  tabla_top,
  file = file.path(DIR_TABLES, "top_eventos_arimax.csv"),
  row.names = FALSE
)

# ==============================================================================
# RESUMEN COINCIDENCIAS
# ==============================================================================

resumen_fechas <- tibble(
  Coinciden = length(fechas_comunes),
  Solo_ARIMAX = length(fechas_solo_arimax),
  Solo_Lagged = length(fechas_solo_lagged)
)

write.csv(
  resumen_fechas,
  file = file.path(DIR_TABLES, "resumen_coincidencias_arimax.csv"),
  row.names = FALSE
)

# ==============================================================================
# DELTA FRACCIÓN ATRIBUIBLE
# ==============================================================================

delta_fa_tbl <- tibble(
  fraccion_atribuible_arimax = fraccion_atribuible_arimax,
  fraccion_atribuible_lagged = fraccion_atribuible_arimax_lagged,
  delta = fraccion_atribuible_arimax_lagged - fraccion_atribuible_arimax
)

write.csv(
  delta_fa_tbl,
  file = file.path(DIR_TABLES, "delta_fraccion_atribuible.csv"),
  row.names = FALSE
)

# ==============================================================================
# ===================== MÉTRICAS ESTADÍSTICAS DEL MODELO ======================
# ==============================================================================

metricas_modelos <- tibble(
  Modelo = c("ARIMAX", "ARIMAX lagged"),
  
  AIC = c(
    AIC(modelo_arimax),
    AIC(modelo_arimax_lagged)
  ),
  
  BIC = c(
    BIC(modelo_arimax),
    BIC(modelo_arimax_lagged)
  ),
  
  LogLik = c(
    as.numeric(logLik(modelo_arimax)),
    as.numeric(logLik(modelo_arimax_lagged))
  )
)

# ==============================================================================
# MÉTRICAS DE ERROR PREDICTIVO
# ==============================================================================

rmse <- function(actual, pred) {
  sqrt(mean((actual - pred)^2, na.rm = TRUE))
}

mae <- function(actual, pred) {
  mean(abs(actual - pred), na.rm = TRUE)
}

metricas_error <- tibble(
  Modelo = c("ARIMAX", "ARIMAX lagged"),
  
  RMSE = c(
    rmse(df_comparacion_arimax$pm25, df_comparacion_arimax$fitted),
    rmse(df_comparacion_arimax_lagged$pm25, df_comparacion_arimax_lagged$fitted)
  ),
  
  MAE = c(
    mae(df_comparacion_arimax$pm25, df_comparacion_arimax$fitted),
    mae(df_comparacion_arimax_lagged$pm25, df_comparacion_arimax_lagged$fitted)
  )
)

# ==============================================================================
# MÉTRICAS COMPLETAS
# ==============================================================================

metricas_completas <- metricas_modelos %>%
  left_join(metricas_error, by = "Modelo")

write.csv(
  metricas_completas,
  file = file.path(DIR_TABLES, "metricas_modelos_arimax.csv"),
  row.names = FALSE
)

# ==============================================================================
# RESUMEN FINAL
# ==============================================================================

model_comparison_results <- list(
  resumen_modelos = resumen_modelos,
  metricas_modelos = metricas_completas,
  tabla_top = tabla_top,
  coincidencias = resumen_fechas,
  delta_fa = delta_fa_tbl
)

save(
  model_comparison_results,
  file = file.path(DIR_PROCESSED, "model_comparison_results.RData")
)