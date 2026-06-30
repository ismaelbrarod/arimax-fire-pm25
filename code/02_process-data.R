# ==============================================================================
# 02_process-data.R
# ==============================================================================

source("code/00_settings.R")

load(
  file.path(
    DIR_PROCESSED,
    "raw_data.RData"
  )
)

# ==============================================================================
# FUNCIONES
# ==============================================================================

procesar_regional <- function(df) {
  
  df %>%
    mutate(
      fecha_yymmdd = as.character(fecha_yymmdd),
      fecha = as.Date(fecha_yymmdd, format = "%y%m%d"),
      registros_validados_num = as.numeric(as.character(registros_validados)),
      registros_preliminares_num = as.numeric(as.character(registros_preliminares)),
      registros_no_validados_num = as.numeric(as.character(registros_no_validados))
    ) %>%
    rowwise() %>%
    mutate(
      registro = case_when(
        !is.na(registros_validados_num)    ~ registros_validados_num,
        !is.na(registros_preliminares_num) ~ registros_preliminares_num,
        !is.na(registros_no_validados_num) ~ registros_no_validados_num,
        TRUE                               ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(fecha, registro)
  
}

procesar_meteorologico <- function(df, nombre_variable) {
  
  df %>%
    rename(
      fecha_txt = 1,
      hora_txt  = 2,
      valor     = 3
    ) %>%
    mutate(
      fecha = as.Date(
        str_pad(as.character(fecha_txt), 6, pad = "0"),
        format = "%y%m%d"
      ),
      valor = as.numeric(valor)
    ) %>%
    filter(!is.na(valor)) %>%
    filter(
      between(
        fecha,
        as.Date("2019-01-01"),
        as.Date("2024-12-31")
      )
    ) %>%
    group_by(fecha) %>%
    summarise(
      valor = mean(valor, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!(month(fecha) == 2 & day(fecha) == 29)) %>%
    complete(
      fecha = seq.Date(
        as.Date("2019-01-01"),
        as.Date("2024-12-31"),
        by = "day"
      )
    ) %>%
    filter(!(month(fecha) == 2 & day(fecha) == 29)) %>%
    arrange(fecha) %>%
    mutate(
      valor = na_interpolation(valor, option = "linear")
    ) %>%
    rename(
      !!nombre_variable := valor
    )
  
}

# ==============================================================================
# DATOS REGIONALES
# ==============================================================================

rm <- procesar_regional(rm)

vina <- procesar_regional(vina)

# ==============================================================================
# DATOS INCENDIOS
# ==============================================================================

meses_es_en <- c(
  "ene" = "Jan",
  "feb" = "Feb",
  "mar" = "Mar",
  "abr" = "Apr",
  "may" = "May",
  "jun" = "Jun",
  "jul" = "Jul",
  "ago" = "Aug",
  "sep" = "Sep",
  "oct" = "Oct",
  "nov" = "Nov",
  "dic" = "Dec"
)

fire <- fire %>%
  mutate(
    fecha_inicio = str_replace_all(fecha_inicio, meses_es_en),
    fecha_fin    = str_replace_all(fecha_fin, meses_es_en),
    fecha_inicio = as.Date(fecha_inicio, format = "%d-%b-%Y"),
    fecha_fin    = as.Date(fecha_fin, format = "%d-%b-%Y")
  ) %>%
  mutate(
    fecha_fin = if_else(
      is.na(fecha_fin) &
        comuna == "Cañete" &
        fecha_inicio == as.Date("2021-01-13"),
      as.Date("2021-01-15"),
      fecha_fin
    )
  )

fire_days <- fire %>%
  filter(
    !is.na(fecha_inicio),
    !is.na(fecha_fin)
  ) %>%
  rowwise() %>%
  mutate(
    fecha = list(
      seq(
        fecha_inicio,
        fecha_fin,
        by = "day"
      )
    )
  ) %>%
  unnest(fecha)

fire_dates <- unique(fire_days$fecha)

# ==============================================================================
# DATOS PM2.5
# ==============================================================================

pm <- pm %>%
  rename(
    fecha_raw    = 1,
    hora_raw     = 2,
    validados    = 3,
    preliminares = 4,
    no_validados = 5
  ) %>%
  mutate(
    fecha = as.Date(
      str_pad(as.character(fecha_raw), 6, pad = "0"),
      format = "%y%m%d"
    ),
    validados = as.numeric(str_trim(as.character(validados))),
    preliminares = as.numeric(str_trim(as.character(preliminares))),
    no_validados = as.numeric(str_trim(as.character(no_validados))),
    pm25 = coalesce(
      validados,
      preliminares,
      no_validados
    )
  ) %>%
  filter(
    between(
      fecha,
      as.Date("2019-01-01"),
      as.Date("2024-12-31")
    )
  ) %>%
  select(
    fecha,
    pm25
  ) %>%
  complete(
    fecha = seq.Date(
      as.Date("2019-01-01"),
      as.Date("2024-12-31"),
      by = "day"
    )
  ) %>%
  filter(!(month(fecha) == 2 & day(fecha) == 29)) %>%
  arrange(fecha) %>%
  mutate(
    pm25 = na_interpolation(pm25)
  )

pm_nobis <- pm

# ==============================================================================
# DATOS METEOROLÓGICOS
# ==============================================================================

temp_dia <- procesar_meteorologico(temp, "temp")

hum_dia <- procesar_meteorologico(hum, "hum_rel")

rain_dia <- procesar_meteorologico(rain, "prec")

press_dia <- procesar_meteorologico(press, "pres_atm")

wind_dia <- procesar_meteorologico(wind, "vel_vien")

# ==============================================================================
# DATASET PARA MODELACIÓN
# ==============================================================================

datos_modelo <- pm_nobis %>%
  mutate(
    incendio = as.integer(fecha %in% fire_dates)
  ) %>%
  left_join(temp_dia, by = "fecha") %>%
  left_join(hum_dia, by = "fecha") %>%
  left_join(rain_dia, by = "fecha") %>%
  left_join(press_dia, by = "fecha") %>%
  left_join(wind_dia, by = "fecha")

# ==============================================================================
# SERIES DE TIEMPO
# ==============================================================================

TS_START <- c(2019, 1)
TS_FREQ  <- 365

pm_ts <- ts(
  datos_modelo$pm25,
  start = TS_START,
  frequency = TS_FREQ
)

temp_ts <- ts(
  datos_modelo$temp,
  start = TS_START,
  frequency = TS_FREQ
)

hum_rel_ts <- ts(
  datos_modelo$hum_rel,
  start = TS_START,
  frequency = TS_FREQ
)

prec_ts <- ts(
  datos_modelo$prec,
  start = TS_START,
  frequency = TS_FREQ
)

pres_atm_ts <- ts(
  datos_modelo$pres_atm,
  start = TS_START,
  frequency = TS_FREQ
)

vel_vien_ts <- ts(
  datos_modelo$vel_vien,
  start = TS_START,
  frequency = TS_FREQ
)

# ==============================================================================
# GUARDAR DATOS PROCESADOS
# ==============================================================================

save(
  rm,
  vina,
  fire,
  fire_days,
  pm_nobis,
  datos_modelo,
  pm_ts,
  temp_ts,
  hum_rel_ts,
  prec_ts,
  pres_atm_ts,
  vel_vien_ts,
  file = file.path(
    DIR_PROCESSED,
    "processed_data.RData"
  )
)