# ==============================================================================
# 01_load-data.R
# ==============================================================================

source("code/00_settings.R")

# ==============================================================================
# DATOS PM2.5 (RM y VALPO)
# ==============================================================================

rm <- import(
  file.path(DIR_RAW, "met.csv")
) %>%
  clean_names()

vina <- import(
  file.path(DIR_RAW, "vina.csv")
) %>%
  clean_names()

# ==============================================================================
# DATOS INCENDIOS
# ==============================================================================

fire <- import(
  file.path(DIR_RAW, "incendios.xls")
)

# ==============================================================================
# DATOS PM2.5 (VIÑA)
# ==============================================================================

pm <- read.csv2(
  file.path(DIR_RAW, "pm25.csv"),
  stringsAsFactors = FALSE
)

# ==============================================================================
# DATOS METEOROLÓGICOS
# ==============================================================================

temp <- read.csv2(
  file.path(DIR_RAW, "temp.csv"),
  stringsAsFactors = FALSE
)

hum <- read.csv2(
  file.path(DIR_RAW, "hum_rel.csv"),
  stringsAsFactors = FALSE
)

rain <- read.csv2(
  file.path(DIR_RAW, "prec.csv"),
  stringsAsFactors = FALSE
)

press <- read.csv2(
  file.path(DIR_RAW, "pres_atm.csv"),
  stringsAsFactors = FALSE
)

wind <- read.csv2(
  file.path(DIR_RAW, "vel_vien.csv"),
  stringsAsFactors = FALSE
)

# ==============================================================================
# GUARDAR DATOS CRUDOS
# ==============================================================================

save(
  rm,
  vina,
  fire,
  pm,
  temp,
  hum,
  rain,
  press,
  wind,
  file = file.path(
    DIR_PROCESSED,
    "raw_data.RData"
  )
)