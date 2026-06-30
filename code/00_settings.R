# ==============================================================================
# 00_settings.R
# ==============================================================================

# ==============================================================================
# LIBRERÍAS
# ==============================================================================

library(tidyverse)
library(rio)
library(stringr)
library(lubridate)
library(forecast)
library(LSTS)
library(TSA)
library(imputeTS)
library(janitor)
library(psych)
library(knitr)
library(glue)
library(tsibble)
library(tibble)
library(fable)
library(fabletools)
library(feasts)

try(
  Sys.setlocale("LC_TIME", "es_ES.UTF-8"),
  silent = TRUE
)

set.seed(2021)

# ==============================================================================
# PATHS
# ==============================================================================

DIR_RAW       <- "data/raw"
DIR_PROCESSED <- "data/processed"
DIR_OUTPUT    <- "data/output"
DIR_FIGURES   <- "results/figures"
DIR_TABLES    <- "results/tables"

# ==============================================================================
# CARPETAS
# ==============================================================================

dirs <- c(DIR_RAW, DIR_PROCESSED, DIR_OUTPUT, DIR_FIGURES, DIR_TABLES)

walk(dirs, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

gitkeep_paths <- file.path(dirs, ".gitkeep")

walk(gitkeep_paths, ~ {
  if (!file.exists(.x)) file.create(.x)
})

# ==============================================================================
# FUNCIONES AUXILIARES
# ==============================================================================

source("functions/TS.summary.R")
source("functions/summary.arima.R")
source("functions/TS.diag.R")
