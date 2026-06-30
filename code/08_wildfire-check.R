# ==============================================================================
# 08_wildfire-check.R
# ==============================================================================

source("code/00_settings.R")

# ==============================================================================
# ARIMAX SIN REZAGOS
# ==============================================================================

# Comparación incendios:
# 2019 - Abril - 15 y 16 --> incendio de Paredones
# 2019 - Mayo - 14 y 25 (muy menor puede ser por calefacción) --> falla
# 2019 - Junio y Julio - 7 fechas (muy menor puede ser por calefacción) --> falla
# 2019 - Noviembre - 15, 16, 17, 18 --> incendio de Cabasablanca
# 2020 - Abril - 15 y 16 (menor pero no invierno, puede ser incendio pequeño) --> falla
# 2020 - Mayo - 30 (menor) --> incendio en Quilpué
# 2020 - Junio - 6 (menor por calefacción) --> falla
# 2021 - Enero - 15, 16, 17 --> incendio Valparaíso-Quilpué
# 2021 - Marzo - 23 --> nube incendio de Araucanía
# 2021 - Abril - 27 y 29 (menor por calefacción) --> falla
# 2021 - Mayo - 15, 16 y 30 (mayores las dos primeras, puede ser una nube lejana y la última por contaminación) --> falla
# 2021 - Julio y Agosto - 31, 1, 13, 14, 15 --> incendio de La Ligua
# 2022 - Marzo - 3 --> incendio Valparaíso
# 2022 - Mayo - 6 y 18 (leve puede ser contaminación) --> falla
# 2022 - Junio - 25 (leve puede ser contaminación) ---> falla
# 2022 - Diciembre - 14 --> incendio Melipilla
# 2023 - Febrero - 6, 7, 8, 9, 10 --> mega incendio nacional
# 2023 - Junio - 14 y 15 (menor puede ser contaminación) --> falla
# 2024 - Febrero - 2, 3 y 5 --> mega incendio de Valparaíso
# 4 fechas fuera de rango de datos

# ==============================================================================
# ARIMAX CON REZAGOS
# ==============================================================================
