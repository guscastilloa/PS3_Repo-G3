#=============================================================================#
# FECHA:
#   2024-05-15 (ISO 8601)
# AUTOR:
#   Gustavo Castillo
#   ga.castillo@uniandes.edu.co
# DESCRIPCION:
#   Diferentes estrategias de lidiar con valores faltantes.
#=============================================================================#

# 1. Preparara entorno -----
rm(list=ls())
gc()
cat('\f')
source("scripts/00_packages.R")
here()

# 2. Load data ----
sf_db <- st_read("stores/raw/spatial_data/sf_db.gpkg")
db <- st_drop_geometry(sf_db)

# 3. Missing values de "bathrooms" --------
## 3.1. Kmeans ------
