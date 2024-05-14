#=============================================================================#
# FECHA:
#   2024-05-13 (ISO 8601)
# AUTOR:
#   Gustavo Castillo
#   ga.castillo@uniandes.edu.co
# DESCRIPCION:
#   Construcción de los primeros modelos lineales.
#=============================================================================#


# 1. Preparara entorno -----
rm(list=ls())
gc()
source("scripts/00_packages.R")
here()

# 2. Load data ----
db <- arrow::read_parquet("stores/db.parquet")

