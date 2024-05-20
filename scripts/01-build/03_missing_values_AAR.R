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
datos<-read_parquet("stores/db2.parquet")


#Revisar los valores faltantes. 
db_miss <- skim(datos) %>% select( skim_variable, n_missing)
library(skimr)
Nobs= nrow(datos) 
Nobs
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
head(db_miss)

#Imputación manual. Lo voy a hacer con el promedio. 
datos$rooms[is.na(datos$rooms)] <- mean(datos$rooms, na.rm = T)
datos$surface_covered[is.na(datos$surface_covered)] <- mean(datos$surface_covered, na.rm = T)
datos$ bathrooms[is.na(datos$bathrooms)] <- mean(datos$bathrooms, na.rm = T)
datos$surface_total[is.na(datos$surface_total)] <- mean(datos$surface_total, na.rm = T)

arrow::write_parquet(datos, sink = "stores/db3.parquet")

#Imputar datos: KNN (esto se me demora mucho. Lo descarto por el momento para empezar ya 
#a correr modelos.)
#datos <-  kNN(datos, variable = c("bathrooms"), k = 6)
#datos$bathrooms <- round(datos$bathrooms,0)
#summary(datos$bathrooms)

#datos <-  kNN(datos, variable = c("surface_covered"), k = 6)
#datos <-  kNN(datos, variable = c("surface_total"), k = 6)

#datos <-  kNN(datos, variable = c("rooms"), k = 6)
#datos$bathrooms <- round(datos$rooms,0)
#summary(datos$bathrooms)

