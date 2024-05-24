#=============================================================================#
# FECHA:
#   2024-05-15 (ISO 8601)
# AUTOR:
#   Gustavo Castillo | ga.castillo@uniandes.edu.co
#   Alexander Almeida
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
db_miss <- skimr::skim(datos) %>% select( skim_variable, n_missing)
Nobs= nrow(datos) 
Nobs
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
head(db_miss)

#Imputación manual. Lo voy a hacer con el promedio. 
datos <- datos %>% 
  mutate(rooms_mean = 
           if_else(is.na(rooms), mean(rooms, na.rm = T), rooms),
         surface_covered_mean = 
           if_else(is.na(surface_covered), mean(surface_covered, na.rm =T),
                   surface_covered),
         bathrooms_mean = 
           if_else(is.na(bathrooms), mean(bathrooms, na.rm = T),
                   bathrooms),
         surface_total_mean = 
           if_else(is.na(surface_total), mean(surface_total, na.rm = T),
                   surface_total))

# Imputar con la mediana
datos <- datos %>% 
  mutate(rooms_median = if_else(is.na(rooms), median(rooms, na.rm = T), rooms),
         surface_covered_median = 
           if_else(is.na(surface_covered), median(surface_covered, na.rm =T),
                   surface_covered),
         bathrooms_median = 
           if_else(is.na(bathrooms), median(bathrooms, na.rm = T),
                   bathrooms),
         surface_total_median = 
           if_else(is.na(surface_total), median(surface_total, na.rm = T),
                   surface_total))

#Imputar datos: KNN (esto se me demora mucho. Lo descarto por el momento para empezar ya 
#a correr modelos.)
datos <-  kNN(datos, variable = c("bathrooms"), k = 6)
# datos$bathrooms <- round(datos$bathrooms,0)
# summary(datos$bathrooms)

datos <-  kNN(datos, variable = c("surface_covered"), k = 6)
datos <-  kNN(datos, variable = c("surface_total"), k = 6)
datos <-  kNN(datos, variable = c("rooms"), k = 6)
#datos$bathrooms <- round(datos$rooms,0)
#summary(datos$bathrooms)

# Export -------
train <- import("stores/raw/train.csv")
validation <- import("stores/raw/test.csv") # doesn't have price tag
train$is_train <- TRUE
validation$is_train <- FALSE
temp <- rbind(train, validation) %>% select(property_id, lon, lat)
datos <- left_join(x = datos, y = temp, by = 'property_id')
arrow::write_parquet(datos, sink = "stores/db3.parquet")
