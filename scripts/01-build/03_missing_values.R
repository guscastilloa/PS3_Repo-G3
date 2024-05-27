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
datos_train <- datos %>% filter(is_train==T)
datos_test <- datos %>% filter(is_train==F)

#Revisar los valores faltantes. 
db_miss <- skimr::skim(datos) %>% select( skim_variable, n_missing)
Nobs= nrow(datos) 
Nobs
db_miss<- db_miss %>% mutate(p_missing= n_missing/Nobs)
head(db_miss)

# Imputar datos con el promedio
datos <- datos %>% 
  mutate(rooms_mean = 
           if_else(is.na(rooms), 
                   mean(datos$rooms[datos$is_train == 1], na.rm = TRUE), rooms),
         surface_covered_mean = 
           if_else(is.na(surface_covered), 
                   mean(datos$surface_covered[datos$is_train == 1], na.rm = TRUE),
                   surface_covered),
         bathrooms_mean = 
           if_else(is.na(bathrooms), 
                   mean(datos$bathrooms[datos$is_train == 1], na.rm = TRUE),
                   bathrooms),
         surface_total_mean = 
           if_else(is.na(surface_total), 
                   mean(datos$surface_total[datos$is_train == 1], na.rm = TRUE),
                   surface_total))

# Imputar con la mediana
datos <- datos %>% 
  mutate(rooms_median = 
           if_else(is.na(rooms), 
                   median(datos$rooms[datos$is_train == 1], na.rm = TRUE),
                   rooms),
         surface_covered_median = 
           if_else(is.na(surface_covered), 
                   median(datos$surface_covered[datos$is_train == 1], na.rm = TRUE),
                   surface_covered),
         bathrooms_median = 
           if_else(is.na(bathrooms), 
                   median(datos$bathrooms[datos$is_train == 1], na.rm = TRUE),
                   bathrooms),
         surface_total_median = 
           if_else(is.na(surface_total), 
                   mean(datos$surface_total[datos$is_train == 1], na.rm = TRUE),
                   surface_total))

#Imputar datos: KNN (esto se me demora mucho. Lo descarto por el momento para empezar ya 
#a correr modelos.)
datos <-  kNN(datos, variable = c("bathrooms"), k = 6)
datos <-  kNN(datos, variable = c("surface_covered"), k = 6)
datos <-  kNN(datos, variable = c("surface_total"), k = 6)
datos <-  kNN(datos, variable = c("rooms"), k = 6)
#datos$bathrooms <- round(datos$rooms,0)
#summary(datos$bathrooms)

# Factor variables ----------
datos$LocCodigo <- as.factor(datos$LocCodigo)

# Export -------
train <- import("stores/raw/train.csv")
validation <- import("stores/raw/test.csv") # doesn't have price tag
train$is_train <- TRUE
validation$is_train <- FALSE
temp <- rbind(train, validation) %>% select(property_id, lon, lat)
datos <- left_join(x = datos, y = temp, by = 'property_id')
arrow::write_parquet(datos, sink = "stores/db3.parquet")
