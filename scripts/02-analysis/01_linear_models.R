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
db <- arrow::read_parquet("stores/db3.parquet")
db_train <- db %>% subset(is_train==T)
db_test <- db %>% filter(is_train==F)



# 3. Elastic Net -------
m1 <- as.formula(log(price) ~ surface_total + surface_covered + rooms +
                   bedrooms + as.factor(property_type2) + n_pisos_num + piso_numerico + 
                   distancia_parque + distancia_gimnasio + distancia_transmi +
                   distancia_cai + distancia_bar + distancia_SM + 
                   distancia_colegio + distancia_hospitales + 
                   distancia_avenida_principal + distancia_comercial + 
                   distancia_universidad)
m2 <- as.formula(log(price) ~ surface_total + surface_covered + rooms +
                   bedrooms + as.factor(property_type2) + n_pisos_num + piso_numerico + 
                   distancia_parque + distancia_gimnasio + distancia_transmi +
                   distancia_cai + distancia_bar + distancia_SM + 
                   distancia_colegio + distancia_hospitales + 
                   distancia_avenida_principal + distancia_comercial + 
                   distancia_universidad)

## M1 ----
set.seed(1985)  
tc_10cv <- trainControl(method = "cv", number = 10)


en1 <- train(
  m1, data = db_train,
  method = "glmnet",
  trControl = tc_10cv,
  tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.1), 
                         lambda = 10^seq(-3, 3, length = 100)),
  metric = 'MAE')

m1_out <- data.frame(property_id = db_test$property_id,
                     price = exp(predict(en1, newdata = db_test)))

## M2: Spatial cross validation ----

# model tuning with spatial cross validation #

localidades_folds <- spatial_leave_location_out_cv(
  st_as_sf(db_train, coords = c('lon', 'lat'), crs = 4686),
  group = LocCodigo)
autoplot(localidades_folds)
block_folds <- spatial_block_cv(
  st_as_sf(db_train,
           coords = c('lon', 'lat'), 
           crs = 4686),
  v = 5)

# create indexes for localidades folds #
folds_train<-list()
for(i in 1:length(localidades_folds$splits)){
  folds_train[[i]]<- localidades_folds$splits[[i]]$in_id
}
ctrl_spa <- trainControl(method = 'cv',
                        index = folds_train)
en_spa <- train(m1, data = db_train, method = 'glmnet', 
                trControl = ctrl_spa, metric = 'MAE',
                tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.1), 
                                       lambda = 10^seq(-3, 3, length = 60)))

m2_out <- data.frame(property_id = db_test$property_id,
                     price = exp(predict(en_spa, newdata = db_test)))
 




## Export ---- 
export(m1_out, "stores/submissions/enet_g1.csv", sep = ',')
export(m2_out, "stores/submissions/enet_spatialcv_g1.csv", sep = ',')

# 4. Boosting -----
