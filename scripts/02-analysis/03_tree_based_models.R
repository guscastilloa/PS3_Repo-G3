#=============================================================================#
# FECHA:
#   2024-05-26 (ISO 8601)
# AUTOR:
#   Gustavo Castillo
#   ga.castillo@uniandes.edu.co
# DESCRIPCION:
#   Construcción de modelos basados en árboles.
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


# 3. Model specification -----
m1 <- as.formula(log(price) ~ surface_total + surface_covered + rooms +
                   bedrooms + as.factor(property_type2) + n_pisos_num + 
                   piso_numerico + 
                   distancia_parque + distancia_gimnasio + distancia_transmi +
                   distancia_cai + distancia_bar + distancia_SM + 
                   distancia_colegio + distancia_hospitales + 
                   distancia_avenida_principal + distancia_comercial + 
                   distancia_universidad)
m2 <- as.formula(log(price) ~ surface_total + surface_covered + rooms +
                   bedrooms + as.factor(property_type2) + n_pisos_num + 
                   piso_numerico + 
                   distancia_parque + distancia_gimnasio + distancia_transmi +
                   distancia_cai + distancia_bar + distancia_SM + 
                   distancia_colegio + distancia_hospitales + 
                   distancia_avenida_principal + distancia_comercial + 
                   distancia_universidad)
m3 <- as.formula(log(price) ~ surface_total_median + surface_covered_median +
                   rooms_median + bedrooms + as.factor(property_type2) + 
                   n_pisos_num + bathrooms_median + piso_numerico + 
                   distancia_parque + distancia_gimnasio + distancia_transmi +
                   distancia_cai + distancia_bar + distancia_SM + 
                   distancia_colegio + distancia_hospitales + 
                   distancia_avenida_principal + distancia_comercial + 
                   distancia_universidad)



# 4. Stochastic Gradient Boosting -----
grid_gbm<-expand.grid(n.trees= c( 50, 100,150),
                      interaction.depth=c(1,2),
                      shrinkage=c(0.01),
                      n.minobsinnode=c(5, 10))
set.seed(1985) 
gbm_tree <- train(m1,
                  data = db_train, 
                  method = "gbm", 
                  trControl = ctrl_spa,
                  tuneGrid=grid_gbm,
                  metric = "MAE",
                  verbose = T)            
m3_out <- data.frame(property_id = db_test$property_id,
                     price = exp(predict(gbm_tree, newdata = db_test)))

## Export ----
export(m3_out, "stores/submissions/gbm_spatialcv_g1.csv", sep = ',')

# 5. Exreme Gradient Boosting -------
grid_xbgoost <- expand.grid(nrounds = c(250,500),
                            max_depth = c(1, 2),
                            eta = c(0.1,  0.01), 
                            gamma = c(0, 1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.4, 0.7), 
                            subsample = c(0.7))

grid_rforest <- expand.grid(
  mtry=c(2,3,4,5,8))

## Export ----
exec_mod <- function(sel_model){
  set.seed(1985)
  if (sel_model == "xgboost") {
    Xgboost_tree <- train(m1,
                          data = db_train, 
                          method = "xgbTree", 
                          # trControl = ctrl_spa,
                          trControl = trainControl(search = "random", verboseIter = T), 
                          tuneLength = 50,
                          tuneGrid=grid_xbgoost,
                          metric = "MAE",
                          verbosity = 0,
                          verbose = 1)         
    Xgboost_tree
  } else if (sel_model == "rf") {
    print("Training random forest.")
    randomforest <- train(m1,
                          data = db_train, 
                          method = "parRF", 
                          trControl = trainControl(number = 10, method = 'cv', verboseIter = T), 
                          tuneLength = 50,
                          tuneGrid=grid_rforest,
                          metric = "MAE",
                          verbose = 1)         
    randomforest
  }
}



# CUIDADO CON ESTO! Revisar que tengan por lo menos 5 núcleos
#stopifnot(1==0)

# Set the number of cores for parallelization
num_cores <- 10

# Initialize a parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

#registerDoMC(cores=10)
r <- exec_mod(sel_model = 'rf')

stopCluster(cl)

#m4_out <- data.frame(property_id = db_test$property_id,
#                     price = exp(predict(r, newdata = db_test)   ))
m5_out <- data.frame(property_id = db_test$property_id,
                     price = exp(predict(r, newdata = db_test)   ))

## Export ----
#export(m4_out, "stores/submissions/xgbTree_randomsearch_g1.csv", sep = ',')
export(m5_out, "stores/submissions/randomforest_randomsearch_g1.csv", sep = ',')