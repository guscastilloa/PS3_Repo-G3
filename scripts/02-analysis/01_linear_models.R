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
                   bedrooms + property_type + n_pisos_num + piso_numerico + 
                   distancia_parque + distancia_gimnasio + distancia_transmi +
                   distancia_cai + distancia_bar + distancia_SM + 
                   distancia_colegio + distancia_hospitales + 
                   distancia_avenida_principal + distancia_comercial + 
                   distancia_universidad)
set.seed(1985)  
tc_10cv <- trainControl(method = "cv", number = 10)
en1 <- train(
  m1, data = db_train,
  method = "glmnet",
  trControl = tc_10cv,
  tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.01), 
                         lambda = 10^seq(-3, 3, length = 100)),
  metric = 'MAE')

yhat_en_caret<-predict(en_caret, newdata = db_test) # Esto quiere decir que escoja sobre grilla de 5x5 para lambda y alpha
                         )
                         

