#=============================================================================#
# FECHA:
#   2024-05-15 (ISO 8601)
# AUTOR:
#   Alexander Almeida-Ramírez 
#   a.almeidar@uniandes.edu.co
# DESCRIPCION:
#   Estimación de modelos.
#=============================================================================#
# 1. Preparara entorno -----
setwd("C:/Users/a.almeidar/OneDrive - Universidad de los andes/PS3_Repo-G3/")
rm(list=ls())
gc()
cat('\f')
source("scripts/00_packages.R")
here()

####DIVIDIMOS LA MUESTRA EN TRAIN Y TEST
db<-read_parquet("stores/db3.parquet")
test <- db %>% filter(is_train==F)
train <- db %>% subset(is_train==T) #subset(LocCodigo!="02")

set.seed(1453)
intrain <- createDataPartition(
  y = train$price,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

train_train <- train[ intrain,]
test_train  <- train[-intrain,]


##ARBOLES---------------------------------------------------------------------
set.seed(1453)

#Cross validation V=5

cv10<- trainControl(number = 10, method ="cv")

##RANDOM FOREST------------------------------------------------------------

set.seed(1453)

#Cross validation V=10

cv10<- trainControl(number = 10, method ="cv")

tunegrid_rf<-expand.grid(mtry=c(2,3,4,5, 8), #Predictores aleatorios
                         splitrule= "variance", ##Cambiar por gini y revisar
                         min.node.size=c(1,2,3,6))


rforest<-train(price ~ bedrooms + bathrooms + surface_total+  property_type + 
                 distancia_parque+ distancia_comercial+ 
                 distancia_avenida_principal + 
                 distancia_universidad+distancia_cai+distancia_bar+distancia_gimnasio
               +distancia_transmi+distancia_bar+distancia_SM+distancia_colegio+
                 distancia_hospitales,
               data=train_train,
               trControl = cv10,
               metric = "MAE",
               tuneGrid = tunegrid_rf,
               method ="ranger")

##Subida a Kaggle. 
pred_test_forest<-predict(rforest, newdata=test)
pred_test_forest

pred_test_forest_round<- round(pred_test_forest, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_forest_round)
test_k

write_csv(test_k, file="random_forest_v2.csv")



#### OLS - ELASTICNET------------------------------------------------------------
##ALPHA Y LAMBDA
##OLS---------------------------------------
set.seed(1453)
fitControl <- trainControl(## 8-fold CV
  method = "cv",
  number = 10)


fmla<-formula(price ~ bedrooms + bathrooms + surface_total+  property_type + distancia_parque+ distancia_comercial+ distancia_avenida_principal +
                distancia_universidad)

linear_reg<-train(fmla,
                  data=train_train,
                  method = 'lm', 
                  trControl = fitControl,
                  preProcess = c("center", "scale")
) 


##Subida a Kaggle. 
pred_test_reg<-predict(linear_reg, newdata=test)
pred_test_reg_round<- round(pred_test_reg, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_reg_round)
test_k

write_csv(test_k, file="OLS_ELASTICNET.csv")

#Probar el GBM.
p_load(gbm)
grid_gbm<-expand.grid(n.trees=c(300,700,1000),interaction.depth=c(1:4),shrinkage=seq(0.01,0.001),n.minobsinnode
                      =c(10,30, 40))
# Cross-validation
ctrl <- trainControl(
  method = "cv", 
  number = 10) # n?mero de folds

ModeloGBM <- train(price ~ bedrooms + bathrooms + surface_total+  property_type + 
                     distancia_parque+ distancia_comercial+ 
                     distancia_avenida_principal + 
                     distancia_universidad+distancia_cai+distancia_bar+distancia_gimnasio
                   +distancia_transmi+distancia_bar+distancia_SM+distancia_colegio+
                     distancia_hospitales,
                   data = train_train, 
                   method = "gbm", 
                   trControl = ctrl,
                   tuneGrid=grid_gbm,
                   metric = "MAE"
)     

##Subida a Kaggle. 
pred_test_reg<-predict(ModeloGBM, newdata=test)
pred_test_reg_round<- round(pred_test_reg, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_reg_round)
test_k

write_csv(test_k, file="Modelo_GBM.csv")

##ELASTIC NET---------------------------------------------------

EN<-train(fmla,
          data=train_train,
          method = 'glmnet', 
          trControl = fitControl,
          tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #grilla de alpha
                                 lambda = seq(0.001,0.02,by = 0.001)),
          preProcess = c("center", "scale")
) 


##Subida a Kaggle. 
pred_test_elastic<-predict(EN, newdata=test)
pred_test_elastic_round<- round(pred_test_elastic, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_elastic_round)
test_k

write_csv(test_k, file="ELASTICNET.csv")



### Super Learner 
p_load("SuperLearner")

ySL <- train$price # definir variable interes, precios
XSL <- train  %>% select(bedrooms,bathrooms,surface_total,property_type,distancia_parque,distancia_comercial,distancia_avenida_principal,
                         distancia_universidad) # definir predictoras

# Definir libreria 
listWrappers()
sl.lib <- c("SL.lm", "SL.ridge", "SL.glm", "SL.ranger") 

# Fit using the SuperLearner package,
fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinaci?n convexa
                     SL.library = sl.lib)

fitY

test_m <- test %>% select(bedrooms,bathrooms,surface_total,property_type,distancia_parque,distancia_comercial,distancia_avenida_principal,
                          distancia_universidad) # definir predictoras


price_SL <- predict(fitY, newdata = test_m, onlySL =TRUE)
price_SL <- data.frame(price_SL) 
price_SL<- price_SL[,-c(2:5)]
price_SL <- data.frame(price_SL) 

test_id<-test %>%select(property_id)
resultados <- data.frame("property_id" = test_id, "price" = price_SL)
colnames(resultados)[2] <- "price"
write.csv(resultados, 'SuperLearner_price.csv',row.names=FALSE) 
