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
rm(list=ls())
gc()
cat('\f')
source("scripts/00_packages.R")
here()

db<-read_parquet("stores/db3.parquet")

#Generar las variables de interacciones. 
db<- db%>% 
  mutate(property_type3 =ifelse(property_type=="Casa",1,0))


db<- db%>% 
  mutate(int1=property_type3*distancia_parque,
         int2= distancia_comercial*property_type3,
         int3=distancia_avenida_principal*property_type3,
         int4=distancia_universidad*property_type3,
         int5=distancia_cai*property_type3,
         int64=distancia_bar*property_type3,
         int7=distancia_gimnasio*property_type3, 
         int8=distancia_transmi*property_type3,
         int9=distancia_bar*property_type3,
         int10=distancia_SM*property_type3,
         int11=distancia_colegio*property_type3,
         int12=distancia_hospitales*property_type3)

####DIVIDIMOS LA MUESTRA EN TRAIN Y TEST
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

arbol_1<-train(price ~ rooms_mean + bathrooms_mean + surface_total_mean+ 
                 property_type + distancia_parque+ distancia_comercial+ 
                 distancia_avenida_principal +
                 distancia_universidad,
               data=train_train,
               method="rpart",
               trControl = cv10)

pred_train_arbol_muestra<-predict(arbol_1, newdata=train_train)
pred_test_arbol_muestra<-predict(arbol_1, newdata=test_train)

pred_test_arbol<-predict(arbol_1, newdata=test)
pred_test_arbol_round<- round(pred_test_arbol, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_arbol_round)
test_k
write_csv(test_k, file="stores/arboles_prediction.csv")


##RANDOM FOREST------------------------------------------------------------
set.seed(1453)
#Cross validation V=10
cv10<- trainControl(number = 10, method ="cv")
tunegrid_rf<-expand.grid(mtry=c(2,3,4,5, 8), #Predictores aleatorios
                         splitrule= "variance", ##Cambiar por gini y revisar
                         min.node.size=c(1,2,3,6))


rforest<-train(price ~ rooms_mean + bathrooms_mean + surface_total_mean+  property_type +
                 distancia_parque+ distancia_comercial+ distancia_avenida_principal +
                 distancia_universidad,
               data=train_train,
               trControl = cv10,
               metric = "RMSE",
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

write_csv(test_k, file="stores\random_forest.csv")

######################Random Forest v2##########################################

rforest2<-train(price ~ rooms_mean + bathrooms_mean + surface_total_mean+  property_type+
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
pred_test_forest<-predict(rforest2, newdata=test)
pred_test_forest

pred_test_forest_round<- round(pred_test_forest, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_forest_round)
test_k

write_csv(test_k, file="stores\random_forest_v2.csv")


######################Random Forest v3##########################################
rforest3<-train(price ~ rooms_mean + bathrooms_mean + surface_total_mean+  property_type+
                  distancia_parque+ distancia_comercial+ 
                  distancia_avenida_principal + 
                  distancia_universidad+distancia_cai+distancia_bar+distancia_gimnasio
                +distancia_transmi+distancia_bar+distancia_SM+distancia_colegio+
                  distancia_hospitales+month+year,
                data=train_train,
                trControl = cv10,
                metric = "MAE",
                tuneGrid = tunegrid_rf,
                method ="ranger")

##Subida a Kaggle. 
pred_test_forest<-predict(rforest3, newdata=test)
pred_test_forest

pred_test_forest_round<- round(pred_test_forest, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_forest_round)
test_k

write_csv(test_k, file="stores\random_forest_v3.csv")

######################Random Forest v4##########################################
rforest4<-train(price ~ rooms_median + bathrooms_median + surface_total_median+  property_type+
                  distancia_parque+ distancia_comercial+ 
                  distancia_avenida_principal + 
                  distancia_universidad+distancia_cai+distancia_bar+distancia_gimnasio
                +distancia_transmi+distancia_bar+distancia_SM+distancia_colegio+
                  distancia_hospitales+month+year+lat+lon,
                data=train_train,
                trControl = cv10,
                metric = "MAE",
                tuneGrid = tunegrid_rf,
                method ="ranger")

##Subida a Kaggle. 
pred_test_forest<-predict(rforest4, newdata=test)
pred_test_forest

pred_test_forest_round<- round(pred_test_forest, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_forest_round)
test_k

write_csv(test_k, file="stores\random_forest_v4.csv")


######################Random Forest v5##########################################
rforest5<-train(price ~ rooms_median + bathrooms_median + surface_total_median+  property_type+
                  distancia_parque+ distancia_comercial+ 
                  distancia_avenida_principal + 
                  distancia_universidad+distancia_cai+distancia_bar+distancia_gimnasio
                +distancia_transmi+distancia_bar+distancia_SM+distancia_colegio+
                  distancia_hospitales+as.factor(month)+as.factor(year)+lat+lon+
                  int1+int2+int3+int4+int5+int6+int7+int8+int9+int10+int11+int12,
                data=train_train,
                trControl = cv10,
                metric = "MAE",
                tuneGrid = tunegrid_rf,
                method ="ranger")

##Subida a Kaggle. 
pred_test_forest<-predict(rforest5, newdata=test)
pred_test_forest

pred_test_forest_round<- round(pred_test_forest, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_forest_round)
test_k

write_csv(test_k, file="stores\random_forest_v5.csv")


#### OLS - ELASTICNET------------------------------------------------------------
##ALPHA Y LAMBDA
##OLS---------------------------------------
set.seed(1453)
fitControl <- trainControl(## 8-fold CV
  method = "cv",
  number = 10)


fmla<-formula(price ~ bedrooms + bathrooms + surface_total+  property_type + 
                distancia_parque+ distancia_comercial+ distancia_avenida_principal +
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


#Elastic - net - v2. 
EN2<-train(price ~ rooms_mean + bathrooms_mean + surface_total_mean+  property_type+
             distancia_parque+ distancia_comercial+ 
             distancia_avenida_principal + 
             distancia_universidad+distancia_cai+distancia_bar+distancia_gimnasio
           +distancia_transmi+distancia_bar+distancia_SM+distancia_colegio+
             distancia_hospitales,
           data=train_train,
           method = 'glmnet', 
           trControl = fitControl,
           tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #grilla de alpha
                                  lambda = seq(0.001,0.02,by = 0.001)),
           preProcess = c("center", "scale")
) 


##Subida a Kaggle. 
pred_test_elastic<-predict(EN2, newdata=test)
pred_test_elastic_round<- round(pred_test_elastic, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_elastic_round)
test_k

write_csv(test_k, file="ELASTICNET_v2.csv")


#v3
EN3<-train(rooms_mean + bathrooms_mean + surface_total_mean+  property_type+
             distancia_parque+ distancia_comercial+ 
             distancia_avenida_principal + 
             distancia_universidad+distancia_cai+distancia_bar+distancia_gimnasio
           +distancia_transmi+distancia_bar+distancia_SM+distancia_colegio+
             distancia_hospitales+month+year,
           data=train_train,
           method = 'glmnet', 
           trControl = fitControl,
           tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #grilla de alpha
                                  lambda = seq(0.001,0.02,by = 0.001)),
           preProcess = c("center", "scale")
) 


##Subida a Kaggle. 
pred_test_elastic<-predict(EN3, newdata=test)
pred_test_elastic_round<- round(pred_test_elastic, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_elastic_round)
test_k

write_csv(test_k, file="ELASTICNET_v3.csv")

#V4
EN4<-train(price ~ rooms_median + bathrooms_median + surface_total_median+  property_type+
             distancia_parque+ distancia_comercial+ 
             distancia_avenida_principal + 
             distancia_universidad+distancia_cai+distancia_bar+distancia_gimnasio
           +distancia_transmi+distancia_bar+distancia_SM+distancia_colegio+
             distancia_hospitales+month+year+lat+lon,
           data=train_train,
           method = 'glmnet', 
           trControl = fitControl,
           tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #grilla de alpha
                                  lambda = seq(0.001,0.02,by = 0.001)),
           preProcess = c("center", "scale")
) 


##Subida a Kaggle. 
pred_test_elastic<-predict(EN4, newdata=test)
pred_test_elastic_round<- round(pred_test_elastic, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_elastic_round)
test_k

write_csv(test_k, file="ELASTICNET_v4.csv")

#Probar el GBM.
p_load(gbm)
grid_gbm<-expand.grid(n.trees=c(300,700,1000),interaction.depth=c(1:4),shrinkage=seq(0.01,0.001),n.minobsinnode
                      =c(10,30, 40))
# Cross-validation
ctrl <- trainControl(
  method = "cv", 
  number = 10) # n?mero de folds


ModeloGBM<- train(price ~ rooms_mean + bathrooms_mean + surface_total_mean+  property_type +
                    distancia_parque+ distancia_comercial+ distancia_avenida_principal +
                    distancia_universidad,
                    data = train_train, 
                    method = "gbm", 
                    trControl = ctrl,
                    tuneGrid=grid_gbm,
                    metric = "MAE"
)     

##Subida a Kaggle. 
pred_test_gbm<-predict(ModeloGBM2, newdata=test)
pred_test_gbm_round<- round(pred_test_gbm, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_gbm_round)
test_k

write_csv(test_k, file="Modelo_GBM.csv")

ModeloGBM2 <- train(price ~ rooms_median + bathrooms_median + surface_total+property_type + 
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
pred_test_gbm<-predict(ModeloGBM, newdata=test)
pred_test_gbm_round<- round(pred_test_gbm, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_gbm_round)
test_k

write_csv(test_k, file="Modelo_GBM_v2.csv")

#Modelo GBM v3.
ModeloGBM3 <- train(price ~ rooms_median + bathrooms_median + surface_total+property_type + 
                      distancia_parque+ distancia_comercial+ 
                      distancia_avenida_principal + 
                      distancia_universidad+distancia_cai+distancia_bar+distancia_gimnasio
                    +distancia_transmi+distancia_bar+distancia_SM+distancia_colegio+
                      distancia_hospitales+month+year+lat+lon,
                    data = train_train, 
                    method = "gbm", 
                    trControl = ctrl,
                    tuneGrid=grid_gbm,
                    metric = "MAE"
)     

##Subida a Kaggle. 
pred_test_gbm<-predict(ModeloGBM3, newdata=test)
pred_test_gbm_round<- round(pred_test_gbm, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_gbm_round)
test_k

write_csv(test_k, file="Modelo_GBM_v3.csv")






### Super Learner 
ySL <- train$price # definir variable interes, precios
XSL <- train  %>% select(rooms_mean,bathrooms_mean,surface_total_mean,property_type,
                         distancia_parque,distancia_comercial,distancia_avenida_principal,
                         distancia_universidad) # definir predictoras

# Definir libreria 
listWrappers()
sl.lib <- c("SL.lm", "SL.ridge", "SL.glm", "SL.ranger") 

# Fit using the SuperLearner package,
fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinaci?n convexa
                     SL.library = sl.lib)

fitY

test_m <- test %>% select(bedrooms,bathrooms,surface_total,property_type,
                          distancia_parque,distancia_comercial,
                          distancia_avenida_principal,
                          distancia_universidad) # definir predictoras


price_SL <- predict(fitY, newdata = test_m, onlySL =TRUE)
price_SL <- data.frame(price_SL) 
price_SL<- price_SL[,-c(2:5)]
price_SL <- data.frame(price_SL) 

test_id<-test %>%select(property_id)
resultados <- data.frame("property_id" = test_id, "price" = price_SL)
colnames(resultados)[2] <- "price"
write.csv(resultados, 'SuperLearner_price.csv',row.names=FALSE) 




