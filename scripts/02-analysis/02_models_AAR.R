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

arbol_1<-train(price ~ bedrooms + bathrooms + surface_total+  property_type + distancia_parque+ distancia_comercial+ distancia_avenida_principal +
                 distancia_universidad,
               data=train_train,
               method="rpart",
               trControl = cv10)

pred_train_arbol_muestra<-predict(arbol_1, newdata=train_train)
pred_test_arbol_muestra<-predict(arbol_1, newdata=test_train)

pred_test_forest<-predict(arbol_1, newdata=test)
pred_test_forest_round<- round(pred_test_forest, digits = -3)

test_k<-test%>%
  select(property_id)%>%
  mutate(price=pred_test_forest_round)
test_k
write_csv(test_k, file="stores/arboles_prediction.csv")


##RANDOM FOREST------------------------------------------------------------

set.seed(1453)

summary(train_muestra)

#Cross validation V=10

cv10<- trainControl(number = 10, method ="cv")

tunegrid_rf<-expand.grid(mtry=c(2,3,4,5, 8), #Predictores aleatorios
                         splitrule= "variance", ##Cambiar por gini y revisar
                         min.node.size=c(1,2,3,6))


rforest<-train(price ~ bedrooms + bathrooms + surface_total+  property_type + distancia_parque+ distancia_comercial+ distancia_avenida_principal +
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
                  data=train_muestra,
                  method = 'lm', 
                  trControl = fitControl,
                  preProcess = c("center", "scale")
) 


linear_reg
summary(linear_reg)



##ELASTIC NET---------------------------------------------------


EN<-train(fmla,
          data=train_muestra,
          method = 'glmnet', 
          trControl = fitControl,
          tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #grilla de alpha
                                 lambda = seq(0.001,0.02,by = 0.001)),
          preProcess = c("center", "scale")
) 



