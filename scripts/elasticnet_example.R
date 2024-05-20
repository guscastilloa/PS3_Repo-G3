sf_db <- st_read("stores/raw/spatial_data/sf_db.gpkg")
db <- st_drop_geometry(sf_db)
test <- db %>% filter(is_train==F)
train <- db %>% subset(is_train==T) #subset(LocCodigo!="02")

# 1. Especificar modelo -----------
elastic_net_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# 2. Fijar grilla de búsqueda para hiperparámetros ---------
grid_values <- grid_regular(penalty(range = c(-2,1)), levels = 50) %>%
               expand_grid(mixture = c(0, 0.25,  0.5, 0.75,  1))

# 3. Crear "recetas" ---------------
# Primera receta
rec_1 <- recipe(price ~ distancia_parque +  rooms + bathrooms + property_type2+ piso_numerico+ n_pisos_num , data = train) %>%
  step_interact(terms = ~ distancia_parque:property_type2) %>% # creamos interacciones con el tipo de propiedad
  step_interact(terms = ~ distancia_parque:piso_numerico) %>% # Crea interacción de  la distancia al parque con el piso donde se encuentra el apto. 
  step_novel(all_nominal_predictors()) %>%   # para las clases no antes vistas en el train. 
  step_dummy(all_nominal_predictors()) %>%  # crea dummies para las variables categóricas
  step_zv(all_predictors()) %>%   #  elimina predictores con varianza cero (constantes)
  step_normalize(all_predictors())  # normaliza los predictores. 

# Segunda receta 
rec_2 <- recipe(price ~  distancia_parque + rooms + bathrooms + property_type2+ piso_numerico+ n_pisos_num, data = train) %>%
  step_interact(terms = ~ distancia_parque:property_type2) %>% 
  step_interact(terms = ~ distancia_parque:piso_numerico) %>% 
  step_interact(terms = ~ distancia_parque:n_pisos_num) %>%   # añadimos una interacción de distancia al parque con el número de pisos de la casa. 
  step_poly(distancia_parque, degree = 2) %>%
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>%   
  step_normalize(all_predictors())

# 4. Crear workflow() --------
workflow_1 <- workflow() %>%
  # Agregar la receta de preprocesamiento de datos
  add_recipe(rec_1) %>%
  # Agregar la especificación del modelo de regresión Elastic Net
  add_model(elastic_net_spec)

## Lo mismo con la receta rec_2 

workflow_2 <- workflow() %>%
  add_recipe(rec_2) %>%
  add_model(elastic_net_spec)

# definimos nuestra variable como sf
# test <- sf_db %>% subset(LocCodigo=="02")
train_sf <- sf_db %>% subset(LocCodigo!="02")

set.seed(86936)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)


p_load("purrr")

walk(block_folds$splits, function(x) print(autoplot(x)))

# 5. Cross validation para buscar hiperparámetros tune_grid() ----------
set.seed(86936)
tune_res1 <- tune_grid(
  workflow_1,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = block_folds,  # Folds de validación cruzada espacial
  grid = grid_values,        # Grilla de valores de penalización
  metrics = metric_set(mae)  # metrica
)

collect_metrics(tune_res1)

# 6. Escoger mejores tuning parameters ----------
# Utilizar 'select_best' para seleccionar el mejor valor.
best_tune_res1 <- select_best(tune_res1, metric = "mae")
best_tune_res1

# 7. Estimar modelo con mejores hiperpatámetros ---------
# Finalizar el flujo de trabajo 'workflow' con el mejor valor de parametros
res1_final <- finalize_workflow(workflow_1, best_tune_res1)
EN_final1_fit <- fit(res1_final, data = train)


# End