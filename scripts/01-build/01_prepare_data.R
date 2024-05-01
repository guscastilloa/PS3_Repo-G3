#=============================================================================#
# FECHA:
#   2024-05-01 (ISO 8601)
# AUTOR:
#   Gustavo Castillo
#   ga.castillo@uniandes.edu.co
# DESCRIPCION:
#   Exploración de las bases de datos.
#=============================================================================#

# 1. Preparara entorno -----
rm(list=ls())
gc()
source("scripts/00_packages.R")
here()

# 2. Load data ----
train <- import("stores/raw/train.csv")
validation <- import("stores/raw/test.csv") # doesn't have price tag

train$is_train <- TRUE
validation$is_train <- FALSE

db <- rbind(train, validation)

# 3. Feature engineering -----
# 3.1. Construir variables a partir de descripción -----
## número de pisos
clean_txt <- function(str){
 str_squish(str_to_lower(
   stringi::stri_trans_general(
     str_replace_all(str,'[^[:alnum:]]', " "), 'Latin-ASCII'))
 )
  }

db <- db %>% 
  mutate(description_c = clean_txt(description),
         property_type2 = case_when(
           (grepl("\\bcasa\\b", description_c) & 
              grepl("\\bapto|apartamento", description_c))~"Ambos",
           grepl("\\bcasa\\b", description_c)==T~"Casa",
           grepl("\\bapto|apartamento", description_c)==T~"Apartamento"),
         property_type2= if_else(property_type2=="Ambos",
                                 true = property_type,
                                 false = property_type2)
         )
table(db$property_type, db$property_type2, dnn = c("Orig", "New"), useNA = 'always')
# Hay 991 viviendas () que con coincide el tipo de propiedad

db %>% 
  filter(
    # property_type!=property_type2
    property_type2=="Ambos"
    # is.na(property_type2)
    ) %>% 
  select(property_id,title,description_c,property_type,property_type2) %>% 
  View()


# 0923bcd0568d89de2cc71a1f




# X. Export -----


