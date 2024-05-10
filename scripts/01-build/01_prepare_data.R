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
## Verificar variable property_type con descripción de cada observación
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
                                 false = property_type2),
         property_type_discrepancy=if_else(property_type!=property_type2,T,F))
table(db$property_type, db$property_type2, dnn = c("Orig", "New"), useNA = 'always')
# Hay 536 viviendas que discrepan en el tipo de propiedad entre property_type
# y la variable construida property_type2.

db %>% 
  filter(
    property_type!=property_type2
    ) %>% 
  select(property_id,title,description_c,property_type,property_type2, surface_total, surface_covered, is_train) %>% 
  View()

## Construir variable de número de pisos



db <- db %>%
  mutate(n_pisos= str_extract(description, "(\\w+|\\d+) pisos")) %>%
  mutate(n_pisos= ifelse(property_type_2=="Casa", n_pisos, NA)) 
numeros_escritos <- c( "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
numeros_numericos <- as.character(2:10)


db <- db %>%
  mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))

db <- db %>%
  mutate(n_pisos_numerico = as.integer(str_extract(n_pisos, "\\d+")))  %>%
  mutate(n_pisos_numerico = if_else(is.na(n_pisos_numerico), 1, n_pisos_numerico)) %>%
  mutate(n_pisos_numerico = if_else(n_pisos_numerico>10, 1, n_pisos_numerico)) ### quedarnos casas de hasta 10 pisos.

## número del piso del apartamento

db <- db %>%
  mutate(piso_info= str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))


numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
numeros_numericos <- as.character(1:10)

db <- db %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos,numeros_escritos)))

db <- db %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

db <- db %>%
  mutate(piso_numerico = ifelse(piso_numerico > 20, NA, piso_numerico)) %>%
  mutate(piso_numerico = ifelse(property_type_2=="Casa", 1, piso_numerico))
a##########################################
db <- db %>%
  mutate(piso_numerico = replace_na(piso_numerico, 1))
ggplot(db %>% filter(piso_numerico>1), aes(x = factor(piso_numerico))) +
  geom_bar() +
  labs(title = "", x = "Pisos", y = "Obs")+
  theme_minimal()



# X. Export -----


