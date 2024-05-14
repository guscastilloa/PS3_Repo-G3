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
         property_type_discrepancy=if_else(property_type!=property_type2,T,F),
         property_type2=if_else(is.na(property_type2),
                                property_type,
                                property_type2))
table(db$property_type, db$property_type2, dnn = c("Orig", "New"), useNA = 'always')

# # NOTA :
# Hay 536 viviendas que discrepan en el tipo de propiedad entre property_type
# y la variable construida property_type2. Representa el 1% de la muestra total:
#   - 1.2% del train
#   - 0.6% del validation

db %>% 
  filter(
    property_type!=property_type2
    ) %>% 
  select(property_id,title,description_c,property_type,property_type2, surface_total, surface_covered, is_train) %>% 
  View()

## Construir variable de número de pisos ##
numeros_escritos <- c( "dos", "tres", "cuatro", "cinco", "seis", "siete", 
                       "ocho", "nueve", "diez")
numeros_numericos <- as.character(2:10)
db <- db %>%
  mutate(n_pisos= str_extract(description, "(\\b\\w{3,6}|\\d+) pisos")) %>%
  mutate(n_pisos= ifelse(property_type2=="Casa", n_pisos, NA)) %>% 
  mutate(n_pisos = str_replace_all(n_pisos, setNames(numeros_numericos,numeros_escritos)))
db <- db %>%
  mutate(n_pisos_num = as.integer(stri_extract_first(n_pisos, regex="\\d+"))) %>% 
  mutate(n_pisos_num = case_when(
    is.na(n_pisos_num) & property_type2=="Casa"~1,
    .default = n_pisos_num)) %>%
  mutate(n_pisos_num = if_else(n_pisos_num>10, 1, n_pisos_num))
  

## número del piso del apartamento ##
numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", 
                      "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", 
                      "seis|sexto", "siete|septimo", "ocho|octavo", 
                      "nueve|noveno", "diez|decimo|dei")
numeros_num <- as.character(1:10)
db <- db %>%
  mutate(piso_info= str_extract(description, "(\\b\\w+|\\d+) piso (\\w+|\\d+)")) %>% 
  mutate(mts_info_bool = grepl(pattern="\\d+(?=m[ts2]+)",piso_info, perl = T)) %>% 
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_num,numeros_escritos))) %>% 
  mutate(piso_numerico = as.integer(stri_extract_first(piso_info, regex= "\\d+"))) %>% 
  mutate(piso_numerico = ifelse(piso_numerico > 66, NA, piso_numerico)) %>%
  mutate(piso_numerico = ifelse(property_type2=="Casa", 1, piso_numerico))
  

db <- db %>%
  mutate(piso_numerico = replace_na(piso_numerico, 1))
ggplot(db %>% filter(piso_numerico>1), aes(x = factor(piso_numerico))) +
  geom_bar() +
  labs(title = "", x = "Pisos", y = "Obs")+
  theme_minimal()


# 4. Incluir datos espaciales -----
sf_db <- st_as_sf(db, coords = c('lon', 'lat'), crs =  4686)

fname <- system.file("stores/raw/spatial_data/GPKG_MR_V1223.gpkg", package = 'sf')
loca <- st_read("stores/raw/spatial_data/GPKG_MR_V1223.gpkg", layer = 'Loca')

ggplot()+
  geom_sf(data = loca %>% filter(LocCodigo %in% c('01', '02', '03')),
          mapping=aes(fill = LocNombre), show.legend = FALSE)+
  geom_sf(data = sf_db %>% filter(is_train==F), fill = NA, colour = alpha('red', 0.4))+
  coord_sf()

sf_db <-st_join(x = sf_db, # %>% filter(is_train==F), 
                y = loca, # %>% filter(LocCodigo %in% c('01', '02', '03')),
                join = st_intersects)

# Plotting Chapinero validation test
sf_db <- sf_db %>% 
  mutate(is_chapinero = if_else(LocCodigo=="02", "Chapinero", "Otra localidad"))
ggplot()+
  geom_sf(data = loca %>% filter(LocCodigo %in% c('01', '02', '03')))+
          # mapping=aes(fill = 'gray'), show.legend = F)+
  geom_sf(data = sf_db %>% filter(is_train==F), 
          mapping = aes(color = is_chapinero), size = 0.5, show.legend = T)+
  scale_color_manual(values = c("Chapinero"="black","Otra localidad"="blue"))+
  theme_bw()+
  labs(color = 'Localidad')+
  theme(legend.position = 'top', legend.direction = 'horizontal')

ggsave("views/figures/train_dots.pdf",plot = last_plot(), 
       width = 10, height = 16, units = 'cm')

# X. Missing data ------

# X. Export -----
arrow::write_parquet(
  st_drop_geometry(sf_db) %>% select(-c(SHAPE_Leng, SHAPE_Area)),
  sink = "stores/db.parquet")

sf::write_sf(sf_db, "stores/raw/spatial_data/sf_db.gpkg")

# End