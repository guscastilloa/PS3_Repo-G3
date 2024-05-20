#=============================================================================#
# FECHA:
#   2024-05-20 (ISO 8601)
# AUTOR:
#   Alexander Almeida-Ramírez 
#   a.almeidar@uniandes.edu.co
# DESCRIPCION:
#   Calcular datos espaciales.
#=============================================================================#

# 1. Preparara entorno -----
rm(list=ls())
gc()
cat('\f')
source("scripts/00_packages.R")
here()

# 2. Load data ----
train <- import("stores/raw/train.csv")
validation <- import("stores/raw/test.csv") # doesn't have price tag

train$is_train <- TRUE
validation$is_train <- FALSE

db <- rbind(train, validation)

# Eliminamos las observaciones que no tienen información de latitud o longitud
filtro <- is.na(db$lat) | is.na(db$lon)
db <- db[!filtro, ]

# Observaciones sólo en Bogotá
limites <- getbb("Bogota Colombia")

##Limitamos sólo para Bogotá
filtro1 <- between(db$lon, limites[1, "min"], limites[1, "max"])
filtro2 <- between(db$lat, limites[2, "min"], limites[2, "max"])

db <- db[(filtro1 & filtro2),] 

# Eliminamos las observaciones que no tienen información de latitud o longitud
filtro <- is.na(db$lat) | is.na(db$lon)
db <- db[!filtro, ]


##DISTANCIA A AVENIDAS MAS CERCANAS---------------------------------

db<-st_as_sf(db,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

avenidas <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "highway", value = "secondary")

avenidas_sf <- osmdata_sf(avenidas)

avenidas_geometria <- avenidas_sf$osm_lines%>%
  select (osm_id, name)

#Busco la geometría más cercana
cercano <- st_nearest_feature(db,avenidas_geometria)
#calculo la distancia
dist <-st_distance(db, avenidas_geometria[cercano,], by_element=TRUE)
dist
db$distancia_avenida_principal<-dist


##DISTANCIA A CENTRO COMERCIAL O AREA COMERCIAL----------------------------------------------
available_tags("building")
comercial <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "building", value = "commercial")

#librería sf:
comercial_sf <- osmdata_sf(comercial)
comercial_geometria <- comercial_sf$osm_points%>%
  select (osm_id)


#Convierto base train en objeto sf:
db<-st_as_sf(db,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

#Busco la geometría comercial más cercana:
cercano_com <- st_nearest_feature(db,comercial_geometria)
#calculo la distancia
dist_com <-st_distance(db, comercial_geometria[cercano_com,], by_element=TRUE)
dist_com
db$distancia_comercial<-dist_com

###DISTANCIA A UNIVERSIDADES-------------------------------------


available_tags("building")
universidad <- opq(bbox=getbb("Bogota Colombia"))%>%
  add_osm_feature(key = "building", value = "university")

#librería sf:
universidad_sf <- osmdata_sf(universidad)
universidad_geometria <- universidad_sf$osm_polygons%>%
  select (osm_id, name)

#Convierto base train en objeto sf:
db<-st_as_sf(db,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object

#Busco la geometría comercial más cercana:
cercano_uni <- st_nearest_feature(db,universidad_geometria)
#calculo la distancia
dist_uni <-st_distance(db, universidad_geometria[cercano_uni,], by_element=TRUE)
dist_uni
db$distancia_universidad<-dist_uni

#Exportar las bases de datos. 
class(db)
db_df<-sf_to_df(db, fill = TRUE)
class(train_df)
db_df<-select(db,"property_id", "distancia_avenida_principal", "distancia_comercial","distancia_universidad")
class(db_df)



#Cargar la base de datos principal y unirla con esta base de datos. 
datos<-read_parquet("stores/db.parquet")

#Realizar el merge entre las bases de datos. 
datos_total <- merge(datos,db_df, 
              by=c("property_id"),
              no.dups = TRUE,
              all = TRUE,
              suffixes = "")  
datos_total<-select(datos_total,-"geometry")
arrow::write_parquet(datos_total, sink = "stores/db2.parquet")

