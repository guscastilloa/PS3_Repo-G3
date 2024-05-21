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


# Distancia a un gimnasio ----------------------------------------------------------
db<-st_as_sf(db,coords=c("lon","lat"),crs=4326,remove=FALSE) #as an sf object
Gym <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "fitness_centre") 
Gym_sf <- osmdata_sf(Gym)
Gym_geometria <- Gym_sf$osm_polygons %>% 
  select(osm_id, name)

#Busco la geometría más cercana
cercano <- st_nearest_feature(db,Gym_geometria)
#calculo la distancia
dist <-st_distance(db, Gym_geometria[cercano,], by_element=TRUE)
dist
db$distancia_gimnasio<-dist

## Distancia a estaciones de TR ------------------------------------------
transmi <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bus_station") 
transmi_sf <- osmdata_sf(transmi)
transmi_geometria <- transmi_sf$osm_polygons %>% 
  select(osm_id, name)

#Busco la geometría más cercana
cercano <- st_nearest_feature(db,transmi_geometria)
#calculo la distancia
dist <-st_distance(db, transmi_geometria[cercano,], by_element=TRUE)
db$distancia_transmi<-dist

#Distancia a Cai. 
cai <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "police") 
cai_sf <- osmdata_sf(cai)
cai_geometria <- cai_sf$osm_polygons %>% 
  select(osm_id, name)

#Busco la geometría más cercana
cercano <- st_nearest_feature(db,cai_geometria)
#calculo la distancia
dist <-st_distance(db, cai_geometria[cercano,], by_element=TRUE)
db$distancia_cai<-dist


#Distancia a bares. 
bar <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bar") 
bar_sf <- osmdata_sf(bar)
bar_geometria <- bar_sf$osm_polygons %>% 
  select(osm_id, name)

#Busco la geometría más cercana
cercano <- st_nearest_feature(db,bar_geometria)
#calculo la distancia
dist <-st_distance(db, bar_geometria[cercano,], by_element=TRUE)
db$distancia_bar<-dist


# Distancia a supermercados ----------------------------------------------------------
SM <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "shop" , value = "supermarket") 
SM_sf <- osmdata_sf(SM)
SM_geometria <- SM_sf$osm_polygons %>% 
  select(osm_id, name)

#Busco la geometría más cercana
cercano <- st_nearest_feature(db,SM_geometria)
#calculo la distancia
dist <-st_distance(db, SM_geometria[cercano,], by_element=TRUE)
db$distancia_SM<-dist

#Distancia a colegios 
colegios <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "school") 
colegios_sf <- osmdata_sf(colegios)
colegios_geometria <- colegios_sf$osm_polygons %>% 
  select(osm_id, name)


#Busco la geometría más cercana
cercano <- st_nearest_feature(db,colegios_geometria)
#calculo la distancia
dist <-st_distance(db,colegios_geometria[cercano,], by_element=TRUE)
db$distancia_colegio<-dist

# Distancia a hospitales ----------------------------------------------------------
hospitales <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "hospital") 
hospitales_sf <- osmdata_sf(hospitales)
hospitales_geometria <- hospitales_sf$osm_polygons %>% 
  select(osm_id, name)

#Busco la geometría más cercana
cercano <- st_nearest_feature(db,hospitales_geometria)
#calculo la distancia
dist <-st_distance(db,hospitales_geometria[cercano,], by_element=TRUE)
db$distancia_hospitales<-dist

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
db_df<-select(db,"property_id","distancia_gimnasio","distancia_transmi","distancia_cai",
              "distancia_bar","distancia_SM","distancia_colegio","distancia_hospitales",
              "distancia_avenida_principal", "distancia_comercial","distancia_universidad")

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

