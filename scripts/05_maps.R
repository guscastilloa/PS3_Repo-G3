##############################################################################-
# DATE:
#   2024/may/10
# AUTHOR:
#  Jorge Congacha
# DESCRIPTION:
#   Map
##############################################################################-

gc()
source("scripts/00_packages.R")

# 2. Load data ----
# (arrow)
db3 <- arrow::read_parquet("stores/db3.parquet")

library(DT, httpgd)

# 5. Descriptive statistics ----
dplyr::glimpse(db3)
head(db3[1:5])
# Missing values
colSums(is.na(db3))/nrow(db3)*100
# Miremos los levels de cada variable
var_levels <- sapply(db3, function(x) length(unique(x)))
var_levels

single_level_vars <- names(var_levels[var_levels == 1])
single_level_vars


#Limpieza y homogenización de la base
p_load(visdat)
vis_dat(dplyr::slice_sample(db3))
# Observamos algunos caracteristicas de las variables numéricas
p_load(stargazer)
stargazer(db3, type="text")
# Calculamos precio por metro cuadrado
db3 <- db3 %>% mutate(surface_total=abs(surface_total))

db3 <- db3 %>%
  mutate(precio_m2 = round(price / surface_total, 0)) %>%
  mutate(precio_m2 = precio_m2 / 1000000)

stargazer(db3["precio_m2"], type="text")

# Visualicemos la distribución de nuestra variable de interés
p <- ggplot(db3, aes(x = price)) +
  geom_histogram(fill = "darkblue", alpha = 0.4) +
  labs(x = "Valor de venta (log-scale)", y = "Cantidad") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()

p

# Guardar la gráfica como un archivo de imagen
ggsave("views/g1.jpg", plot = p, width = 6, height = 4, dpi = 600)

# Elminamos inforacion que no tiene información de latitud o longitud
db3 <- db3 %>%
  filter(!is.na(lat.x) & !is.na(lon.x))
# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = db3$lon.x, 
             lat = db3$lat.x)
# Definimos las observaciones que solo estan dentro de Bogotá
limites <- getbb("Bogotá Colombia")
limites

## Cargamos el shape de Bogotá
db3 <- db3 %>%
  filter(
    between(lon.x, limites[1, "min"], limites[1, "max"]) & 
      between(lat.x, limites[2, "min"], limites[2, "max"])
  )

# Reescalamos la variable de precio por m2 (0-100)
db3 <- db3 %>%
  mutate(precio_m2_r = rescale(precio_m2, to = c(0, 100)))
# Fijamos el centro del mapa
latitud_central <- mean(db3$lat.x)
longitud_central <- mean(db3$lon.x)
# filtramos solo los datos de Chapinero
db3_c <- db3 %>%
  filter(is_chapinero == "Chapinero")
# Creamos el plot
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addCircles(lng = db3_c$lon.x, 
             lat = db3_c$lat.x, 
             fillOpacity = 1,
             opacity = 1,
             radius = db3_c$precio_m2_r*0.2)
# Transformemos los datos de precio de la vivienda a sf para poder graficasr
db3_sf <- st_as_sf(db3, coords = c("lon.x", "lat.x"), crs = 4626)
# grafiquemos solo los datos de Chapinero
dev.off()
ggplot() +
  geom_sf(data=db3_sf%>% filter(is_chapinero == "Chapinero"),aes(color = precio_m2), shape=15, size=1)+
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.text = element_text(size = 15)) +
  theme(legend.key = element_blank())# +
  #labs(title = "Precio en M2 en Chapinero (precio por millones)", title.size = 20, title.align = "center")
# Save the graph 
ggsave("views/g2.jpg", plot = p, width = 6, height = 4, dpi = 600)
