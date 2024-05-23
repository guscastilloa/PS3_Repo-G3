#=============================================================================#
# FECHA:
#   2024-05-15 (ISO 8601)
# AUTOR:
#   Gustavo Castillo | ga.castillo@uniandes.edu.co
# DESCRIPCION:
#   Construcción de variables con submercados usando K-means clustering.
#=============================================================================#

# 1. Preparara entorno -----
rm(list=ls())
gc()
cat('\f')
source("scripts/00_packages.R")
here()

# 2. Load data ----
db <-read_parquet("stores/db3.parquet")


# Normalize the latitude and longitude columns
train_df <- filter(db, is_train == T) %>%
  mutate(across(c(lat, lon), ~ rescale(.)))

test_df <- filter(db, is_train == F) %>%
  mutate(across(c(lat, lon), ~ rescale(.)))

# Set seed for reproducibility
set.seed(42)

# Perform K-Means clustering on the training data
kmeans_result <- kmeans(train_df %>% select(lat, lon), centers = 5)

# Add cluster labels to the training data
train_df$kmeans5_submarket <- kmeans_result$cluster

# Function to assign clusters to new data based on centroids
assign_clusters <- function(data, centers) {
  distances <- as.matrix(dist(rbind(data, centers)))[1:nrow(data), (nrow(data)+1):(nrow(data)+nrow(centers))]
  clusters <- max.col(-distances)
  return(clusters)
}

# Predict clusters for the test data
test_df$kmeans5_submarket <- assign_clusters(test_df %>% select(lat, lon), kmeans_result$centers)



# Assess created clusters: compare with localidades
sf_db <- st_as_sf(left_join(x=db, y=train_df %>% select(property_id, kmeans5_submarket), by = 'property_id'),
                  coords = c('lon', 'lat'), crs =  4686)

ggplot()+
  geom_sf(data = loca %>% filter(!(LocCodigo %in% c('20'))),
  mapping=aes(fill = LocNombre), show.legend = FALSE)+
  geom_sf(data = sf_db %>% filter(is_train==T),
          mapping = aes(color = alpha(kmeans5_submarket, 0.2)),size = 0.5)+
          # , colour = alpha('red', 0.4))+
  coord_sf()


# Exportar ------------
train <- import("stores/raw/train.csv")
validation <- import("stores/raw/test.csv") # doesn't have price tag
train$is_train <- TRUE
validation$is_train <- FALSE
temp <- rbind(train, validation) %>% select(property_id, lon, lat)
datos <- left_join(x = datos, y = temp, by = 'property_id')

tmp <- rbind(train_df %>% select(property_id, kmeans5_submarket),
             test_df %>% select(property_id, kmeans5_submarket))
db <- left_join(db, tmp, by = 'property_id')
arrow::write_parquet(datos, sink = "stores/db3.parquet")
