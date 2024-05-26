
cat("Run under: ", version$version.string)

# Load (or install) pacman library
if(!require(pacman)){install.packages('pacman') ; require(pacman)}

p_load(
  # Basic Functions
  arrow,
  sfheaders,
  VIM,
  tidyverse,
  janitor,
  here,
  rio,
  stringi,
  readr,
  skimr,
  # Spatial packages
  sf,
  leaflet,
  spatialsample,
  osmdata,
  tmaptools,
  caret,
  
  # Machine learning
  tidymodels,
  xgboost,
  gbm,
  SuperLearner
  )


# Functions
here <- here::here()
