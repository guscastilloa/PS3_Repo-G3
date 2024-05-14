
cat("Run under: ", version$version.string)

# Load (or install) pacman library
if(!require(pacman)){install.packages('pacman') ; require(pacman)}

p_load(
  # Basic Functions
  tidyverse,
  janitor,
  here,
  rio,
  stringi,
  # Spatial packages
  sf,
  leaflet,
  spatialsample
  )


# Functions
here <- here::here()
