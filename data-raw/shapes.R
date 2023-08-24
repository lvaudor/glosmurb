library(sf)
library(osmdata)
## code to prepare `confluence` dataset goes here
LyonOSM=getbb("Lyon, France",format_out="sf_polygon")[[1]] %>%
  st_make_valid() %>%
  .[2]
crs_osmdata=st_crs(LyonOSM)
usethis::use_data(crs_osmdata, overwrite = TRUE)

# CONFLUENCE
## code to prepare `shape_confluence` dataset goes here
points <- data.frame(
  lon = c(4.7997, 4.8396),  # Coordonnées de longitude des deux points
  lat = c(45.7406,45.7248)  # Coordonnées de latitude des deux points
)
# Convertir l'objet data.frame en un objet sf de type POINT
points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)
confluence <- st_as_sfc(st_bbox(points_sf)) %>%
  st_transform(crs_osmdata)
#confluence=st_transform(confluence, crs=crs_osmdata)
usethis::use_data(confluence, overwrite = TRUE)

# CONDRIEU
points <- data.frame(
  lon= c(4.7, 4.8),
  lat=c(45.4,45.50)
)
points_sf <- st_as_sf(points, coords=c("lon", "lat"), crs= 4326)
condrieu <- st_as_sfc(st_bbox(points_sf)) %>%
  st_set_crs(crs_osmdata)
lyon=sf::st_read("data-raw/study_areas_temp/study_area_examples.shp")[1,] %>%
  st_as_sfc() %>%
  st_transform(crs_osmdata)
usethis::use_data(lyon, overwrite=TRUE)

condrieu=sf::st_intersection(lyon,condrieu)
usethis::use_data(condrieu, overwrite = TRUE)

