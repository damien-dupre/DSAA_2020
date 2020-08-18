################################################################################
#                        Distance to OSM Features                              #
################################################################################

# libraries --------------------------------------------------------------------
library(tidyverse)
library(osmdata)
library(sp)
library(sf)

# options ----------------------------------------------------------------------
# https://wiki.openstreetmap.org/wiki/Overpass_API
# osmdata::get_overpass_url()
osmdata::set_overpass_url('https://overpass-api.de/api/interpreter')

# data -------------------------------------------------------------------------
geocode_dublin_2018 <- here::here("data/geocode_dublin_2018.rds") %>% 
  readr::read_rds()

# case with 1 OSM feature ######################################################
# convert data to sf class
data_sf <- st_as_sf(geocode_dublin_2018, coords = c("lng", "lat"))
st_crs(data_sf) <- st_crs(4326) # assign crs
data_sf <- st_transform(data_sf, crs = 32721) # transform

# OSM feature ------------------------------------------------------------------
variable_osm <- osmdata::opq("Dublin, Ireland") %>%
  osmdata::add_osm_feature(key = "leisure", value = "park")

# variable_osm %>%
#   osmdata::osmdata_sp() %>%
#   magrittr::use_series(osm_points) %>%
#   sp::plot()

variable_sf <- variable_osm %>% 
  osmdata::osmdata_sf() %>%
  magrittr::use_series(osm_points) %>% 
  magrittr::use_series(geometry) %>% 
  sf::st_sf()

st_crs(variable_sf) <- st_crs(4326) # assign crs
variable_sf <- st_transform(variable_sf, crs = 32721) # transform
variable_sf <- st_combine(variable_sf)

# distance to OSM feature ------------------------------------------------------
# by_element = TRUE returns the shorter distance
geocode_dublin_2018$variable <- st_distance(x = data_sf, y = variable_sf)

# Case with all OSM features ###################################################
# manual list OSM features -----------------------------------------------------
list_osm_features <- file.path(data_path, "list_osm_features.rds") %>% 
  readr::read_rds() %>% 
  dplyr::filter(value != "*") %>% 
  dplyr::filter(!stringr::str_detect(value, "/|[(]|User Defined|Number|Date|Name|see opening_hours|[:digit:]")) %>% 
  dplyr::filter(!stringr::str_detect(key, ":"))

# automatic list OSM features --------------------------------------------------
# list_osm_features <- osmdata::available_features() %>% 
#   tibble::enframe(name = NULL, value = "key") %>% 
#   dplyr::group_by(key) %>% 
#   dplyr::summarise(osm_tag_processing(key))

# batch distance ---------------------------------------------------------------
for (i in 1:nrow(list_osm_features)) {
  key <- list_osm_features[i, "key"]
  value <- list_osm_features[i, "value"]
  variable_name <- paste(key, value, sep = "_")
  
  paste(i, variable_name) %>% print()
  # obtain gps coordinates of geographic variables (e.g., cycle lanes)
  
  variable_osm <- osmdata::opq('Dublin, Ireland') %>%
    osmdata::add_osm_feature(key = key, value = value)
  
  variable_sf <- variable_osm %>% 
    osmdata::osmdata_sf() %>%
    magrittr::use_series(osm_lines) %>% 
    magrittr::use_series(geometry) %>% 
    st_sf()
  
  if (nrow(variable_sf) == 0) {
    
    geocode_dublin_2018 <- geocode_dublin_2018 %>% 
      dplyr::mutate(!!variable_name := NA)
    
  } else {
    
    st_crs(variable_sf) <- st_crs(4326) # assign crs
    variable_sf <- st_transform(variable_sf, crs = 32721) # transform
    variable_sf <- st_combine(variable_sf)
    
    # distance
    geocode_dublin_2018 <- geocode_dublin_2018 %>% 
      dplyr::mutate(
        !!variable_name := st_distance(x = data_sf, y = variable_sf) %>% as.numeric()
      )
  }
  Sys.sleep(5)
}