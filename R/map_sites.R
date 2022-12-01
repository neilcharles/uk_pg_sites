draw_sites_map <- function(){
  library(tidyverse)
  
  sites <- readr::read_csv('sites.csv')
  
  sites %>% 
    sf::st_as_sf(coords = c("takeoff_lon", "takeoff_lat"), 
                 crs = 4326) %>% 
    leaflet::leaflet() %>% 
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>% 
      leaflet::addCircleMarkers(popup = ~takeoff_name)
}

export_sites <- function(path = NULL){
  readr::read_csv("sites.csv") %>% 
    dplyr::filter(is.na(exclude) | !exclude) %>% 
    dplyr::select(-exclude, -notes) %>% 
    readr::write_csv(path)
  
}
