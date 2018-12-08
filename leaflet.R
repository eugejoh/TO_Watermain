
library(here)
library(rgdal)
library(sp)
library(sf)
library(leaflet)
library(dplyr)

list.files("data", "WatermainBreaks_wgs84")

d_raw <- readOGR(here::here("data", "WatermainBreaks_wgs84"))

sf_d <- st_as_sf(d_raw)

sf_d %>% glimpse()

sf::st_crs(sf_d) #CRS, missing EPSG code

sf_d <- sf::st_set_crs(sf_d, 4326) #set the CRS EPSG code to WGS84 which is 4326
# need to change this because Leaflet's default projection is 3857 (Google Mercator)

sf_d <- st_transform(sf_d, crs = sf::st_crs(sf_d)) #now reproject data

# setting leaflet options is OPTIONAL as we just set the CRS for the data
# leaflet(options = leafletOptions(crs = leaflet::leafletCRS(proj4def = sf::st_crs(sf_d))))

sf_d %>% filter(BREAK_YEAR == "2012") %>% 
  select(lng=POINT_X, lat=POINT_Y) %>% 
  leaflet() %>% addTiles() %>% 
  addMarkers(~lng, ~lat)

df <- sf_d
st_geometry(df) <- NULL

pal <- colorFactor(c("navy", "red"), domain = c("2015", "2016"))
leaflet(data = df %>% dplyr::filter(BREAK_YEAR %in% c(2015:2016))) %>% 
  addTiles() %>% 
  addCircleMarkers(~POINT_X, ~POINT_Y, 
                   label = ~BREAK_DATE, color = ~pal(BREAK_YEAR), stroke = FALSE, fillOpacity = 0.75,
                   clusterOptions = markerClusterOptions())




        