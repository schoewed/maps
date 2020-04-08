#load libraries needed for analysis
library(here)
library(tidyverse)
library(plotly)
library(ggmap)
library(leaflet)
library(tidycensus)
library(tigris)
library(sf)
library(htmltools)
library(broom)
library(RColorBrewer)
library(DT)
library(leaflet.extras)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

#get msas and counties
cb <- core_based_statistical_areas(cb = TRUE) %>% 
  st_transform(cb, crs = "+init=epsg:4326")

str(cb$NAME)
co <- counties(cb = TRUE) %>% 
  st_transform(co, crs = "+init=epsg:4326")

#define metro and index to county codes
metro_name <- "Boulder"
metro <- filter(cb, grepl(metro_name, NAME))
metro_ct <-st_centroid(metro) %>% 
  st_coordinates(metro_ct)
cocodes <- co[metro,]$GEOID

#retrieve tracts
tracts_output <- NULL
for (i in cocodes) {
  cty_tracts <- get_acs(geography = "tract",
                        variables = c(med_inc = "B19013_001", pop = "B01003_001", hh = "B09019_001"),
                        state = substr(i, 1, 2),
                        county = substr(i, 3, 5),
                        year = 2018,
                        output = "wide",
                        geometry = TRUE) %>% 
    mutate(cty_code = i)
  assign('tracts_output', rbind(cty_tracts, tracts_output))
}
tracts_output <- st_transform(tracts_output, crs = "+init=epsg:4326")


#get roads available at state level
st_roads <- NULL
uni_states <- unique(substr(cocodes, 1, 2))
uni_states
for (i in uni_states) {
  int_rd <- primary_secondary_roads(as.numeric(substr(i, 1, 2)), year = 2018) %>% 
    filter(RTTYP %in% c("I", "S", "U"))
  assign("st_roads", rbind(int_rd, st_roads))}
st_roads <- st_transform(st_roads, crs = "+init=epsg:4326")
#ggplot() + geom_sf(data = tracts_output) + geom_sf(data = st_roads, aes(color = "red"))


#get linear water
county_river <- NULL
for (i in cocodes) {
  int_riv <- linear_water(as.numeric(substr(i, 1, 2)),
                                             as.numeric(substr(i, 3, 5)))
  assign("county_river", rbind(int_riv, county_river))}
county_river <- st_transform(county_river, crs = "+init=epsg:4326")

#get area water
county_lake <- NULL
for (i in cocodes) {
  int_lake <- area_water(as.numeric(substr(i, 1, 2)),
                          as.numeric(substr(i, 3, 5)))
  assign("county_lake", rbind(int_lake, county_lake))}
county_lake <-st_transform(county_lake, crs = "+init=epsg:4326")



#map it
pal <- colorNumeric("Oranges", tracts_output$med_incE)

leaflet(options = leafletOptions(maxZoom = 12)) %>% 
  addProviderTiles("CartoDB") %>% 
  addPolygons(data = tracts_output,
              weight = 0,
              color = "black",
              fillColor = ~pal(tracts_output$med_incE),
              fillOpacity = 0.7,
              popup = ~paste0(NAME, "<br> <strong>Median HH Income: </strong>$", tracts_output$med_incE),
              smoothFactor = 0,
              group = "Tracts") %>%
  addPolygons(data = county_lake,
              weight = 0,
              color = "lightblue",
              fillColor = "lightblue",
              group = "Water",
              fillOpacity = 1) %>% 
  addPolylines(data = county_river,
               weight = 0.7,
               color = "light blue",
               opacity = 1,
               smoothFactor = 0,
               group = "Water") %>% 
  addPolylines(data = st_roads,
               weight = 0.7,
               color = "gray",
               opacity = 1,
               smoothFactor = 2,
               group = "Roads") %>% 
  addPolygons(data = metro,
              weight = 2,
              color = "black",
              fillColor = NULL,
              group = "MSA Boundary",
              opacity = 0,
              fillOpacity = 0) %>% 
  addLayersControl(overlayGroups = c("Tracts", "Roads", "Water", "MSA Boundary")) %>% 
  addLegend(data = tracts_output,
            position = "bottomright",
            pal = pal,
            values = tracts_output$med_incE,
            title = "Median HH<br>Income, 2018") %>% 
  addSearchOSM() %>% 
  addResetMapButton() %>% 
  setView(metro_ct, lng = metro_ct[1,1], lat = metro_ct[1,2], zoom = 8)

