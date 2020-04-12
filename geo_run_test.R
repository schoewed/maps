library(here)
library(googleway)
library(leaflet)
library(tidyverse)
library(ggmap)
source("google_place_results.R")



#read in data and define api key
data <- read.csv("Copy of Detroit Geocode List.csv")
data$cat <- as.character(data$Cat)
str(data)
api_key <- "AIzaSyAppXc3urvv_tMrB3YrsJTOZOgnTxAYM7Q"
search_list <- as.character(data$Cat)[1:4]
View(search_list)

#run the geocoder
output <- google_place_results(search_list, "3737 Woodward Avenue, Detroit, MI 48201", 2.75, api_key)

# max results
60*length(search_list)


#summarize output
out_sum <- group_by(output, search_id) %>% 
  summarise(n = n())


#filter for unique places
out_unique <- output %>% 
  distinct(output, formatted_address, .keep_all = TRUE)
View(out_unique)

#summarize filtered data
out_sum_uni <- group_by(out_unique, search_id) %>% 
  summarise(n = n())

unique(out_unique$search_id)

str(out_unique$search_id)
pal <- colorFactor(palette = c("blue",
                               "darkgreen",
                               "purple",
                               "yellow"),
                   levels = unique(out_unique$search_id))

leaflet(data=out_unique) %>%
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(color = ~pal(out_unique$search_id),
                   weight = 1,
                   label = paste0(out_unique$name,", ", out_unique$search_id),
                   opacity = 0.8,
                   fillOpacity = 0.5,
                   radius = 4) %>% 
  addLegend(data = output,
            position = "bottomright",
            pal = pal,
            values = out_unique$search_id)



#output results
write.csv(out_unique, "search_output_test.csv")
