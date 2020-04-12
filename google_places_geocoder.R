### GOAL: USE GOOGLE API TO GENERATE CSV OF SEARCH RESULTS (WITH A FIELD FOR NAME OF SEARCH)
### KEY TASKS: NEED TO FIGURE OUT HOW TO LOOP THROUGH GOOGLE PAGES AND THEN ADD BIND TOGETHER WITH SEARCH NAME
### THIS CODE RETURNS 60 RESULTS

#using google api to access maps
library(here)
library(ggmap)
library(googleway)
library(rvest)
library(leaflet)
library(rgdal)
library(sf)
library(tidyverse)

register_google(key = "AIzaSyAVXbEN8p-3vD0gKEicmKejGVGCO1Bf2KY", write = TRUE)
api_key <- 'AIzaSyAVXbEN8p-3vD0gKEicmKejGVGCO1Bf2KY'
getwd()


#steamboat_ctr <- geocode("Steamboat Springs, CO")
#steamboat_map <- get_map(steamboat_ctr, zoom = 13, maptype = "roadmap")
#ggmap(steamboat_map)

#data <- read.csv("Copy of Detroit Geocode List.csv")
#length(data$Cat)
#head(data)

#inputs for search - change change to custom list or list of vectors from csv
search_type <- c("restaurants", "pharmacy", "hair salons")
search_loc <- "3737 Woodward Avenue, Detroit, MI 48201"
rad <- 4425
output <- NULL
map_ctr <- geocode(search_loc, output = "latlon")

#for each item in search list, do search
for (i in search_type){
  res <- googleway::google_places(search_string = paste0(i," ",search_loc),
                                radius=rad,
                                key=api_key)
  
  #if results, pull first 20 records
  res_results <- as.data.frame(res$results)
  geometry <- res_results$geometry$location
  res_results <- select(res_results, c(name, formatted_address))
  res_results <- cbind(res_results,geometry) %>% 
    mutate(search_id = i)

  #define pagination token  
  token <- res$next_page_token
  #sleep for two secs
  message("first page pulled, sleeping")
  Sys.sleep(2)
  
  #if token isn't null (i.e. there are more results), then pull next 20, otherwise move to next item in list
  if (!is.null(token)) {
    res2 <- googleway::google_places(search_string = paste0(i," ",search_loc),
                                     radius=rad,
                                     page_token=token,
                                     key=api_key)
    res_next <- as.data.frame(res2$results)
    geometry <- res_next$geometry$location
    res_next <- select(res_next, c(name, formatted_address))
    res_next <- cbind(res_next,geometry)%>%
      mutate(search_id = i)
    
    #redefine token and rest
    token <- res2$next_page_token
    message("second page pulled, sleeping")
    Sys.sleep(2)
    }
  else {
    res_next <- NULL
    break
  }
  
  #if pg 2 token isn't null (i.e. there are more results), then pull next 20, otherwise move to next item in list
  if (!is.null(token)) {
    res3 <- googleway::google_places(search_string = paste0(i," ",search_loc),
                                     radius=rad,
                                     page_token=token,
                                     key=api_key)
    res_last <- as.data.frame(res3$results)
    geometry <- res_last$geometry$location
    res_last <- select(res_last, c(name, formatted_address))
    res_last <- cbind(res_last,geometry)%>%
      mutate(search_id = i)
    
    #redefine token and rest
    token <- res3$next_page_token
    message("third page pulled, sleeping")
    Sys.sleep(2)
  }
  else {
    res_last <- NULL
    break
  }
  
  #combine pages
  res_final <- rbind(res_results,res_next,res_last)
  #assign pages for each variable to output dataframe
  assign('output', rbind(res_final, output))
}


#how many results should there be
length(search_type)*20*3

#check out results
length(unique(output$formatted_address))
output <- left_join(output, data, by = c("search_id" = "Cat"))
View(output)

#summarize output
out_sum <- group_by(output, search_id) %>% 
  summarise(n = n())
View(out_sum)

#filter for unique places
out_unique <- output %>% 
  distinct(output, formatted_address, .keep_all = TRUE)

#summarize filtered data
out_sum_uni <- group_by(out_unique, search_id) %>% 
  summarise(n = n())

#output results
write.csv(out_unique, "search_output_test.csv")



#create map
unique(output$RCLCO)
View(output$search_id)
pal <- colorFactor(palette = c("blue", "darkgreen", "purple", "yellow", "orange", "lightblue", "red"), levels = unique(output$RCLCO))

leaflet(data=output) %>%
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(color = ~pal(output$RCLCO),
                   weight = 1,
                   label = paste0(output$name,", ", output$search_id),
                   opacity = 0.8,
                   fillOpacity = 0.5,
                   radius = 4) %>% 
  addLegend(data = output,
            position = "bottomright",
            pal = pal,
            values = output$RCLCO) %>% 
  setView(map_ctr)

