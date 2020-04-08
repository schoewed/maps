library(shiny)
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


#county codes function for tracts
source(here("county_codes.R"))
#state roads function for roads
source(here("state_roads.R"))
#area water function
source(here("county_area_water.R"))

#need cbsas for user input
cb <- core_based_statistical_areas(cb = TRUE) %>% 
  st_transform(cb, crs = "+init=epsg:4326")


# Define UI for application that draws a map
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("metro_selection", "Select MSA:", sort(cb$NAME)))
)

# Define server logic required to map
server <- function(input, output) {
  
  #for tracts
  user_selection <- reactive({
    county_codes(metro_name = input$metro_selection)
  })
  
  #for roads
  user_roads <- reactive({
    state_roads(metro_name = input$metro_selection)
  })
  
  #for lakes
  user_lake <- reactive ({
    county_area_water(metro_name = input$metro_selection)
  })
  
  #add reactive for metro
  user_metro <- reactive({
    filter(cb, cb$NAME == input$metro_selection) %>% 
      st_centroid() %>% 
      st_coordinates()
  })
  
  
  
  #generate base map
  output$map <- renderLeaflet({
    
    pal <- colorNumeric("Oranges", user_selection()$med_incE)
    
    leaflet(options = leafletOptions(maxZoom = 15)) %>% 
      addProviderTiles(providers$CartoDB, options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = user_selection(),
                  weight = 0,
                  color = "black",
                  fillColor = ~pal(user_selection()$med_incE),
                  fillOpacity = 0.7,
                  popup = ~paste0(NAME, "<br> <strong>Median HH Income: </strong>$", user_selection()$med_incE),
                  smoothFactor = 0,
                  group = "Tracts") %>% 
      addPolylines(data = user_roads(),
                   weight = 0.8,
                   color = "gray",
                   opacity = 1,
                   smoothFactor = 2,
                   group = "Roads") %>%
      addPolygons(data = user_lake(),
                  weight = 0,
                  color = "lightblue",
                  fillColor = "lightblue",
                  group = "Water",
                  fillOpacity = 1) %>% 
      addLegend(data = user_selection(),
                position = "bottomright",
                pal = pal,
                values = user_selection()$med_incE,
                title = "Median HH<br>Income, 2018") %>% 
      addSearchOSM() %>% 
      addResetMapButton() %>% 
      setView(lng = user_metro()[1,1], lat = user_metro()[1,2], zoom = 10)
  })
  

  
  #observe({
    
    #pal <- reactive({
      #colorNumeric("Oranges", user_selection()$med_incE)})
      
    #leafletProxy("map", data = user_selection()) %>% 
      #addPolygons(weight = 1,
                  #color = "black",
                 #fillColor = ~pal(user_selection()$med_incE),
                  #fillOpacity = 0.7,
                  #popup = ~paste0(NAME, "<br> <strong>Median HH Income: </strong>$", user_selection()$med_incE),
                  #smoothFactor = 0,
                  #group = "Tracts")
  #})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
