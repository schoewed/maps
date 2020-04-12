library(shiny)
library(here)
library(googleway)
library(leaflet)
library(tidyverse)
library(ggmap)
source("google_place_results.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Google Places Search"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        textInput("keyword",
                  "Up to Five Keyword(s) Separated by Commas:","Restaurants, Bars"),
        textInput("address","Address (Centroid):","Steamboat Springs, CO"),
        numericInput("radius","Search Radius in Miles:",5),
        submitButton(),
        downloadButton("download", "Download")),

        # Show a plot of the search output
        mainPanel(
           tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #pull in function from user defined inputs
  user_output <- reactive({
    google_place_results(search_list = stringr::str_trim(stringr::str_split(input$keyword, ",")[[1]]), search_location_ctr = input$address,
                         radius_in_miles = input$radius)})
  
    
    #renders output table
    output$table <- renderTable({
      user_output()})
    
  #provides download option  
   output$download <- downloadHandler(
     filename = function (){
       paste(input$keyword, ".csv", sep = "")},
     content = function (file) {
       write.csv(user_output(), file, row.names = FALSE)})
       
}

# Run the application 
shinyApp(ui = ui, server = server)
