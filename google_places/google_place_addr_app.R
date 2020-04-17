library(shiny)
library(here)
library(googleway)
library(leaflet)
library(tidyverse)
library(ggmap)
source("google_place_result_addr.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Google Place Search with Address List"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        textInput("keyword","Enter Search String of Interest:","Grocery Store"),
        fileInput("addr_file","Upload CSV of Addresses in Column A:",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        checkboxInput("header", "CSV Header in Row 1?", TRUE),
        numericInput("radius","Search Radius in Miles:",0.5),
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
  
  #load in csv of addresses
  file_data <- reactive({
    in_file <- input$addr_file
    if (is.null(in_file)) {
      return (NULL)}
    in_file <- read.csv(in_file$datapath, header = input$header, stringsAsFactors = FALSE) 
    in_file <- as.vector(in_file[,1])
    return(in_file)
  })
  
  #pull in function from user defined inputs
  user_output <- reactive({
    google_place_result_addr(search_list = input$keyword,
                         search_location_ctr = file_data(),
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
