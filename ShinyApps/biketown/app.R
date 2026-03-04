library(shiny)
library(tidyverse)
library(lubridate)
library(leaflet)
library(shinythemes)

biketown <- read_csv("biketown.csv") %>%
  filter(Distance_Miles < 1000) %>%
  mutate(StartDate = mdy(StartDate),
         EndDate = mdy(EndDate),
         StartDateTime = ymd_hms(paste(StartDate, StartTime, sep = " ")),
         EndDateTime = ymd_hms(paste(EndDate, EndTime, sep = " "))) %>%
  drop_na(StartLongitude, StartLatitude) 

# Most frequently checkout bikes
popular_bikes <- biketown %>%
  count(BikeID) %>%
  slice_max(n = 2, order_by = n)

biketown_popular <- biketown %>%
  filter(BikeID %in% popular_bikes$BikeID)


pal <- colorFactor("Dark2", biketown_popular$BikeID)



# User interface
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Where do Nike's Biketown Bikes Go?"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("date", "Date",
                                min = min(biketown_popular$StartDateTime), 
                                max = max(biketown_popular$StartDateTime),
                                value = min(biketown_popular$StartDateTime), 
                                animate = TRUE)
                  ),
                  mainPanel(leafletOutput("biketown_map"))
                )
                
)

# Server function
server <- function(input, output){
  
  # Reactive data
  filtered_biketown <- reactive({
    biketown_popular %>%
      filter(StartDateTime <= input$date)
  })
  
  # Base map (notice I am not providing the reactive dataset!)
  output$biketown_map <- renderLeaflet({
    
    biketown_popular %>%
      leaflet() %>%
      addTiles() %>%
      fitBounds(~min(StartLongitude), ~min(StartLatitude), 
                ~max(StartLongitude), ~max(StartLatitude))
  })
  
  # Here is where we provide the reactive dataset
  observe({
    
    leafletProxy("biketown_map", data = filtered_biketown()) %>%
      clearShapes() %>%
      addCircles(lng = ~StartLongitude, lat = ~StartLatitude, 
                 radius = 10, color = ~pal(BikeID), fillOpacity = 0.4,
                 opacity = 0.4)
  })
  
  
}

# Creates app
shinyApp(ui = ui, server = server)
