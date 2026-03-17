library(shiny)
library(mosaic)
library(tidyverse)
library(mosaic)
library(tidyverse)
library(lubridate)
library(glue)


# User interface
ui <- fluidPage(
  titlePanel("Births in 2015"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "weekday",
                         label = "Choose day of the week",
                         choices = unique(Births2015$wday)),
      dateRangeInput("dates",
                     label = "Select Range of Dates",
                     start = "2015-01-01", end = "2015-12-31",
                     min = "2015-01-01", max = "2015-12-31")
    ),
    mainPanel(
      plotOutput(outputId = "graph")
    )
  )
)

# Server function
server <- function(input, output){
  output$graph <- renderPlot({
    Births2015 %>%
      filter(wday %in% input$weekday, 
             date <= input$dates[2], date >= input$dates[1]) %>%
      ggplot(mapping = aes(x = date, y = births,
                           color = wday)) +
      geom_point() +
      theme(legend.position = "bottom")
  })
}

# Creates app
shinyApp(ui = ui, server = server)