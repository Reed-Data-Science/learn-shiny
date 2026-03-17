#Problem 1
# Load libraries
library(shiny)
library(mosaic)
library(tidyverse)
# User interface
ui <- fluidPage(
  titlePanel("Births in 2015"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "weekday",
                         label = "Choose day of the week",
                         choices = unique(Births2015$wday))
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
      filter(wday %in% input$weekday) %>%
      ggplot(mapping = aes(x = date, y = births,
                           color = wday)) +
      geom_point() +
      theme(legend.position = "bottom")
  })
}
# Creates app
shinyApp(ui = ui, server = server)