library(shiny)
library(mosaic)
library(tidyverse)
library(mosaic)
library(tidyverse)
library(lubridate)



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
      plotOutput(outputId = "graph"),
      textOutput("max_babies")
    )
  )
)

# Server function
server <- function(input, output){
  Births2015_reactive <- reactive({
    Births2015 %>%
      filter(wday %in% input$weekday, 
             date <= input$dates[2], date >= input$dates[1])
    
  })
  
  
  output$graph <- renderPlot({
    Births2015_reactive() %>%
      ggplot(mapping = aes(x = date, y = births,
                           color = wday)) +
      geom_point() +
      theme(legend.position = "bottom")
  })
  
  output$max_babies <- renderText({
    Births2015_max <- Births2015_reactive() %>%
      slice_max(n = 1, births)
    
    
    glue("Between ", format(min(Births2015_reactive()$date), "%A, %B %d, %Y"), " and ",
         format(max(Births2015_reactive()$date), "%A, %B %d, %Y"),
         " and for the selected days of the week, the day with the most babies born was ", 
         format(Births2015_max$date, "%A, %B %d, %Y"),
         " with ", Births2015_max$births,
         " babies born that day!")
  })
}

# Creates app
shinyApp(ui = ui, server = server)


