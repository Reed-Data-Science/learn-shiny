library(shiny)
library(tidyverse)
library(DT)

babynames <- read_csv("data/babynames_math241.csv")

# User interface
# User interface
ui <- fluidPage(
  titlePanel(title = "Which Math 241 name is most popular?"),
  sidebarLayout(
    sidebarPanel(
      # Create a text input widget
      selectizeInput(inputId = "names",
                     label = "Enter Math 241 names here",
                     choices = NULL,
                     multiple = TRUE),
      radioButtons(
        inputId = "variable",
        label = "Variable of Interest",
        choices = c("n", "prop"),
        selected = "prop"
      ),
      sliderInput(
        "year_range",
        "Range of Years:",
        min = min(babynames$year),
        max = max(babynames$year),
        value = c(1980, 2010),
        sep = ""
      ),
      actionButton("update", "Update Results!")
    ),
    mainPanel(
      plotOutput(outputId = "graph"),
      DTOutput(outputId = "table")
    )
  )
)

# Server function
server <- function(input, output, session){
  
  updateSelectizeInput(session, 'names', 
                       choices = unique(babynames$name), 
                       server = TRUE)
  
  dat_names <- eventReactive(input$update, {
    babynames %>%
      group_by(year, name) %>%
      summarize(n = sum(n)) %>%
      group_by(year) %>%
      mutate(prop = n/sum(n)) %>%
      filter(name %in% c(unlist(str_split(input$names, " "))),
             year >= input$year_range[1], 
             year <= input$year_range[2]) %>%
      mutate(y_var = .data[[input$variable]])
  })
  
  output$graph <- renderPlot({
    
    ggplot(data = dat_names(), 
           mapping = aes(x = year,
                         y = y_var,
                         color = name)) +
      geom_line(linewidth = 2)
  })
  
  
  dat_names_agg <- reactive({ 
    dat_names() %>%
      group_by(name) %>%
      summarize(count = sum(n)) %>%
      arrange(desc(count))
  })
  
  output$table <-  renderDT({
    
    datatable(dat_names_agg(), 
              options = list(paging = FALSE,
                             searching = FALSE,
                             orderClasses = TRUE))
  })
  
}

# Creates app
shinyApp(ui = ui, server = server)