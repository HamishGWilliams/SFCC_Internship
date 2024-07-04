# Load Packages
library(dplyr)
library(ggplot2)
library(shiny)
library(here)
library(rsconnect)

# Load Data outside of server
global_data <- read.csv("data/SFCC_data.csv", header = T)

# Connect to server
rsconnect::setAccountInfo(name='hamish-williams', token='6C0A215BD2F0A4C1F14D69F38D667B5D', secret='ZZYWiT9nyNA8udsuxvg9TiYrZ0kZIODHrp4KBYug')

# Define UI for application
ui <- fluidPage(
  titlePanel("Funding Shortfall interactable boxplot"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("management_action", "Select Proposed.Management.Action:",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      selectInput("district", "Select District:",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      selectInput("confidence", "Select Confidence Level:",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE),
      selectInput("scale", "Select scale:",
                  choices = NULL,
                  selected = NULL,
                  multiple = TRUE)
    ),
    
    mainPanel(
      plotOutput("boxplot")
    )
  )
)

# Define server logic required to draw a boxplot
server <- function(input, output, session) {
  # Update the selectInput choices dynamically based on the loaded data
  observe({
    updateSelectInput(session, "management_action", choices = unique(global_data$Proposed.Management.Action))
    updateSelectInput(session, "district", choices = unique(global_data$District))
    updateSelectInput(session, "confidence", choices = unique(global_data$Costing.Certainty))
    updateSelectInput(session, "scale", choices = unique(global_data$Scale...5.District..4.catchment..3.river..2.coastal..1.local))
  })
  
  # Reactive data filtering
  filtered_data <- reactive({
    global_data %>%
      filter(
        Proposed.Management.Action %in% input$management_action,
        District %in% input$district,
        Costing.Certainty %in% input$confidence,
        Scale...5.District..4.catchment..3.river..2.coastal..1.local %in% input$scale,
        Funding.Shortfall > 0
      )
  })
  
  # Render the boxplot
  output$boxplot <- renderPlot({
    ggplot(filtered_data(), aes(x = Proposed.Management.Action, y = Funding.Shortfall, fill = District, color = District)) +
      geom_boxplot(alpha = 0.3) +
      theme_bw() +
      labs(
        title = "Boxplot by Proposed.Management.Action and District", 
        x = "Proposed.Management.Action", 
        y = "Funding Shortfall"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

#rsconnect::deployApp(forceUpdate = TRUE, appName = "SFCC_interactable_shiny_app")
