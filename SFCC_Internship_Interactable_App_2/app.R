library(shiny)
library(ggplot2)
library(dplyr)

# Sample data for demonstration
getwd()
results_full <- read.csv("./data/average_spending_results.csv", header = T)
str(results_full)

data_subset <- read.csv("./data/SFCC_VC_Subset.csv", header = T)
data_subset <- data_subset %>%
  mutate(Scale = factor(paste("Scale", Scale)))
str(data_subset)

# UI
ui <- fluidPage(
  titlePanel("Proposed Management Actions Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_action", "Select Action(s):", 
                  choices = LETTERS[1:20], 
                  selected = LETTERS[1:20], 
                  multiple = TRUE),
      selectInput("selected_scale", "Select Scale(s):", 
                  choices = unique(results_full$scale), 
                  selected = unique(results_full$scale), 
                  multiple = TRUE),
      sliderInput("filter_value", "Filter out values above:", 
                  min = 0, 
                  max = ceiling(max(c(results_full$average_cost, data_subset$Filtered_Costings_Data), na.rm = TRUE) / 1000) * 1000, 
                  value = ceiling(max(c(results_full$average_cost, data_subset$Filtered_Costings_Data), na.rm = TRUE) / 1000) * 1000,
                  step = 10000)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Plot", plotOutput("barPlot", height = "700px", width = "700px")),
        tabPanel("Box Plot", plotOutput("boxPlot", height = "700px", width = "700px"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data_bar <- reactive({
    results_full %>%
      filter(Proposed.Management.Action %in% input$selected_action,
             scale %in% input$selected_scale,
             average_cost <= input$filter_value)
  })
  
  filtered_data_box <- reactive({
    data_subset %>%
      filter(Proposed.Management.Action %in% input$selected_action,
             Scale %in% input$selected_scale,
             Filtered_Costings_Data <= input$filter_value)
  })
  
  output$barPlot <- renderPlot({
    ggplot(filtered_data_bar(), aes(x = Proposed.Management.Action, y = average_cost, fill = Proposed.Management.Action)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Cost by Proposed Management Action for all Scales",
           x = "Proposed Management Action",
           y = "Average Cost") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
      scale_fill_discrete(name = "Action") + 
      facet_wrap(~scale, scales = "free_y", nrow = 5)
  })
  
  output$boxPlot <- renderPlot({
    ggplot(filtered_data_box(), aes(x = Proposed.Management.Action, y = Filtered_Costings_Data, fill = Proposed.Management.Action)) +
      geom_boxplot() + 
      labs(title = "Costs by Proposed Management Action for all Scales",
           x = "Proposed Management Action",
           y = "Cost (£)") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
      scale_fill_discrete(name = "Action") + 
      facet_wrap(~Scale, scales = "free_y", nrow = 5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
