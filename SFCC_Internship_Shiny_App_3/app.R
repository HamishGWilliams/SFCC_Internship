#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages ----
library(shiny)
library(dplyr)
library(ggplot2)
library(here)
library(rsconnect)
library(openxlsx)

# Connect to server ----
rsconnect::setAccountInfo(name='hamish-williams', token='6C0A215BD2F0A4C1F14D69F38D667B5D', secret='ZZYWiT9nyNA8udsuxvg9TiYrZ0kZIODHrp4KBYug')

# Load Data ----
    # Function to safely read and convert data ----
load_and_convert_data <- function(file_path) {
  data <- read.csv(file_path, header = TRUE)
  
  if (nrow(data) == 0) {
    stop(paste("The file", file_path, "is empty or could not be read."))
  }
  
  data$Proposed.Management.Action <- as.factor(data$Proposed.Management.Action)
  data$District <- as.factor(data$District)
  data$Scale <- as.factor(data$Scale)
  data$Costing.Certainty <- as.factor(data$Costing.Certainty)
  
  # Convert to numeric and handle potential NAs or empty columns
  data$Average_Funding_Shortfall <- as.numeric(as.character(data$Average_Funding_Shortfall))
  data$Average_Filtered_Costings_Data <- as.numeric(as.character(data$Average_Filtered_Costings_Data))
  data$Count <- as.numeric(as.character(data$Count))
  
  # Check if any conversion resulted in all NAs
  if (all(is.na(data$Average_Funding_Shortfall))) {
    stop("Conversion to numeric for Average_Funding_Shortfall failed.")
  }
  if (all(is.na(data$Average_Filtered_Costings_Data))) {
    stop("Conversion to numeric for Average_Filtered_Costings_Data failed.")
  }
  if (all(is.na(data$Count))) {
    stop("Conversion to numeric for Count failed.")
  }
  
  return(data)

}

   # Load your data outside of the server function ----
getwd()
#calculated_averages <- load_and_convert_data("./SFCC_Internship_SHiny_App_3/data/calculated_averages.csv")
calculated_averages <- load_and_convert_data("./data/calculated_averages.csv")



# Define UI for application ----
ui <- fluidPage(
  
  # Custom CSS to center the button ----
  tags$head(
    tags$style(HTML("
      .centered {
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .centered img {
        margin: 0 20px; /* Adjust the margin value as needed */
      }
      .center-text {
        text-align: center;
      }
      .left-align-center {
        margin: 0 auto;
        max-width: 800px; /* Adjust this width as needed */
      }
      .left-align-center p {
        text-align: left;
      }
      .tabset-panel-centered .nav-tabs {
        display: flex;
        justify-content: center;
      }
      .tabset-panel-centered .nav-tabs > li {
        float: none;
        display: inline-block;
      }
      .center-table {
        margin: 0 auto;
        width: 800px;
      }
      .center-table table {
        width: 100%;
        border-collapse: collapse;
      }
      .center-table th, .center-table td {
        border: 1px solid #b8b8b8;
        text-align: left;
        padding: 8px;
      }
      .center-table th {
        background-color: #abdcde;
        color: #333;
      }
      .center-table tr:nth-child(even) {
        background-color: #f9f9f9;
      }
      .center-table tr:hover {
        background-color: #e0e0e0;
      }
    "))
  ),
  
  tags$style(type = 'text/css', 
             '#update { 
             display: block; 
             margin-left: auto; 
             margin-right: auto; 
             } 
             .centered { 
             text-align: center; 
             } 
             .spacer { margin-bottom: 20px; }'),
  
  # Top banner with logos ----
  fluidRow(
    column(12, div(class = "centered",
                   img(src = 'https://scotland.shinyapps.io/sg-pressures-wild-atlantic-salmon-scotland/_w_c9337153/SFCC%20Logo.jpg', height = '75px'),
                   img(src = 'https://scotland.shinyapps.io/sg-pressures-wild-atlantic-salmon-scotland/_w_c9337153/FMS%20logo%20white%20new%20(hi-res).jpg', height = '75px')
    ))
  ),
  
  # Main title text ----
  div(class = "centered",
    titlePanel(strong("SFCC Internship - Funding Shortfall Evaluation"))),
  
  # Centered paragraph ----
  fluidRow(
    column(12, div(class = "center-text",
                   p("Work in Progress")
    ))
  ),
  
  # Tabs ----
div(class = "tabset-panel-centered",
  tabsetPanel(
    # Welcome Page ----
    tabPanel("Welcome", # Tab Name
             div(class = "center-text",
                 h2("Welcome Content")
             ),
             div(class = "spacer left-align-center", # Container for paragraphs
                 p("As part of the Scottish Fishereies Coordination Centre's 
                       goals to collate, analyse, interpret, and distribute 
                       information and data to stakeholders, an internship with the 
                       Univeristy of Aberdeen was utilised to create and curate a
                       tool to allow for investigation of the Funding Shortfalls
                       of proposed management actions from the 'Fisheries 
                       Management Scotland' actions dataset."),
                 p("The current app is aimed to provide users with the opportunity
                       to explore the data collated to present (2024). Using this tool
                       users can see the ranges and average funding shortfalls for 
                       specific actions and districts, as well as the numbers of 
                       inidividual actions both at the action and district levels for 
                       a chosen set of scales and confidence levels."),
                 p("This tool is only in its initial phase, and feedback on the
                       presentation of results, and needs for further options would
                       be insightful for aiding the SFCC provide the tools and resources
                       stakeholders want and need.")
             ),
             
             fluidRow(
               column(12, div(class = "spacer centered",
                              img(src = 'https://fms.scot/wp-content/uploads/2021/11/Leaping-Salmon-Sean-Dugan.gif', height = '250px'),
               ))
             ),
    ),
    
    # How to use app page ----
    tabPanel("How to use the app", 
             div(class = "center-text",
                 h3("How to use the app Content"
                    )),
             
             div(class = "spacer left-align-center", # Container for paragraphs
                 p("Using the app is accessible and intuitive. There is a side-panel 
                   contains options for subsetting and curating the data into 
                   the specific confidences/scale/actions/districts that the user 
                   wishes to visualise."),
                 p("There is an 'Update Plots' button which is used to submit 
                   the selections to the plots. This is primarily done to reduce 
                   the number of 'updates' which the app must complete when 
                   selecting a number of options in quick succession."),
                 p("There is a number of selections for different plots also. 
                   These include boxplots for both Actions and Districts, as well 
                   as barplot of the 'counts' of datapoints contributing to the 
                   average values calculated for each action and district under the 
                   selected options. This is primarily used to understand how 
                   representative the range of values and the average value of 
                   each boxplot is of a given action under the selected options.")
             ),
          
             
             
             
             ),
    
    # Funding Shortfall App ----
    tabPanel("Funding Shortfall App",
             
             # App Options selector ----
             sidebarLayout(
               sidebarPanel(
                 
                 
                 # The goal is to have several different options. I want to make options for:
                 # - Selecting Which scale you want to look at, which selects what data file to use
                 # - Multiple choice selection for the management actions you want to look at
                 # - Mulitple choice selection for the Districts which you want to look at
                 # - Input value for thresholding the data to look at lower value plotting aspects
                 # - I also want to selection which will let you select which plots you want to have displayed
                 
                 
                 # Selection of which data you want to use:
                 
                 selectInput("Costing.Certainty", "Select costing certainty:",
                             choices = NULL,
                             selected = NULL,
                             multiple = TRUE),
                 div(class = "spacer centered",
                    actionButton("select_all_costing", "Select All Costing Certainty")
                    ),
                 
                 selectInput("Scale", "Select scale:",
                             choices = NULL,
                             selected = NULL,
                             multiple = TRUE),
                 div(class = "spacer centered",
                    actionButton("select_all_scale", "Select All Scales")
                    ),
                 
                 selectInput("Proposed.Management.Action", "Select Proposed Management Actions:",
                             choices = NULL,
                             selected = NULL,
                             multiple = TRUE),
                 div(class = "spacer centered",
                     actionButton("select_all_actions", "Select All Actions")
                 ),
                 
                 selectInput("District", "Select District:",
                             choices = NULL,
                             selected = NULL,
                             multiple = TRUE),
                 div(class = "spacer centered",
                     actionButton("select_all_districts", "Select All Districts")
                 ),
                 
                 # Button to Update plots on demand:
                 div(class = "spacer",
                     actionButton("update", "Update Plots")
                 ),
                 
                 div(class = "spacer centered",
                     checkboxGroupInput("selected_plots", "Select Plots to Display:",
                                        choices = list("Action Boxplot" = "Action_boxplot",
                                                       "District Boxplot" = "District_boxplot",
                                                       "Action Count Barplot" = "Action_barplot",
                                                       "District Count Barplot" = "District_barplot",
                                                       "Actions by Districts Boxplot" = "Action_by_District_boxplot"),
                                        selected = NA))
               ),
               
               mainPanel(
                 # For the outputs I have a vision of:
                 # - A boxplot showing the range of funding shortfall values for the different actions
                 # - A boxplot showing the range of funding shortfall values for the different districts
                 # - A barplot which shows the count of rows which contribute to the average data of actions
                 # - A Barplot which shows that count of rows which contribute to the average data of districts
                 
                 # The output of plots will depend on the selection of plots that the user wants to display at once
                 # which will enhance the intractability of the shiny app and allow a user
                 # to explore various plots simultaneously
                 
                 # Action Boxplot:
                 conditionalPanel(
                   condition = "input.selected_plots.includes('Action_boxplot')",
                   plotOutput("Action_boxplot")
                 ),
                 
                 # District Boxplot
                 conditionalPanel(
                   condition = "input.selected_plots.includes('District_boxplot')",
                   plotOutput("District_boxplot")
                 ),
                 
                 # Action Barplot
                 conditionalPanel(
                   condition = "input.selected_plots.includes('Action_barplot')",
                   plotOutput("Action_barplot")
                 ),
                 
                 # District Barplot
                 conditionalPanel(
                   condition = "input.selected_plots.includes('District_barplot')",
                   plotOutput("District_barplot")
                 ),
                 # Action~District Barplot
                 conditionalPanel(
                   condition = "input.selected_plots.includes('Action_by_District_boxplot')",
                   plotOutput("Action_by_District_boxplot")
                 ),
               )
             )
          ),

    
    
    # SFCC internship information page ----
    tabPanel("SFCC Internship", 
             div(class = "center-text",
                 h3("About this work")
                 ),
             
             div(class = "left-align-center",
                 p("This work was completed as part of a University of Aberdeen 
                   funded internship. The candidate (Hamish Williams) completed 
                   this work over the course of five weeks."),
                 p("Hamish - 'I have found this internship deeply fulfilling, 
                   and it has exposed me to new experiences, as well as a new 
                   qualification in electrofishing. The SFCC has provided me 
                   an amazing opporutnity to contribute to their work, and speak 
                   to people who are making the difference in Scottish Salmon 
                   Conservation across Scotland."),
             ),
          
             ),
  
  # Data Dictionary ----
   tabPanel("Data Dictionary",
            
            # Confidece ----
            div(class = "center-text",
            h3("Confidence")),
            
            div(class = "left-align-center",
                p("Confidence refers to the level of certaininty that a given 
                  esimate of a funding shortfall has. What derives a specific level 
                  of certainty in an action can depend on a multitude of factors")),
            
            fluidRow(
              column(12, div(class = "center-table",
              tableOutput("dataDictionary_Confidence")
              ))
            ),
            
            # Scale ----
            div(class = "center-text",
                h3("Scale")),
            
            div(class = "left-align-center",
                p("Scale refers to the geographical range at which a specific or
                   group of actions takes place across. Ranging from specific 
                  loations, all the way to the whole district.")
            ),
            
            fluidRow(
              column(12, div(class = "center-table",
                             tableOutput("dataDictionary_scale")
              ))
            ),
            
            # Actions ----
            div(class = "center-text",
                h3("Management Actions")),
            
            div(class = "left-align-center",
                p("Proposed management actions are identified action necessary to 
                  resolve and improve the management issue which has been raised. 
                  These vary from interacting with anglers, removing obstructions or
                   placing in wooden structures to rivers, all the way towards lobbying 
                  in government for policy and law changes.")
                ),
            
            fluidRow(
              column(12, div(class = "center-table",
                             tableOutput("dataDictionary_actions")
              ))
            ),

)
)
)
)
  
# Server Logic ----
server <- function(input, output, session) {
  
  output$dataDictionary_Confidence <- renderTable({
    data.frame(
      Option = c("Very Confident","Fairly Confident","Not Confident","Estimate","N/A"),
      Description = c("Cost of the action will be within 
                      £10,000 of the listed cost. Typically based on 
                      invoices from previous work, 
                      or quotes from contractors if not undertaken before. 
                      Costs may also be listed on providers websites ",
                      "Cost of the action will be within 
                      £20,000 of the listed cost. Based on quotes from 
                      contractors, or information on similar projects 
                      undertaken in other areas. There may be some elements of 
                      the action which are uncosted, but would represent a small 
                      proportion of the total cost. Costs may also be listed on 
                      providers websites.",
                      "Little evidence in terms of quotes or invoices 
                      from previous work to underpin estimate. The costs 
                      listed are the best estimate with available 
                      information. Costs may also be listed on providers websites",
                      "The listed cost is the best estimate with the
                      available information. There is no evidence to support the 
                      listed cost.",
                      "This category is used where it is not applicable to 
                      assign a costing certainty to the action.")
    )
  }, striped = F, hover = T
)
  
  output$dataDictionary_scale <- renderTable({
    data.frame(
      Scale = c("Targeted Location","Coastal","River","Catchment","Whole District"),
      Description = c("For actions which are in a specific location.",
                      "Coastal refers to actions pertaining to coastal zones.",
                      "River refers to actions taking place 
                         over the majority or the whole of a watercourse.",
                      "Refers to actions whic take place over an entire 
                      catchment within a district.",
                      "For actions which occur across the entirity of a district.")
    )
  }, striped = F, hover = T
)
  
  output$dataDictionary_actions <- renderTable({
    data.frame(
      Letter = c("A",
                 "B",
                 "C",
                 "D",
                 "E",
                 "F",
                 "G",
                 "H",
                 "I",
                 "J",
                 "K",
                 "L",
                 "M",
                 "N",
                 "O",
                 "P",
                 "Q",
                 "R",
                 "S",
                 "T"),
      Corresponding_Action = c("Angling Development",
                                 "Barrier Mitigation",
                                 "Campaigning",
                                 "Collaboration",
                                 "Enforcement",
                                 "Improving Instream Habitat",
                                 "Improving Riparian Habitat",
                                 "Land Management",
                                 "Lobbying",
                                 "Managing Predators",
                                 "Managing Species Interactions",
                                 "Monitoring",
                                 "Peatland Restoration",
                                 "Raising Awareness",
                                 "Controlling Invasive Species",
                                 "Research",
                                 "Riparian Tree Planting",
                                 "Stocking",
                                 "Water Flow Management",
                                 "Engaging with Consultations"),
      Description = c("Encouraging the uptake or continuance of Angling for those who are not regularly participating in the sport.",
                      "Mitigating the impacts of barriers to fish passage, in upstream, downstream or both directions. Measures such as installing or maintaining a fish pass, trap and transport, or complete removal can be included.",
                      "Organizing and/or educating the general public about the importance of a particular topic or issue, usually in order to create support for a particular change or outcome. ",
                      "Working in partnership with other organisations to achieve your objective.",
                      "Enforcing legislation in relation to salmon and sea trout. Typically this will involve bailiff patrols to prevent illegal exploitation by rod and line or net.",
                      "Any action which will improve fish habitat within a watercourse.",
                      "Any action in the Riparian zone, which is defined as an area adjacent to running freshwater.",
                      "Any action which will change or attempt to influence how land resources are used, developed or managed. This can also include directly assisting landowners with land management practices.",
                      "In a professional capacity, attempting to influence, or advising those who wish to influence, the UK Government, Parliament, the devolved legislatures or administrations, regional or local government or other public bodies on any matter within their competence (includes SEPA, NatureScot, FLS, Marine Scotland, etc).",
                      "Managing the impact that predators have on salmon and sea trout populations. Examples can include scaring piscivorous birds or deploying seal scarers. Where Mink is the predator in question, please use Control of Invasives as the Management Action.",
                      "This should be used primarily for actions relating to beavers, as the interaction between beavers and salmon or sea trout will have to be managed. More detail about the issue being caused and the strategy can be included in other columns. Where the interactions between salmon and other species have to be managed in a way that does not relate to predation, please use this column but in the description make it clear that the action is not related to beavers.",
                      "Undertaking scientific work to better understand an issue. Electrofishing falls into this category. Outputs of Monitoring work may be published in Annual Reports, websites, blog posts etc, but are NOT intended to be published in academic journals.",
                      "Management measures that aim to restore the original form and function of peatland habitats to favourable conservation status.",
                      "Working to highlight an issue to the wider public, or targeted groups. This may be with the ultimate aim of influencing opinion on a certain issue or topic. This differs from lobbying in that lobbying is targeting government, whereas raising awareness is more geared towards the goal of raising public understanding and support.",
                      "Undertaking work to remove invasive species or to mitigate their impact.",
                      "Undertaking scientific work which is intended to be published in an academic journal. This will likely be work undertaken in collaboration with a University.",
                      "Undertaking tree planting in the area adjacent to a watercourse. This action should be selected where riparian tree planting will be the main action that achieves the objective, there may be other aspects involved such as land management but if the outcome is to be riparian tree planting, then that’s what should be selected.",
                      "Selected for where stocking will be the main action. This can involve stocking of any life stage of salmon or sea trout.",
                      "This action is for instances where the flow of a river will be managed or changed. An example might be re-designing the flow regime in a river within a certain water budget, or it could be to reduce or stop abstraction from a river or watercourse.",
                      "District Salmon Fishery Boards are statutory consultees, and are consulted on for planning or energy developments. Fishery Trusts may also be engaged with consultations in submitting a response. Use this action to highlight where the action involves responding to consultations.")
    )
  })

  # Now add in the reactive elements for the different action/district choices:
  observe({
    data <- calculated_averages
    
    # Action Costing certainty:
    updateSelectInput(session, 
                      "Costing.Certainty", 
                      choices = unique(data$Costing.Certainty))
    # Action Choices:
    updateSelectInput(session, 
                      "Scale", 
                      choices = unique(data$Scale))
    
    # Action Choices:
    updateSelectInput(session, 
                      "Proposed.Management.Action", 
                      choices = unique(data$Proposed.Management.Action))
    # District Choices
    updateSelectInput(session, 
                      "District", 
                      choices = unique(data$District))
  })
  
  filtered_data_action_by_district <- eventReactive(input$update, {
    data <- calculated_averages
    data %>%
      filter(
        Costing.Certainty %in% input$Costing.Certainty,
        Scale %in% input$Scale,
        Proposed.Management.Action %in% input$Proposed.Management.Action,
        District %in% input$District
      )
  })
  
  # Render the Action Boxplot
  output$Action_boxplot <- renderPlot({
    
    data_action <- filtered_data_action_by_district()
    
    ggplot(data_action, aes(x = Proposed.Management.Action, y = Average_Funding_Shortfall)) +
      geom_boxplot(alpha = 0.3) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Boxplot by Proposed Management Action", 
        x = "Proposed.Management.Action", 
        y = "Funding Shortfall"
      )
  })
  
  output$District_boxplot <- renderPlot({
    
    data_district <- filtered_data_action_by_district()
    
    ggplot(data_district, aes(x = District, y = Average_Funding_Shortfall)) +
      geom_boxplot(alpha = 0.3) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Boxplot by District", 
        x = "District", 
        y = "Funding Shortfall"
      )
  })
  
  output$Action_barplot <- renderPlot({
    
    # Select data to use
    data_action <- filtered_data_action_by_district()

    # Create plotting function
    ggplot(data_action, aes(x = Proposed.Management.Action, y = Count)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 5, col = "red", linewidth = 1.5) +
      labs(title = "Count of Actions",
           x = "Management Action",
           y = "Count") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = FALSE)  # Hide the legend if not needed
  })
  
  output$District_barplot <- renderPlot({
    
    # Select data to use
    data_district <- filtered_data_action_by_district()
    
    # Create plotting function
    ggplot(data_district, aes(x = District, y = Count)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 5, col = "red", linewidth = 1.5) +
      labs(title = "Count of Actions in Districts",
           x = "District",
           y = "Count") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill = FALSE)  # Hide the legend if not needed
  })
  
  output$Action_by_District_boxplot <- renderPlot({
    
    data_action_by_district <- filtered_data_action_by_district()
    
    ggplot(data_action_by_district, aes(x = Proposed.Management.Action, 
                            y = Average_Funding_Shortfall,
                            fill= District,
                            col = District)) +
      geom_boxplot(alpha = 0.3) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(
        title = "Boxplot by Proposed Management Action", 
        x = "Proposed.Management.Action", 
        y = "Funding Shortfall"
      )
  })
  
  # Observe "Select All" button click events for each selectInput
  observeEvent(input$select_all_costing, {
    data <- calculated_averages
    updateSelectInput(session, "Costing.Certainty", selected = unique(data$Costing.Certainty))
  })
  
  observeEvent(input$select_all_scale, {
    data <- calculated_averages
    updateSelectInput(session, "Scale", selected = unique(data$Scale))
  })
  
  observeEvent(input$select_all_actions, {
    data <- calculated_averages
    updateSelectInput(session, "Proposed.Management.Action", selected = unique(data$Proposed.Management.Action))
  })
  
  observeEvent(input$select_all_districts, {
    data <- calculated_averages
    updateSelectInput(session, "District", selected = unique(data$District))
  })
  
  
# 
# End of server code
}

# Run the application 
shinyApp(ui = ui, server = server)
