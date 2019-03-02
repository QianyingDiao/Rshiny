library(shiny)
library(leaflet)
library(shinythemes)

# Define UI for application that find information of parks in New York state.
shinyUI(fluidPage(
  
  # Change the theme to darkly
  theme = shinytheme("darkly"),
  
  # Application title
  titlePanel("State Park Facility Points in New York"),
  br(),
  helpText("Qianying Diao"),
  # Sidebar for uploading the file
  sidebarLayout(
    sidebarPanel(
      
      # Create a file input
      fileInput("file","Choose a CSV File Please",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Create a multiple checkbox input for categories.
      checkboxGroupInput("Category",
                         "Classfication of State Park Facility:",
                         c("Marine Facility","State Historic Park","State Historic Site","State Park",
                           "State Park Preserve","Other")
      ),
      
      # Create a multiple checkbox input for regions.
        checkboxGroupInput("Region",
                         "State Region:",
                         choices = list("Region 1"= 1,"Region 2"= 2,"Region 3"= 3,"Region 4"= 4,
                                        "Region 5"= 5,"Region 6"= 6,"Region 7"= 7,"Region 8"= 8,"Region 9"= 9,"Region 10"= 10,
                                        "Region 11"= 11,"Region 12"= 12)
      ),
      
      hr(),
      helpText("Please Select The Cateories and Regions to Find More Information"),
      helpText("You Can Choose More Than One")
    ),
    
    
    # Make the sidebar on the left of the webpage
    position = "left",
    fluid = TRUE,
    
    # Create three tabs
    mainPanel(
      
      tabsetPanel(type="tabs",
                  
      # Add a tab for problem description
      
      tabPanel("Problem Description", textOutput("text")),
      
      
      # Add a tab for map
      tabPanel("Map", leafletOutput("map", height=630)),
      
      
      # Add a tab for Add a tab for decriptive table
      tabPanel("Descriptive Analysis",
               
               # Add two subtabs
               tabsetPanel(type="tabs",
                 tabPanel("Golf", 
                          fluidRow(
                          helpText("View availability of GOLF in selected regions:"),
                          verbatimTextOutput("table1.1")),
                          fluidRow(
                          plotOutput("plot1",height="300px")),
                          hr(),
                          helpText("Find the location:"),
                          verbatimTextOutput("table1")),
                          
                          
                 tabPanel("Camp",
                          fluidRow(
                          helpText("View availability of CAMP in selected regions:"),
                          verbatimTextOutput("table2.1")),
                          fluidRow(
                          plotOutput("plot2",height="300px")),
                          hr(),
                          helpText("Find the location:"),
                          verbatimTextOutput("table2")),
                 
                 tabPanel("Playground",
                          fluidRow(
                          helpText("View availability of PLAYGROUND in selected regions:"),
                          verbatimTextOutput("table3.1")),
                          fluidRow(
                          plotOutput("plot3",height="300px")),
                          hr(),
                          helpText("Find the location:"),
                          verbatimTextOutput("table3")),
                 
                 tabPanel("Nature Center",
                          fluidRow(
                          helpText("View availability of NATURE CENTER in selected regions:"),
                          verbatimTextOutput("table4.1")),
                          fluidRow(
                          plotOutput("plot4",height="300px")),
                          hr(),
                          helpText("Find the location:"),
                          verbatimTextOutput("table4")),
                 
                 tabPanel("Beach",
                          fluidRow(
                          helpText("View availability of BEACH in selected regions:"),
                          verbatimTextOutput("table5.1")),
                          fluidRow(
                          plotOutput("plot5",height="300px")),
                          hr(),
                          helpText("Find the location:"),
                          verbatimTextOutput("table5"))
               )
      )
    )
  )
)
))
