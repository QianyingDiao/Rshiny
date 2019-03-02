# Rshiny
Rshiny application

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




options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)



# Define server that find detail information about parks in different regions in New York.
shinyServer(function(input, output) {
  
  # Create an output variable for problem description
  output$text <- renderText({
  
    "This project uses the dataset 'State Park Facility Points'. It is provided by The New York State Office of Parks, Recreation and Historic Preservation (OPRHP) in 2018. These facilities contribute to the economic vitality and quality of life of local communities and directly support New York's tourism industry. We can visualize the distribution of different types of parks. For different regions in New York state, we can get information about the facility if it has golf available, camp available, playground available, nature center available and beach available, which is very helpful for tourists to make a diserable choice."
    
  })
  
  
  
  # Create a map output variable
  output$map <- renderLeaflet({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    
    if(is.null(inFile))
      return(NULL)
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    # Filter the data for different categories and different regions
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    map_df <- filter(mydata, Category %in% target1 & Region %in% target2)
    
    # Create colors with a categorical color function
    color <- colorFactor(rainbow(6), map_df$Category)
  
   
     # Create the leaflet function
    leaflet(map_df) %>%
      
      # Set the default view
      setView(lng = -76.0094, lat = 42.7658, zoom = 6) %>%
      
      # Provide tiles
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
   
     # Add circles
    addCircleMarkers(
      radius = 2,
      lng= map_df$Longitude,
      lat= map_df$Latitude,
      stroke= FALSE,
      fillOpacity=0.1,
      color= color(Category)
    ) %>%
      
      # Add legends for different types of facilities
      addLegend(
        "bottomright",
        pal=color,
        values= Category,
        opacity=0.5,
        title="Classfication of State Park Facility"
      )
  })
  
  
   
  
  
  # Create a descriptive table for golf
  output$table1 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
   
     # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
  
    
    # Filter the data for different categories and different regions
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    golf_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Golf" & Availability %in% "Available")
    select(golf_df,Name,Category,Region,County)
    
    })
  
   # Table the data
  output$table1.1 <- renderPrint({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
  
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    golf_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Golf")
    table(golf_df$Availability,golf_df$Region)
   
    
  })
  
  ###### ggplot
  
  output$plot1 <- renderPlot({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    golf_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Golf")
    
    ggplot(golf_df, aes(Availability,fill=Availability)) + geom_bar(aes(fill=factor(Region)))+scale_fill_discrete(name="Region")
  
  })
  
  
  
  # Create a descriptive table for camp
  output$table2 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    
    # Filter the data for different categories and different regions
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    camp_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Camp" & Availability %in% "Available")
    select(camp_df,Name,Category,Region,County)
})
  
  # Table the data
  output$table2.1 <- renderPrint({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    camp_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Camp")
    table(camp_df$Availability,camp_df$Region)
    
  })
  
  ###### ggplot
  
  output$plot2 <- renderPlot({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    camp_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Camp")
    
    ggplot(camp_df, aes(Availability,fill=Availability)) + geom_bar(aes(fill=factor(Region)))+scale_fill_discrete(name="Region")
    
  })
  
  
  
  
  # Create a descriptive table for playground
  output$table3 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    
    # Filter the data for different categories and different regions
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    playground_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Playground" & Availability %in% "Available")
    select(playground_df,Name,Category,Region,County)
  })
  
  # Table the data
  output$table3.1 <- renderPrint({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    playground_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Playground")
    table(playground_df$Availability,playground_df$Region)
    
  })
  
  ###### ggplot
  
  output$plot3 <- renderPlot({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    playground_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Playground")
    
    ggplot(playground_df, aes(Availability,fill=Availability)) + geom_bar(aes(fill=factor(Region)))+scale_fill_discrete(name="Region")
    
  })
  
  
  
  # Create a descriptive table for nature center
  output$table4 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    
    # Filter the data for different categories and different regions
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    nature_center_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Nature_Center" & Availability %in% "Available")
    select(nature_center_df,Name,Category,Region,County)
  })
  
  # Table the data
  output$table4.1 <- renderPrint({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    nature_center_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Nature_Center")
    table(nature_center_df$Availability,nature_center_df$Region)
    
  })
  
  ###### ggplot
  
  output$plot4 <- renderPlot({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    nature_center_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Nature_Center")
    
    ggplot(nature_center_df, aes(Availability,fill=Availability)) + geom_bar(aes(fill=factor(Region)))+scale_fill_discrete(name="Region")
    
  })
  
  
  
  # Create a descriptive table for Beach
  output$table5 <- renderPrint({
    
    # Connect to the sidebar of file input
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    
    # Read input file
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    
    # Filter the data for different categories and different regions
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    beach_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Beach" & Availability %in% "Available")
    select(beach_df,Name,Category,Region,County)
  })
  
  # Table the data
  output$table5.1 <- renderPrint({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    beach_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Beach")
    table(beach_df$Availability,beach_df$Region)
    
  })
  
  
  ###### ggplot
  
  output$plot5 <- renderPlot({
    inFile <- input$file
    if(is.null(inFile))
      return("Please Upload A File For Search")
    mydata <- read.csv(inFile$datapath)
    attach(mydata)
    
    target1 <- c(input$Category)
    target2 <- c(input$Region)
    beach_df <- filter(mydata, Category %in% target1 & Region %in% target2 & Facility %in% "Beach")
    
    ggplot(beach_df, aes(Availability,fill=Availability)) + geom_bar(aes(fill=factor(Region)))+scale_fill_discrete(name="Region")
    
  })
  
})



