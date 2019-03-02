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


