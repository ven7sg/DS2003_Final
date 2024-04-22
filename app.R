#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(maps)
library(jsonlite)
library(dplyr)
library(sf)
library(data.table)

ath_data <- read.csv("athlete_events.csv")
con_data <- read.csv("noc_regions.csv")



ui <- fluidPage(
  
  # Application title
  titlePanel("World Map with ggplot2"),
  
  ###input sport####
  selectInput("sport", "Pick a sport!",choices = unique(ath_data$Sport), selected = "Tug-Of-War"),
  
  ###input time####
  sliderInput("year", "Select a year for the games", min = 1896, max = 2016, value = 1896, step = 2),
  
  ###input country###
  selectInput("country", "Pick a Country!", choices = unique(ath_data$Team),selected = "Denmark/Sweden"),
  
  
  mainPanel(
    h4("Medals Won:"),
    textOutput("medal_count"),
    textOutput("total_medal_count")
  ),
  
  ####Output: Plot
  plotOutput("world_map"),
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$country, input$year, input$sport)
    filter(ath_data, Team == input$country & Year == input$year & Sport == input$sport)
  })
  
  total_medals <- reactive({
    req(input$country, input$year, input$sport)
    filter(ath_data, Team == input$country & Year <= input$year & Sport == input$sport)
  })
  
  output$medal_count <- renderText({
    filtered <- filtered_data()
    
    # Initialize medal counts
    medal_counts <- c(Bronze = 0, Silver = 0, Gold = 0)
    
    # Loop through each medal type and count the number of medals
    for (medal_type in c("Bronze", "Silver", "Gold")) {
      medal_counts[medal_type] <- sum(filtered$Medal == medal_type)
    }
    
    
    # Handle case where no medals were earned
    text <- paste(
      "Medals for", input$country, "in", input$year, input$sport, ":\n",
      "Bronze:", medal_counts["Bronze"], "\n",
      "Silver:", medal_counts["Silver"], "\n",
      "Gold:", medal_counts["Gold"], "\n"
    )
    return(text)
  })
  
  output$total_medal_count <- renderText({
    total <- total_medals()
    
    total_medal_counts <- c(Bronze = 0, Silver = 0, Gold = 0)
    
    for (medal_type in c("Bronze", "Silver", "Gold")) {
      total_medal_counts[medal_type] <- sum(total$Medal == medal_type)
    }
    
    text <- paste(
      "Total Medals for", input$country, "in", input$year, input$sport, ":\n",
      "Bronze:", total_medal_counts["Bronze"], "\n",
      "Silver:", total_medal_counts["Silver"], "\n",
      "Gold:", total_medal_counts["Gold"], "\n"
    )
    return(text)
    
  })
  
  
  output$world_map <- renderPlot({
    
    req(input$country)
    #create_world_map(input$country)
    
    world <- map_data("world")
    world <- as.data.frame(world) %>%
      filter(region != "Antarctica")
    
    
    selected_name <- filter(ath_data, Team == input$country)
    noc <- selected_name$NOC[1]
    country_name <- con_data$region[con_data$NOC == noc]
    country_name
    
    selected <-map_data("world")
    selected <- as.data.frame(selected) %>%
      filter(region == country_name)
    
    
    
    # Plot world map
    ggplot() +
      geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
      geom_polygon(data = selected, aes(x = long, y = lat, group = group), color = 'blue', fill = 'blue')+
      coord_fixed() +
      theme_void() 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
