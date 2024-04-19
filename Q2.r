# Author: Victor Pan
# Date: April 19, 2024
# Before you run this file, make sure to modify read.csv to include your path to the athlete_events.csv

library(shiny)
library(ggplot2)
library(dplyr)


olympic_data <- read.csv("C:/Users/victo/DS2003/mywork/athlete_events.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Olympic Athletes Visualization"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("weight", "Weight", min = min(olympic_data$Weight, na.rm = TRUE), 
                  max = max(olympic_data$Weight, na.rm = TRUE), value = c(min(olympic_data$Weight, na.rm = TRUE), max(olympic_data$Weight, na.rm = TRUE))),
      sliderInput("height", "Height", min = min(olympic_data$Height, na.rm = TRUE), 
                  max = max(olympic_data$Height, na.rm = TRUE), value = c(min(olympic_data$Height, na.rm = TRUE), max(olympic_data$Height, na.rm = TRUE))),
      checkboxGroupInput("medal", "Medal", choices = c("Gold" = "Gold", "Silver" = "Silver", "Bronze" = "Bronze", "None" = "NA")),
      checkboxGroupInput("sport", "Sport", choices = unique(olympic_data$Sport))
    ),
    mainPanel(
      plotOutput("medalPlot")
    )
  )
)

server <- function(input, output) {
  output$medalPlot <- renderPlot({
    
    filtered_data <- olympic_data %>%
      filter(Weight >= input$weight[1], Weight <= input$weight[2],
             Height >= input$height[1], Height <= input$height[2],
             Sport %in% input$sport) %>%
      filter(if("NA" %in% input$medal) is.na(Medal) | Medal %in% input$medal else Medal %in% input$medal)
    
    ggplot(filtered_data, aes(x = Weight, y = Height)) +
      geom_point(aes(color = Medal)) +
      scale_color_manual(values = c("Gold" = "gold", "Silver" = "gray", "Bronze" = "brown", "NA" = "black")) +
      theme_minimal() +
      labs(title = "Olympic Athletes by Weight, Height, and Medal Won",
           x = "Weight (kg)", y = "Height (cm)", color = "Medal")
  })
}

shinyApp(ui = ui, server = server)