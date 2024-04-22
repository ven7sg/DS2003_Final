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
library(dplyr)
library(readr)
library(plotly)

# Load data globally
data <- read_csv("/Users/kaitlinblakeslee/Desktop/datacomm/athlete_events.csv")
data <- data %>%
  filter(Sport != "Art Competitions") %>%
  mutate(Year = ifelse(Year %in% c(1994, 1998, 2002, 2006, 2010, 2014), Year + 2, Year))

# Define the user interface
ui <- fluidPage(
  titlePanel("How has gender participation in the Olympics changed over time?
"),
  sidebarLayout(
    sidebarPanel(
      helpText("Explore changes in the participation of male and female athletes in the Olympics over time."),
      
      # Input: Select a range of years
      sliderInput("yearRange",
                  "Select Year Range:",
                  min = min(data$Year), max = max(data$Year),
                  value = c(min(data$Year), max(data$Year)),
                  step = 4,
                  sep = ""),
      
      # Input: Select a country
      selectInput("country",
                  "Select Country:",
                  choices = c("All", unique(data$Team))),
      
      # Input: Select a sport
      selectInput("sport",
                  "Select Sport:",
                  choices = c("All", unique(data$Sport))),
      
      actionButton("submit", "Update View")
    ),
    mainPanel(
      conditionalPanel(
        condition = "output.availableData === true",
        plotlyOutput("genderPlot"),
        textOutput("medalCount"),
        textOutput("participantCount")
      ),
      conditionalPanel(
        condition = "output.availableData === false",
        tags$h3("No participants or no data available for the selected filters.", style = "color: red;")
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on inputs
  filteredData <- reactive({
    input$submit
    isolate({
      temp_data <- data %>%
        filter(Year >= input$yearRange[1], Year <= input$yearRange[2],
               if (input$country != "All") Team == input$country else TRUE,
               if (input$sport != "All") Sport == input$sport else TRUE)
      
      # Count athletes by Year and Sex
      temp_data %>%
        group_by(Year, Sex) %>%
        summarize(Athletes = n_distinct(ID), .groups = 'drop')
    })
  })
  
  # Plot the data with interactive features
  output$genderPlot <- renderPlotly({
    plot_data <- req(filteredData())
    if (nrow(plot_data) == 0) {
      return(NULL)
    }
    
    p <- ggplot(plot_data, aes(x = Year, y = Athletes, group = Sex, color = Sex, text = paste("Athletes: ", Athletes))) +
      geom_point(size = 4) +
      geom_line() +
      scale_color_manual(values = c("pink", "lightblue")) +
      labs(title = "Number of Male and Female Olympians Over Time",
           x = "Year",
           y = "Number of Athletes",
           color = "Sex") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Calculate and display medal counts
  output$medalCount <- renderText({
    req(filteredData())
    medal_data <- data %>%
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2],
             if (input$country != "All") Team == input$country else TRUE,
             if (input$sport != "All") Sport == input$sport else TRUE,
             !is.na(Medal))
    count_men <- nrow(medal_data[medal_data$Sex == "M",])
    count_women <- nrow(medal_data[medal_data$Sex == "F",])
    paste("Number of Male Medalists:", count_men, "| Number of Female Medalists:", count_women)
  })
  
  # Calculate and display total participants
  output$participantCount <- renderText({
    req(filteredData())
    participant_data <- filteredData()  # Already filtered reactively
    count_men <- sum(participant_data$Athletes[participant_data$Sex == "M"])
    count_women <- sum(participant_data$Athletes[participant_data$Sex == "F"])
    paste("Total Male Participants:", count_men, "| Total Female Participants:", count_women)
  })
  
  # Determine if data is available
  output$availableData <- reactive({
    plot_data <- filteredData()
    nrow(plot_data) > 0
  })
  outputOptions(output, "availableData", suspendWhenHidden = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
