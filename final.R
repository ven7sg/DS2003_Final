library(shiny)
library(ggplot2)
library(maps)
library(jsonlite)
library(dplyr)
library(sf)
library(data.table)
library(readr)
library(plotly)
library(shinythemes)


# LOAD DATA 
ath_data <- read.csv("~/Desktop/DS2003_Shiny/athlete_events.csv")
con_data <- read.csv("~/Desktop/DS2003_Shiny/noc_regions.csv")

ath_data <- ath_data %>%
  filter(Sport != "Art Competitions") %>%
  mutate(Year = ifelse(Year %in% c(1994, 1998, 2002, 2006, 2010, 2014), Year + 2, Year))

ui <- fluidPage(
  theme = shinytheme("superhero"),  # Applying Bootstrap's Superhero theme
  titlePanel("Olympic Data Analysis"),
  
  # Create a tabbed layout
  tabsetPanel(
    tabPanel("Introduction",
             fluidPage(
               h1("Introduction"),
               p("text."),
               # img(src = "intro.jpg", height = "200px"),  
               tags$ul(
                 tags$li("Bullet point."),
                 tags$li("Bullet point."),
                 tags$li("Bullet point.")
               )
             )),
    tabPanel("Question 1 - World Map",
             fluidPage(
               titlePanel("World Map with ggplot2"),
               selectInput("sport", "Pick a sport!", choices = unique(ath_data$Sport), selected = "Tug-Of-War"),
               sliderInput("year", "Select a year for the games", min = 1896, max = 2016, value = 1896, step = 2),
               selectInput("country", "Pick a Country!", choices = unique(ath_data$Team), selected = "Denmark/Sweden"),
               h4("Medals Won:"),
               textOutput("medal_count"),
               textOutput("total_medal_count"),
               plotOutput("world_map")
             )),
    tabPanel("Question 2 - Athlete Performance",
             fluidPage(
               titlePanel("Olympic Athletes Visualization"),
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("weight", "Weight", min = min(ath_data$Weight, na.rm = TRUE), 
                               max = max(ath_data$Weight, na.rm = TRUE), value = c(min(ath_data$Weight, na.rm = TRUE), max(ath_data$Weight, na.rm = TRUE))),
                   sliderInput("height", "Height", min = min(ath_data$Height, na.rm = TRUE), 
                               max = max(ath_data$Height, na.rm = TRUE), value = c(min(ath_data$Height, na.rm = TRUE), max(ath_data$Height, na.rm = TRUE))),
                   checkboxGroupInput("medal", "Medal", choices = c("Gold" = "Gold", "Silver" = "Silver", "Bronze" = "Bronze", "None" = "NA")),
                   checkboxGroupInput("sport2", "Sport", choices = unique(ath_data$Sport))
                 ),
                 mainPanel(
                   plotOutput("medalPlot")
                 )
               )
             )),
    tabPanel("Question 3 - Gender Participation",
             fluidPage(
               titlePanel("How has gender participation in the Olympics changed over time?"),
               sidebarLayout(
                 sidebarPanel(
                   helpText("Explore changes in the participation of male and female athletes in the Olympics over time."),
                   sliderInput("yearRange", "Select Year Range:", min = min(ath_data$Year), max = max(ath_data$Year),
                               value = c(min(ath_data$Year), max(ath_data$Year)), step = 4, sep = ""),
                   selectInput("country3", "Select Country:", choices = c("All", unique(ath_data$Team))),
                   selectInput("sport3", "Select Sport:", choices = c("All", unique(ath_data$Sport))),
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
             )),
    tabPanel("Conclusion",
             fluidPage(
               h1("Key Takeaways"),
               p("Our analysis brings forward the significant trends and insights from Olympic history."),
               # img(src = "conclusion.jpg", height = "200px"),  
               tags$ul(
                 tags$li("Bullet point."),
                 tags$li("Bullet point."),
                 tags$li("Bullet point.")
               )
             )
    )
  )
)

server <- function(input, output) {
  # Question 1 Server Logic
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
    
    medal_counts <- c(Bronze = 0, Silver = 0, Gold = 0)
    
    for (medal_type in c("Bronze", "Silver", "Gold")) {
      medal_counts[medal_type] <- sum(filtered$Medal == medal_type)
    }
    
    paste(
      "Medals for", input$country, "in", input$year, input$sport, ":\n",
      "Bronze:", medal_counts["Bronze"], "\n",
      "Silver:", medal_counts["Silver"], "\n",
      "Gold:", medal_counts["Gold"], "\n"
    )
  })
  
  output$total_medal_count <- renderText({
    total <- total_medals()
    
    total_medal_counts <- c(Bronze = 0, Silver = 0, Gold = 0)
    
    for (medal_type in c("Bronze", "Silver", "Gold")) {
      total_medal_counts[medal_type] <- sum(total$Medal == medal_type)
    }
    
    paste(
      "Total Medals for", input$country, "in", input$year, input$sport, ":\n",
      "Bronze:", total_medal_counts["Bronze"], "\n",
      "Silver:", total_medal_counts["Silver"], "\n",
      "Gold:", total_medal_counts["Gold"], "\n"
    )
  })
  
  output$world_map <- renderPlot({
    req(input$country)
    world <- map_data("world")
    world <- as.data.frame(world) %>%
      filter(region != "Antarctica")
    
    selected_name <- filter(ath_data, Team == input$country)
    noc <- selected_name$NOC[1]
    country_name <- con_data$region[con_data$NOC == noc]
    country_name
    
    selected <- map_data("world")
    selected <- as.data.frame(selected) %>%
      filter(region == country_name)
    
    ggplot() +
      geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", color = "black") +
      geom_polygon(data = selected, aes(x = long, y = lat, group = group), color = 'blue', fill = 'blue')+
      coord_fixed() +
      theme_void()
  })
  
  # Question 2 Server Logic
  output$medalPlot <- renderPlot({
    filtered_data <- ath_data %>%
      filter(Weight >= input$weight[1], Weight <= input$weight[2],
             Height >= input$height[1], Height <= input$height[2],
             Sport %in% input$sport2) %>%
      filter(if("NA" %in% input$medal) is.na(Medal) | Medal %in% input$medal else Medal %in% input$medal)
    
    ggplot(filtered_data, aes(x = Weight, y = Height)) +
      geom_point(aes(color = Medal)) +
      scale_color_manual(values = c("Gold" = "gold", "Silver" = "gray", "Bronze" = "brown", "NA" = "black")) +
      theme_minimal() +
      labs(title = "Olympic Athletes by Weight, Height, and Medal Won",
           x = "Weight (kg)", y = "Height (cm)", color = "Medal")
  })
  
  # Question 3 Server Logic
  filteredData <- reactive({
    input$submit
    isolate({
      temp_data <- ath_data %>%
        filter(Year >= input$yearRange[1], Year <= input$yearRange[2],
               if (input$country3 != "All") Team == input$country3 else TRUE,
               if (input$sport3 != "All") Sport == input$sport3 else TRUE)
      
      # Count athletes by Year and Sex
      temp_data %>%
        group_by(Year, Sex) %>%
        summarize(Athletes = n_distinct(ID), .groups = 'drop')
    })
  })
  
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
  
  output$medalCount <- renderText({
    req(filteredData())
    medal_data <- ath_data %>%
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2],
             if (input$country3 != "All") Team == input$country3 else TRUE,
             if (input$sport3 != "All") Sport == input$sport3 else TRUE,
             !is.na(Medal))
    count_men <- nrow(medal_data[medal_data$Sex == "M",])
    count_women <- nrow(medal_data[medal_data$Sex == "F",])
    paste("Number of Male Medalists:", count_men, "| Number of Female Medalists:", count_women)
  })
  
  output$participantCount <- renderText({
    req(filteredData())
    participant_data <- filteredData()  # Already filtered reactively
    count_men <- sum(participant_data$Athletes[participant_data$Sex == "M"])
    count_women <- sum(participant_data$Athletes[participant_data$Sex == "F"])
    paste("Total Male Participants:", count_men, "| Total Female Participants:", count_women)
  })
  
  output$availableData <- reactive({
    plot_data <- filteredData()
    nrow(plot_data) > 0
  })
  outputOptions(output, "availableData", suspendWhenHidden = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)