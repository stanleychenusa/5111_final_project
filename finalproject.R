install.packages("shiny")
install.packages(c("dplyr", "ggplot2", "DT", "readr", "lubridate", "usmap", "tidyr"))
setwd("C:\\Users\\PhotonUser\\My Files\\OneDrive\\Files\\STSCI5111")


# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(lubridate)
library(tidyr)
library(usmap)

# Load and clean data
delay_data <- read_csv("Airline_Delay_Cause.csv")

# Clean column names
colnames(delay_data) <- gsub("[^A-Za-z0-9_]", "_", colnames(delay_data))

# Ensure date column is correctly formatted
if ("Date" %in% colnames(delay_data)) {
  delay_data$Date <- as.Date(delay_data$Date)
} else {
  delay_data$Date <- NA
}

# Remove rows with missing key information
delay_data_clean <- delay_data %>%
  filter(!is.na(Airline), !is.na(Airport), !is.na(Total_Delays))

# Convert categorical columns
delay_data_clean$Airline <- as.factor(delay_data_clean$Airline)
delay_data_clean$Airport <- as.factor(delay_data_clean$Airport)

# Delay cause breakdown
cause_columns <- c("Carrier_Delay", "Weather_Delay", "NAS_Delay", "Security_Delay", "Late_Aircraft_Delay")

delay_causes <- delay_data_clean %>%
  summarise(across(all_of(cause_columns), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Cause", values_to = "Total")

# UI
ui <- fluidPage(
  titlePanel("Airline Delay Explorer Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Filter by Date:", start = min(delay_data_clean$Date, na.rm = TRUE), 
                     end = max(delay_data_clean$Date, na.rm = TRUE)),
      selectInput("airline", "Select Airline:", choices = unique(delay_data_clean$Airline)),
      selectInput("airport", "Select Airport:", choices = unique(delay_data_clean$Airport))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Top Airlines", plotOutput("topAirlinesPlot")),
        tabPanel("Top Airports", plotOutput("topAirportsPlot")),
        tabPanel("Delay Causes", plotOutput("delayCausePlot")),
        tabPanel("Avg Delay per Flight", plotOutput("avgDelayPlot")),
        tabPanel("Delay Heatmap by State", plotOutput("stateHeatmap")),
        tabPanel("Filtered Table", DTOutput("filteredTable"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    delay_data_clean %>%
      filter((is.na(Date) | (Date >= input$date_range[1] & Date <= input$date_range[2])))
  })
  
  output$topAirlinesPlot <- renderPlot({
    filtered_data() %>%
      group_by(Airline) %>%
      summarise(Total_Delays = sum(Total_Delays, na.rm = TRUE)) %>%
      top_n(10, Total_Delays) %>%
      ggplot(aes(x = reorder(Airline, -Total_Delays), y = Total_Delays, fill = Airline)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Airlines by Total Delays", x = "Airline", y = "Total Delays") +
      theme_minimal()
  })
  
  output$topAirportsPlot <- renderPlot({
    filtered_data() %>%
      group_by(Airport) %>%
      summarise(Total_Delays = sum(Total_Delays, na.rm = TRUE)) %>%
      top_n(10, Total_Delays) %>%
      ggplot(aes(x = reorder(Airport, -Total_Delays), y = Total_Delays, fill = Airport)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Airports by Total Delays", x = "Airport", y = "Total Delays") +
      theme_minimal()
  })
  
  output$delayCausePlot <- renderPlot({
    ggplot(delay_causes, aes(x = reorder(Cause, -Total), y = Total, fill = Cause)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Delay by Cause", x = "Cause", y = "Total Delays") +
      theme_minimal()
  })
  
  output$avgDelayPlot <- renderPlot({
    filtered_data() %>%
      group_by(Airline) %>%
      summarise(Average_Delay = mean(Total_Delays, na.rm = TRUE)) %>%
      top_n(10, Average_Delay) %>%
      ggplot(aes(x = reorder(Airline, -Average_Delay), y = Average_Delay, fill = Airline)) +
      geom_col() +
      labs(title = "Top Airlines by Avg Delay per Flight", x = "Airline", y = "Average Delay") +
      theme_minimal()
  })
  
  output$stateHeatmap <- renderPlot({
    if ("State" %in% colnames(delay_data_clean)) {
      state_summary <- filtered_data() %>%
        group_by(State) %>%
        summarise(Total_Delays = sum(Total_Delays, na.rm = TRUE))
      
      plot_usmap(data = state_summary, values = "Total_Delays", color = "white") +
        scale_fill_continuous(name = "Total Delays", label = scales::comma) +
        labs(title = "Heatmap of Delays by State") +
        theme(legend.position = "right")
    } else {
      ggplot() + 
        annotate("text", x = 1, y = 1, label = "No 'State' column available in dataset.", size = 6) +
        theme_void()
    }
  })
  
  output$filteredTable <- renderDT({
    filtered_data() %>%
      filter(Airline == input$airline | Airport == input$airport) %>%
      datatable(options = list(scrollX = TRUE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

