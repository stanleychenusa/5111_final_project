#install.packages("shiny")
#install.packages(c("dplyr", "ggplot2", "DT", "readr", "lubridate", "usmap", "tidyr"))
#setwd("C:\\Users\\PhotonUser\\My Files\\OneDrive\\Files\\STSCI5111")


# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(lubridate)
library(tidyr)
library(usmap)
library(stringr)

# Load and clean data
delay_data <- read_csv("Airline_Delay_Cause.csv")

fare_passengers_data <- read_csv("AverageFare_Q4_2024.csv", skip = 1)
fare_passengers_data <- fare_passengers_data[1:8]

# Clean column names
colnames(delay_data) <- gsub("[^A-Za-z0-9_]", "_", colnames(delay_data))

# Make date column
delay_data$date <- as.Date(paste(delay_data$year, delay_data$month, "01", sep = "-"))

# Ensure date column is correctly formatted
if ("date" %in% colnames(delay_data)) {
  delay_data$date <- as.Date(delay_data$date)
} else {
  delay_data$date <- NA
}

# Create date column and extract state from airport name
delay_data_clean <- delay_data %>%
  mutate(
    date = make_date(year, month, 1),
    state = str_extract(airport_name, "([A-Z]{2})(?=:)")
  ) %>%
  filter(
    !is.na(date),
    !is.na(carrier_name),
    !is.na(airport_name)
  )

# pick the join‐key (e.g. Airport Code) plus two columns, and rename them:
fare_keyed <- fare_passengers_data %>%
  select(
    airport = `Airport Code`,
    avg_fare      = `Average Fare ($)`,
    passengers_10_pct    = `2024 Passengers (10% sample)`
  )

# left_join onto delays data
delay_data_clean <- delay_data_clean %>%
  left_join(fare_keyed, by = "airport")

# Delay cause breakdown
cause_columns <- c("carrier_delay", "weather_delay", "nas_delay", "security_delay", "late_aircraft_delay")

delay_causes <- delay_data_clean %>%
  summarise(across(all_of(cause_columns), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Cause", values_to = "Total")

# UI
ui <- fluidPage(
  titlePanel("Airline Delay Explorer Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Filter by Date:", start = min(delay_data_clean$date, na.rm = TRUE), 
                     end = max(delay_data_clean$date, na.rm = TRUE)),
      selectInput("airline", "Select Airline:", choices = unique(delay_data_clean$carrier_name)),
      selectInput("airport", "Select Airport:", choices = unique(delay_data_clean$airport_name))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Top Airlines", plotOutput("topAirlinesPlot")),
        tabPanel("Top Airports", plotOutput("topAirportsPlot")),
        tabPanel("Delay Causes", plotOutput("delayCausePlot")),
        tabPanel("Avg Delay Time", plotOutput("avgDelayPlot")),
        tabPanel("Delay Heatmap by State", plotOutput("stateHeatmap")),
        tabPanel("Fare vs. Delays", plotOutput("fareDelayPlot")),
        tabPanel("Passengers vs. Delays", plotOutput("passengerDelayPlot")),
        tabPanel("Filtered Table", DTOutput("filteredTable"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    delay_data_clean %>%
      filter((is.na(date) | (date >= input$date_range[1] & date <= input$date_range[2])))
  })
  
  output$topAirlinesPlot <- renderPlot({
    filtered_data() %>%
      group_by(carrier_name) %>%
      summarise(Total_Delays = sum(arr_del15, na.rm = TRUE)) %>%
      top_n(10, Total_Delays) %>%
      ggplot(aes(x = reorder(carrier_name, -Total_Delays), y = Total_Delays, fill = carrier_name)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Airlines by Total Delays", x = "Airline", y = "Total Delays") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$topAirportsPlot <- renderPlot({
    filtered_data() %>%
      group_by(airport) %>%
      summarise(Total_Delays = sum(arr_del15, na.rm = TRUE)) %>%
      top_n(10, Total_Delays) %>%
      ggplot(aes(x = reorder(airport, -Total_Delays), y = Total_Delays, fill = airport)) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Airports by Total Delays", x = "Airport", y = "Total Delays") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$delayCausePlot <- renderPlot({
    filtered_data() %>%
      summarise(Carrier = sum(carrier_ct, na.rm = TRUE),
                Weather = sum(weather_ct, na.rm = TRUE),
                NAS = sum(nas_ct, na.rm = TRUE),
                Security = sum(security_ct, na.rm = TRUE),
                Late_Aircraft = sum(late_aircraft_ct, na.rm = TRUE)) %>%
      pivot_longer(cols = everything(), names_to = "Cause", values_to = "Total") %>%
      ggplot(aes(x = reorder(Cause, -Total), y = Total, fill = Cause)) +
      geom_bar(stat = "identity") +
      labs(title = "Total Delay by Cause", x = "Cause", y = "Total Delay Count") +
      theme_minimal()
  })
  
  output$avgDelayPlot <- renderPlot({
    filtered_data() %>%
      group_by(carrier_name) %>%
      summarise(Average_Delay = mean(arr_del15, na.rm = TRUE)) %>%
      top_n(10, Average_Delay) %>%
      ggplot(aes(x = reorder(carrier_name, -Average_Delay), y = Average_Delay, fill = carrier_name)) +
      geom_col() +
      labs(title = "Top Airlines by Avg Delay Time", x = "Airline", y = "Average Delay Time") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$stateHeatmap <- renderPlot({
    state_data <- filtered_data() %>%
      group_by(state) %>%
      summarise(total_delays = sum(arr_del15, na.rm = TRUE)) %>%
      filter(!is.na(state))
    
    plot_usmap(data = state_data, values = "total_delays", regions = "states") +
      scale_fill_continuous(low = "white", high = "red", name = "Total Delays") +
      labs(title = "Delay Heatmap by State") +
      theme(legend.position = "right")
  })
  
  ### Average fare vs. total delays scatter + trend ###
  output$fareDelayPlot <- renderPlot({
    # for each airport, grab its single AvgFare and total delays
    df_fare <- filtered_data() %>%
      group_by(airport_name, avg_fare) %>%
      summarise(
        Total_Delays = sum(arr_del15, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(df_fare, aes(x = avg_fare, y = Total_Delays)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Airport Average Fare vs. Total Delays",
        x     = "Average Fare ($)",
        y     = "Total Delays"
      ) +
      theme_minimal()
  })
  
  ### Passenger volume vs. total delays scatter + trend ###
  output$passengerDelayPlot <- renderPlot({
    # for each airport, grab its Pass10pct and total delays
    df_pass <- filtered_data() %>%
      group_by(airport_name, passengers_10_pct) %>%
      summarise(
        Total_Delays = sum(arr_del15, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(df_pass, aes(x = passengers_10_pct, y = Total_Delays)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Airport Passenger Volume vs. Total Delays",
        x     = "2024 Passengers (10% sample)",
        y     = "Total Delays"
      ) +
      theme_minimal()
  })
  
  output$filteredTable <- renderDT({
    filtered_data() %>%
      filter(carrier_name == input$airline | airport_name == input$airport) %>%
      datatable(options = list(scrollX = TRUE))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

