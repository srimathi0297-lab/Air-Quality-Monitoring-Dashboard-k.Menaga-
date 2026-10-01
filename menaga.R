install.packages(c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "dplyr",
  "plotly",
  "tidyr"
))


library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)

# Load dataset
air <- read.csv

# Convert Date
air$Date <- as.Date(air$Date)

# ---------------- UI ----------------

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Air Quality Dashboard"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Analysis",
        tabName = "analysis",
        icon = icon("chart-line")
      )
      
    ),
    
    selectInput(
      "city",
      "Select City",
      choices = c(
        "All",
        sort(unique(air$City))
      ),
      selected = "All"
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      # ================= DASHBOARD =================
      
      tabItem(
        
        tabName = "dashboard",
        
        fluidRow(
          
          valueBoxOutput("avgAQI"),
          valueBoxOutput("maxAQI"),
          valueBoxOutput("minAQI")
          
        ),
        
        fluidRow(
          
          box(
            title = "Average AQI by City",
            width = 6,
            plotlyOutput("cityAQI")
          ),
          
          box(
            title = "AQI Distribution",
            width = 6,
            plotlyOutput("aqiDistribution")
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Average Pollutant Levels",
            width = 12,
            plotlyOutput("pollutants")
          )
          
        )
        
      ),
      
      # ================= ANALYSIS =================
      
      tabItem(
        
        tabName = "analysis",
        
        fluidRow(
          
          box(
            title = "AQI Trend Over Time",
            width = 12,
            plotlyOutput("aqiTrend")
          )
          
        ),
        
        fluidRow(
          
          box(
            title = "Temperature vs AQI",
            width = 6,
            plotlyOutput("temperatureAQI")
          ),
          
          box(
            title = "Humidity vs AQI",
            width = 6,
            plotlyOutput("humidityAQI")
          )
          
        )
        
      )
      
    )
    
  )
  
)


# ---------------- SERVER ----------------

server <- function(input, output) {
  
  # Filter data
  filteredData <- reactive({
    
    if (input$city == "All") {
      
      air
      
    } else {
      
      air %>%
        filter(City == input$city)
      
    }
    
  })
  
  
  # Average AQI
  output$avgAQI <- renderValueBox({
    
    valueBox(
      value = round(
        mean(filteredData()$AQI, na.rm = TRUE),
        2
      ),
      subtitle = "Average AQI",
      icon = icon("wind")
    )
    
  })
  
  
  # Maximum AQI
  output$maxAQI <- renderValueBox({
    
    valueBox(
      value = round(
        max(filteredData()$AQI, na.rm = TRUE),
        2
      ),
      subtitle = "Maximum AQI",
      icon = icon("arrow-up")
    )
    
  })
  
  
  # Minimum AQI
  output$minAQI <- renderValueBox({
    
    valueBox(
      value = round(
        min(filteredData()$AQI, na.rm = TRUE),
        2
      ),
      subtitle = "Minimum AQI",
      icon = icon("arrow-down")
    )
    
  })
  
  
  # Average AQI by City
  output$cityAQI <- renderPlotly({
    
    data <- filteredData() %>%
      group_by(City) %>%
      summarise(
        Average_AQI = mean(AQI, na.rm = TRUE)
      )
    
    p <- ggplot(
      data,
      aes(
        x = City,
        y = Average_AQI
      )
    ) +
      
      geom_col() +
      
      labs(
        x = "City",
        y = "Average AQI"
      ) +
      
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
    ggplotly(p)
    
  })
  
  
  # AQI Distribution
  output$aqiDistribution <- renderPlotly({
    
    p <- ggplot(
      filteredData(),
      aes(x = AQI)
    ) +
      
      geom_histogram(
        bins = 30
      ) +
      
      labs(
        title = "AQI Distribution",
        x = "AQI",
        y = "Frequency"
      )
    
    ggplotly(p)
    
  })
  
  
  # Pollutant Analysis
  output$pollutants <- renderPlotly({
    
    data <- filteredData() %>%
      summarise(
        
        `PM2.5` = mean(
          `PM2.5`,
          na.rm = TRUE
        ),
        
        PM10 = mean(
          PM10,
          na.rm = TRUE
        ),
        
        Ozone = mean(
          Ozone,
          na.rm = TRUE
        ),
        
        NO2 = mean(
          NO2,
          na.rm = TRUE
        ),
        
        CO = mean(
          CO,
          na.rm = TRUE
        ),
        
        SO2 = mean(
          SO2,
          na.rm = TRUE
        )
        
      ) %>%
      
      pivot_longer(
        cols = everything(),
        names_to = "Pollutant",
        values_to = "Average"
      )
    
    
    p <- ggplot(
      data,
      aes(
        x = Pollutant,
        y = Average
      )
    ) +
      
      geom_col() +
      
      labs(
        x = "Pollutant",
        y = "Average Level"
      )
    
    ggplotly(p)
    
  })
  
  
  # AQI Trend
  output$aqiTrend <- renderPlotly({
    
    data <- filteredData() %>%
      group_by(Date) %>%
      summarise(
        AQI = mean(
          AQI,
          na.rm = TRUE
        )
      )
    
    p <- ggplot(
      data,
      aes(
        x = Date,
        y = AQI
      )
    ) +
      
      geom_line() +
      
      labs(
        x = "Date",
        y = "AQI"
      )
    
    ggplotly(p)
    
  })
  
  
  # Temperature vs AQI
  output$temperatureAQI <- renderPlotly({
    
    p <- ggplot(
      filteredData(),
      aes(
        x = Temperature,
        y = AQI
      )
    ) +
      
      geom_point() +
      
      labs(
        x = "Temperature",
        y = "AQI"
      )
    
    ggplotly(p)
    
  })
  
  
  # Humidity vs AQI
  output$humidityAQI <- renderPlotly({
    
    p <- ggplot(
      filteredData(),
      aes(
        x = Humidity,
        y = AQI
      )
    ) +
      
      geom_point() +
      
      labs(
        x = "Humidity",
        y = "AQI"
      )
    
    ggplotly(p)
    
  })
  
}


# ---------------- RUN DASHBOARD ----------------

shinyApp(
  ui = ui,
  server = server
)