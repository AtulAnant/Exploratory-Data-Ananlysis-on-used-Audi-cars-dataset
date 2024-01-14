
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

audi_data <- read.csv("audi.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Audi Data Exploration"),
  dashboardSidebar(
    checkboxGroupInput("fuel_type", "Select Fuel Types", choices = unique(audi_data$fuelType), selected = unique(audi_data$fuelType)),
    sliderInput("price_range", "Price Range", min = min(audi_data$price), max = max(audi_data$price), value = c(min(audi_data$price), max(audi_data$price))),
    selectInput("transmission_type", "Select Transmission Type", choices = c("Automatic", "Manual", "Semi-Auto"))
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Fuel Type variations with number of cars",
        status = "primary",
        solidHeader = TRUE,
        plotOutput("barPlot")
      ),
      box(
        title = "Fuel Type variations with price",
        status = "info",
        solidHeader = TRUE,
        plotOutput("boxPlot")
      ),
      box(
        title = "Change in Mileage with price and fuel type",
        status = "success",
        solidHeader = TRUE,
        plotOutput("scatterPlot")
      ),
      box(
        title = "Car Model Insights",
        status = "info",
        solidHeader = TRUE,
        plotOutput("modelPlot")
      )
    )
  )
)

server <- function(input, output) {
  
  # Filtered data based on user inputs
  filtered_data <- reactive({
    filter(audi_data, 
           fuelType %in% input$fuel_type &
             price >= input$price_range[1] & price <= input$price_range[2] &
             (is.null(input$transmission_type) | transmission == input$transmission_type)
    )
  })
  
  # Bar Plot
  output$barPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = fuelType, fill = fuelType)) +
      geom_bar() +
      theme_minimal() +
      labs(title = "Bar Plot of Fuel Types")
  })
  
  # Box Plot
  output$boxPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = fuelType, y = price, fill = fuelType)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Box Plot of Prices by Fuel Type")
  })
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = mileage, y = price, color = fuelType)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Scatter Plot of Mileage vs. Price")
  })
  
  # Car Model Insights - Average Price Bar Plot
  output$modelPlot <- renderPlot({
    avg_price <- aggregate(price ~ model, data = filtered_data(), FUN = mean)
    ggplot(avg_price, aes(x = reorder(model, price), y = price)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      theme_minimal() +
      labs(title = "Average Price by Car Model", x = "Car Model", y = "Average Price") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)
