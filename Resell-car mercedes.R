library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# Load the dataset
mydata <- read.csv("/Users/arkaroy/Desktop/Kaggle/merc.csv")

# Define UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    # Application title
    title = "Mercedes Resell price Analysis"
  ),
  
  dashboardSidebar(
    # Add any other sidebar content common to all tabs
    radioButtons("page",
                 "Select Page:",
                 choices = c("Transmission Analysis", "Fuel Type Analysis",  "Model Analysis"),
                 selected = "Transmission Analysis")
  ),
  dashboardBody(
    # Content for Transmission Analysis
    conditionalPanel(
      condition = "input.page == 'Transmission Analysis'",
      fluidRow(
        column(3,
               radioButtons("transmission_type",
                            "Select Transmission Type:",
                            choices = c( "Automatic", "Manual", "Semi-Auto"),
                            selected = "Automatic")),
        column(width = 12, plotOutput("transmission_plot"))
      )
    ),
    
    # Content for Fuel Type Analysis
    conditionalPanel(
      condition = "input.page == 'Fuel Type Analysis'",
      fluidRow(
        column(3,
               radioButtons("fuel_type",
                            "Select Fuel Type:",
                            choices = c("All", "Petrol", "Diesel", "Hybrid", "Other"),
                            selected = "All")),
        column(width = 12, plotOutput("fuel_type_plot")),
        column(width = 12, plotOutput("fuel_type_line_plot"))
      )
    ),
    
    # Content for Model Analysis
    conditionalPanel(
      condition = "input.page == 'Model Analysis'",
      fluidRow(
        column(3,
               radioButtons("fuel_type_model",
                            "Select Fuel Type:",
                            choices = c( "Petrol", "Diesel", "Hybrid"),
                            selected = "Petrol")),
        column(width = 12, plotOutput("model_plot", height = "500px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Transmission Analysis Plot
  output$transmission_plot <- renderPlot({
    filtered_data <- switch(input$transmission_type,
                            "All"       = mydata,
                            "Automatic" = filter(mydata, transmission == "Automatic"),
                            "Manual"    = filter(mydata, transmission == "Manual"),
                            "Semi-Auto" = filter(mydata, transmission == "Semi-Auto"))
    
    ggplot(filtered_data) +
      geom_point(aes(x = year, y = price, colour = transmission), size = 0.2) +
      ggtitle("Price variation with respect to transmission") +
      coord_trans(x = "log", y = "log") +
      facet_wrap(~transmission, ncol = 3) +
      theme(axis.text.x = element_text(angle = 45), axis.text.y = element_text(angle = 0))
  })
  
  # Fuel Type Analysis Bar Plot
  output$fuel_type_plot <- renderPlot({
    filtered_data <- switch(input$fuel_type,
                            "All"    = mydata,
                            "Petrol" = filter(mydata, fuelType == "Petrol"),
                            "Diesel" = filter(mydata, fuelType == "Diesel"),
                            "Hybrid" = filter(mydata, fuelType == "Hybrid"),
                            "Other"  = filter(mydata, fuelType == "Other"))
    
    ggplot(filtered_data) +
      geom_bar(aes(x = model, fill = fuelType), stat = "count") +
      ggtitle("Model Re-sale count with respect to selected fuel type and model") +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  # Fuel Type Analysis Line Plot
  output$fuel_type_line_plot <- renderPlot({
    filtered_data <- switch(input$fuel_type,
                            "All"    = mydata,
                            "Petrol" = filter(mydata, fuelType == "Petrol"),
                            "Diesel" = filter(mydata, fuelType == "Diesel"),
                            "Hybrid" = filter(mydata, fuelType == "Hybrid"),
                            "Other"  = filter(mydata, fuelType == "Other"))
    
    ggplot(filtered_data) +
      stat_summary(aes(x = year, y = price, color = fuelType), fun = "mean", geom = "line", size = 1) +
      ggtitle("Mean Resale Price Variation with Respect to Fuel Type") +
      theme(axis.text.x = element_text(angle = 45), axis.text.y = element_text(angle = 0)) +
      scale_x_continuous(breaks = seq(1970, 2020, by = 5))  # Adjust x-axis breaks for better readability
  })
  
  # Model Analysis Plot with Faceting
  output$model_plot <- renderPlot({
    filtered_data <- switch(input$fuel_type_model,
                            "Petrol" = filter(mydata, fuelType == "Petrol"),
                            "Diesel" = filter(mydata, fuelType == "Diesel"),
                            "Hybrid" = filter(mydata, fuelType == "Hybrid"),
                            "All"    = mydata)
    
    ggplot(data = filtered_data, aes(x = year, y = price, color = model)) +
      geom_line(stat = "summary", fun = "mean") +
      ggtitle("Mean Resale Price with respect to selected fuel type and model") +
      facet_wrap(~model, scales = "free_y", ncol = 3) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),  # Adjust font size
        axis.text.y = element_text(size = 6),  # Adjust font size
        strip.text = element_text(size = 7, face = "bold"),  # Adjust strip text font size
        strip.background = element_rect(fill = "lightgray"),  # Background color for facet labels
        plot.title = element_text(size = 14, face = "bold")  # Adjust plot title font size
      ) +
      scale_x_continuous(breaks = seq(1970, 2020, by = 5))  # Adjust x-axis breaks for better readability
  })
}

# Run the application
shinyApp(ui = ui, server = server)


