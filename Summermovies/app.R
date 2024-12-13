#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(curl)
summer_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-30/summer_movies.csv')

# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Interactive Plot of IMDb Ratings vs. Movie Runtime"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "runtime_range", 
        "Select Runtime Range (minutes):",
        min = summer_movies |> pull(runtime_minutes) |> min(na.rm = TRUE),
        max = summer_movies |> pull(runtime_minutes) |> max(na.rm = TRUE),
        value = c(
          summer_movies |> pull(runtime_minutes) |> min(na.rm = TRUE),
          summer_movies |> pull(runtime_minutes) |> max(na.rm = TRUE)
        )
      ),
      sliderInput(
        "rating_range",
        "Select IMDb Rating Range:",
        min = summer_movies |> pull(average_rating) |> min(na.rm = TRUE),
        max = summer_movies |> pull(average_rating) |> max(na.rm = TRUE),
        value = c(
          summer_movies |> pull(average_rating) |> min(na.rm = TRUE),
          summer_movies |> pull(average_rating) |> max(na.rm = TRUE)
        )
      )
    ),
    
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    summer_movies |>
      filter(
        runtime_minutes >= input$runtime_range[1],
        runtime_minutes <= input$runtime_range[2],
        average_rating >= input$rating_range[1],
        average_rating <= input$rating_range[2]
      )
  })
  
  output$scatterPlot <- renderPlot({
    filtered_data() |>
      ggplot(aes(x = runtime_minutes, y = average_rating)) +
      geom_point() +
      labs(
        x = "Runtime (minutes)",
        y = "Average IMDb Rating",
        title = "Distribution of IMDb Ratings by Movie Runtime"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)