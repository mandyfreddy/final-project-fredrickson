# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

# shinyapp.R

# 1. Shiny app
# This app will allow users to select a state on a map to see veteran economic outcomes in
# income and LFP in that state compared to non-veterans.

library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("By State Analysis of Veterans' Economic Outcomes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("veteranStatus", "Select Population:",
        choices = c("All", "Veterans", "Non-Veterans")
      ),
      selectInput("economicMeasure", "Select Economic Outcome:",
        choices = c("Earnings", "Labor Force Participation")
      ),
      selectInput("stateSelection", "Select State:",
        choices = sort(unique(ipums_cleaned$STATENAME)),
        selected = 1
      )
    ),
    mainPanel(
      plotOutput("comparisonPlot")
    )
  )
)

server <- function(input, output) {
  output$comparisonPlot <- renderPlot({
    data_to_plot <- ipums_cleaned %>%
      filter(STATENAME == input$stateSelection, VETSTAT %in% c(1, 2))

    if (input$veteranStatus != "All") {
      vet_filter <- ifelse(input$veteranStatus == "Veterans", 2, 1)
      data_to_plot <- data_to_plot %>% filter(VETSTAT == vet_filter)
    }

    if (input$economicMeasure == "Earnings") {
      ggplot(data_to_plot, aes(
        x = as.factor(VETSTAT), y = INCWAGE,
        fill = as.factor(VETSTAT)
      )) +
        geom_bar(stat = "summary", fun = "mean", position = "dodge") +
        labs(
          title = paste("Average Earnings in", input$stateSelection, "by Veteran Status"),
          x = "Veteran Status",
          y = "Average Earnings"
        ) +
        scale_fill_manual(
          values = c("#ffd92f", "#7570b3"),
          labels = c("Non-Veterans", "Veterans")
        ) +
        theme_minimal() +
        guides(fill = guide_legend(title = "Veteran Status"))
    } else {
      ggplot(data_to_plot, aes(
        x = as.factor(VETSTAT), y = as.numeric(LABFORCE),
        fill = as.factor(VETSTAT)
      )) +
        geom_bar(stat = "summary", fun = "mean", position = "dodge") +
        labs(
          title = paste(
            "Labor Force Participation in", input$stateSelection,
            "by Veteran Status"
          ),
          x = "Veteran Status",
          y = "Proportion in Labor Force"
        ) +
        scale_fill_manual(
          values = c("#ffd92f", "#7570b3"),
          labels = c("Non-Veterans", "Veterans")
        ) +
        theme_minimal() +
        guides(fill = guide_legend(title = "Veteran Status"))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# UI
ui <- fluidPage(
  titlePanel("Veteran vs. Non-Veteran Economic Outcomes Comparison"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ageGroup", "Select Age Group:",
        choices = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+")
      ),
      selectInput("disabilityLevel", "Select Disability Level:",
        choices = c("No rating", "0%", "1-60%", "70%+", "Not reported")
      )
    ),
    mainPanel(
      plotOutput("earningsPlot"),
      plotOutput("laborForcePlot")
    )
  )
)

# Server
server <- function(input, output) {
  # Filtered data based on user input
  reactive_filtered_data <- reactive({
    ipums_cleaned %>%
      filter(
        AGE_GROUP == input$ageGroup,
        DISABILITY_SIMPLE == input$disabilityLevel
      )
  })

  # Plot for average earnings
  output$earningsPlot <- renderPlot({
    data_to_plot <- reactive_filtered_data()
    ggplot(data_to_plot, aes(
      x = as.factor(VETSTAT), y = INCWAGE,
      fill = as.factor(VETSTAT)
    )) +
      geom_bar(stat = "summary", fun = "mean", position = "dodge") +
      scale_fill_manual(
        values = c("#6BAED6", "#FC9272"),
        labels = c("Non-Veterans", "Veterans")
      ) +
      labs(
        title = paste(
          "Average Earnings for", input$disabilityLevel,
          "Disability in Age Group", input$ageGroup
        ),
        x = "Veteran Status",
        y = "Average Earnings",
        fill = "Veteran Status"
      ) +
      theme_minimal()
  })

  # Plot for labor force participation
  output$laborForcePlot <- renderPlot({
    data_to_plot <- reactive_filtered_data()
    ggplot(data_to_plot, aes(
      x = as.factor(VETSTAT), y = as.numeric(LABFORCE),
      fill = as.factor(VETSTAT)
    )) +
      geom_bar(stat = "summary", fun = "mean", position = "dodge") +
      scale_fill_manual(
        values = c("#6BAED6", "#FC9272"),
        labels = c("Non-Veterans", "Veterans")
      ) +
      labs(
        title = paste(
          "Labor Force Participation for", input$disabilityLevel,
          "Disability in Age Group", input$ageGroup
        ),
        x = "Veteran Status",
        y = "Labor Force Participation Rate",
        fill = "Veteran Status"
      ) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
