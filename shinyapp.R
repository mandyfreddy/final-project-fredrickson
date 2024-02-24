# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

# Two interactive Shiny plots

# Library
library(shiny)
library(ggplot2)
library(dplyr)

# Shiny app 1
# This app will allow users to select a range of disability ratings to view average
# earnings and labor force participation for those ratings, and compared to their age group

# UI
ui <- fluidPage(
  titlePanel("Disability Rating and Economic Outcomes"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("disabilityRange", "Select Disability Rating Range:",
        min = min(ipums_cps$VDISRATE, na.rm = TRUE),
        max = max(ipums_cps$VDISRATE, na.rm = TRUE),
        value = c(min(ipums_cps$VDISRATE, na.rm = TRUE), max(ipums_cps$VDISRATE, 
                                                             na.rm = TRUE))
      ),
      checkboxGroupInput("stateInput", "Select States:", 
                         choices = unique(ipums_cps$STATEFIP))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Earnings", plotOutput("earningsPlot")),
        tabPanel("Labor Force Participation", plotOutput("lfpPlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    ipums_cps %>%
      filter(
        VDISRATE >= input$disabilityRange[1], VDISRATE <= input$disabilityRange[2],
        STATEFIP %in% input$stateInput
      )
  })

  output$earningsPlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = as.factor(VDISRATE), y = INCWAGE)) +
      geom_boxplot() +
      labs(
        title = "Earnings by Disability Rating",
        x = "Disability Rating",
        y = "Earnings"
      )
  })

  output$lfpPlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = as.factor(VDISRATE), fill = as.factor(LABFORCE))) +
      geom_bar(position = "fill") +
      labs(
        title = "Labor Force Participation by Disability Rating",
        x = "Disability Rating",
        y = "Proportion in Labor Force"
      ) +
      scale_fill_discrete(name = "Labor Force Participation", labels = c("Not in LFP", "In LFP"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# 2. Shiny app 
# This app will allow users to select a state to see veteran economic outcomes in 
# income and LFP in that state.

# UI
ui <- fluidPage(
  titlePanel("Regional Analysis of Veterans' Economic Outcomes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("veteranStatus", "Select Population:",
        choices = c("All", "Veterans", "Non-Veterans")
      ),
      selectInput("economicMeasure", "Select Economic Outcome:",
        choices = c("Earnings", "Labor Force Participation")
      )
    ),
    mainPanel(
      plotOutput("comparisonPlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$comparisonPlot <- renderPlot({
    # This is a placeholder. You would need to filter and generate the appropriate plots
    # based on the selected veteran status and economic measure.
    # Below is a simple example of what the logic could look like:

    data_to_plot <- ipums_cleaned # or ipums_cps based on the analysis you want to perform
    if (input$veteranStatus != "All") {
      data_to_plot <- data_to_plot[data_to_plot$VETSTAT == (input$veteranStatus == "Veterans"), ]
    }

    if (input$economicMeasure == "Earnings") {
      ggplot(data_to_plot, aes(x = STATEFIP, y = INCEARN, group = VETSTAT, fill = VETSTAT)) +
        geom_bar(stat = "summary", fun = "mean") +
        labs(
          title = "Average Earnings by State and Veteran Status",
          x = "State",
          y = "Average Earnings"
        )
    } else {
      ggplot(data_to_plot, aes(x = STATEFIP, fill = LABFORCE)) +
        geom_bar(position = "fill") +
        labs(
          title = "Labor Force Participation by State",
          x = "State",
          y = "Proportion in Labor Force"
        )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
