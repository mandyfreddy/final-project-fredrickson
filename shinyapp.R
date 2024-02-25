# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

# Two interactive Shiny plots

# Shiny app 1
# This app will allow users to select a range of disability ratings to view average
# earnings and labor force participation for those ratings, and compared to their age group

# Library
library(shiny)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Disability Rating and Economic Outcomes"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("disabilityRange", "Select Disability Rating Range:",
                  min = min(as.numeric(as.character(ipums_cps_cleaned$VDISRATE)), na.rm = TRUE),
                  max = max(as.numeric(as.character(ipums_cps_cleaned$VDISRATE)), na.rm = TRUE),
                  value = c(min(as.numeric(as.character(ipums_cps_cleaned$VDISRATE)), na.rm = TRUE), 
                            max(as.numeric(as.character(ipums_cps_cleaned$VDISRATE)), na.rm = TRUE))
      )
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
    ipums_cps_cleaned %>%
      mutate(VDISRATE = as.numeric(as.character(VDISRATE))) %>%
      filter(
        VDISRATE >= input$disabilityRange[1], VDISRATE <= input$disabilityRange[2],
        !is.na(age_group) # Filter out NA values for age_group
      ) %>%
      group_by(VDISRATE, age_group) %>%
      summarise(
        Average_INCWAGE = mean(INCWAGE, na.rm = TRUE),
        Labor_Force_Participation = mean(as.numeric(LABFORCE) - 1, na.rm = TRUE)
      )
  })
  
  output$earningsPlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = as.factor(VDISRATE), 
                                       y = Average_INCWAGE, fill = age_group)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Average Earnings by Disability Rating and Age Group",
        x = "Disability Rating",
        y = "Average Earnings"
      ) +
      scale_fill_brewer(palette = "Set1")
  })
  
  output$lfpPlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = as.factor(VDISRATE), 
                                       y = Labor_Force_Participation, fill = age_group)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Labor Force Participation by Disability Rating and Age Group",
        x = "Disability Rating",
        y = "Labor Force Participation Rate"
      ) +
      scale_fill_brewer(palette = "Set1")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# 2. Shiny app
# This app will allow users to select a state on a map to see veteran economic outcomes in
# income and LFP in that state.

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
                  choices = sort(unique(ipums_cleaned$STATEFIP)),
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
      filter(STATEFIP == input$stateSelection, VETSTAT %in% c(1, 2))
    
    if (input$veteranStatus != "All") {
      vet_filter <- ifelse(input$veteranStatus == "Veterans", 2, 1)
      data_to_plot <- data_to_plot %>% filter(VETSTAT == vet_filter)
    }
    
    if (input$economicMeasure == "Earnings") {
      ggplot(data_to_plot, aes(x = as.factor(VETSTAT), y = INCWAGE, 
                               fill = as.factor(VETSTAT))) +
        geom_bar(stat = "summary", fun = "mean", position = "dodge") +
        labs(
          title = paste("Average Earnings in", input$stateSelection, "by Veteran Status"),
          x = "Veteran Status",
          y = "Average Earnings"
        ) +
        scale_fill_manual(values = c("#ffd92f", "#7570b3"), 
                          labels = c("Non-Veterans", "Veterans")) +
        theme_minimal() +
        guides(fill = guide_legend(title = "Veteran Status"))
    } else {
      ggplot(data_to_plot, aes(x = as.factor(VETSTAT), y = as.numeric(LABFORCE), 
                               fill = as.factor(VETSTAT))) +
        geom_bar(stat = "summary", fun = "mean", position = "dodge") +
        labs(
          title = paste("Labor Force Participation in", input$stateSelection, "by Veteran Status"),
          x = "Veteran Status",
          y = "Proportion in Labor Force"
        ) +
        scale_fill_manual(values = c("#ffd92f", "#7570b3"), 
                          labels = c("Non-Veterans", "Veterans")) +
        theme_minimal() +
        guides(fill = guide_legend(title = "Veteran Status"))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
