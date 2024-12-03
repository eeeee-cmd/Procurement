#### Preamble ####
# Purpose: This Shiny application provides an interactive analysis of federal procurement contracts.
# Author: Deyi Kong
# Date: December 3rd, 2024
# Contact: deyi.kong@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `shiny`, `ggplot2`, `DT`, `dplyr`, `rstanarm`, and `bayesplot` packages must be installed.
# Any other information needed? Ensure that you are working within the `Procurement`.rproj.

# Load necessary libraries
library(shiny)
library(ggplot2)
library(DT)
library(dplyr) 
library(rstanarm)
library(bayesplot)

# Define the UI
ui <- fluidPage(
  titlePanel("Analysis of Federal Contracts"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Cleaned Data (CSV)", accept = ".csv"),
      selectInput("plotType", "Select Plot Type", 
                  choices = c("Yearly Contract Amounts", 
                              "Trace Plot", 
                              "Posterior Predictive Check")),
      numericInput("titleSize", "Title Font Size", value = 14, min = 10, max = 30),
      actionButton("refresh", "Refresh Plot"),
      br(),
      h3("Model Results"),
      DTOutput("resultsTable")
    ),
    mainPanel(
      plotOutput("mainPlot"),
      h3("Description"),
      textOutput("plotDescription")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Reactive expression to load data
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    
    # Print column names for debugging
    print(colnames(df))
    
    # Simulate 'yrep' if missing
    if (!("yrep" %in% colnames(df))) {
      set.seed(123)
      df$yrep <- rnorm(nrow(df), mean = mean(df$Amount), sd = sd(df$Amount))
    }
    
    # Simulate 'Prior' if missing
    if (!("Prior" %in% colnames(df))) {
      df$Prior <- rnorm(nrow(df), mean = 0, sd = 1)
    }
    
    # Simulate 'Posterior' if missing
    if (!("Posterior" %in% colnames(df))) {
      df$Posterior <- rnorm(nrow(df), mean = 1, sd = 0.5)
    }
    
    # If 'y' doesn't exist, use 'Amount' as observed 'y'
    if (!("y" %in% colnames(df))) {
      df$y <- df$Amount
    }
    
    return(df)
  })
  
  # Fit the Bayesian model reactively based on the uploaded data
  bayesian_model <- reactive({
    req(data())  # Ensure data is available before fitting the model
    # Fit a simple Bayesian model using rstanarm
    stan_glm(Amount ~ Year + ContractType, data = data(), family = gaussian())
  })
  
  # Model results table (replace this with actual model results)
  model_results <- reactive({
    data.frame(
      Metric = c("R2", "R2 Adjusted", "WAIC", "RMSE"),
      Bayesian_Full = c(0.289, 0.015, 30992.3, 2714467.04),
      Bayesian_Simple = c(0.082, 0.027, 30894.8, 2971135.35)
    )
  })
  
  output$resultsTable <- renderDT({
    datatable(model_results(), options = list(pageLength = 5))
  })
  
  # Generate the plots based on user input
  output$mainPlot <- renderPlot({
    req(bayesian_model())  # Ensure the model is fitted before rendering the plot
    plotType <- input$plotType
    titleSize <- input$titleSize
    
    if (plotType == "Yearly Contract Amounts") {
      yearly_amount <- data() %>%
        group_by(Year = as.factor(Year)) %>%
        summarise(TotalAmount = sum(Amount))
      
      ggplot(yearly_amount, aes(x = Year, y = TotalAmount)) +
        geom_bar(stat = "identity", fill = "darkorange", color = "black", alpha = 0.7) +
        labs(
          title = "Total Contract Amounts by Year for Microsoft",
          x = "Year",
          y = "Total Amount ($million)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = titleSize),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size = 8)
        )
      
    } else if (plotType == "Trace Plot") {
      trace_plot <- bayesplot::mcmc_trace(bayesian_model())
      trace_plot +
        theme_minimal() +
        theme(legend.position = "bottom", legend.text = element_text(size = 6)) +
        theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6))
      
    } else if (plotType == "Posterior Predictive Check") {
      pp_check_plot <- bayesplot::pp_check(bayesian_model())
      pp_check_plot +
        theme_classic() +
        theme(legend.position = "bottom")
    }
  })
  
  # Add descriptions for each plot
  output$plotDescription <- renderText({
    plotType <- input$plotType
    if (plotType == "Yearly Contract Amounts") {
      "This plot shows the total contract amounts awarded to Microsoft for each year, providing a temporal overview of federal spending."
    } else if (plotType == "Trace Plot") {
      "The trace plot displays the sampled parameter values over iterations for each Markov chain, assessing convergence."
    } else if (plotType == "Posterior Predictive Check") {
      "This plot compares observed data to simulated data generated under the posterior distribution to evaluate model fit."
      }
    })
}

# Run the app
shinyApp(ui = ui, server = server)

