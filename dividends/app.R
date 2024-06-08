# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate) # Import lubridate for date functions

# User Interface (UI)
ui <- fluidPage(
  titlePanel("Dividend Income Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("investedMoney", "Initial Investment (€):", min = 0, max = 1000000, value = 10000, step = 1000),
      sliderInput("dividendYield", "Dividend Yield (%):", min = 0, max = 20, value = 5, step = 0.1),
      sliderInput("cagr", "CAGR (%):", min = 0, max = 20, value = 5, step = 0.1),
      sliderInput("monthlyInvestment", "Monthly Investment (€):", min = 0, max = 10000, value = 100, step = 100),
      sliderInput("investmentGrowth", "Yearly Increase in Monthly Investment (%):", min = 0, max = 20, value = 2, step = 0.1),
      sliderInput("numYears", "Number of Years:", min = 1, max = 50, value = 5, step = 1),
      sliderInput("stopInvestingYears", "Stop Investing After Years:", min = 0, max = 50, value = 5, step = 1),
      numericInput("exemptAmount", "Yearly Exempt Amount (€):", value = 0, min = 0),
      numericInput("taxRate", "Tax Rate (%):", value = 26.38, min = 0, max = 100, step = 0.01),
      width = 3,
      downloadButton("downloadData", "Download Table")
    ),
    
    mainPanel(
      plotlyOutput("dividendPlot"),
      tableOutput("dividendTable")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Observe the numYears slider and update stopInvestingYears max value
  observe({
    updateSliderInput(session, "stopInvestingYears", max = input$numYears)
  })
  
  # Calculate dividends and other data
  calculateData <- reactive({
    current_year <- year(Sys.Date())
    
    taxRate <- input$taxRate / 100
    years <- input$numYears
    stopInvestingYears <- input$stopInvestingYears
    monthlyCAGR <- (1 + input$cagr / 100)^(1/12) - 1
    
    data <- data.frame(
      Year = integer(years * 12),
      Month = integer(years * 12),
      DividendReinvest = numeric(years * 12),
      DividendNoReinvest = numeric(years * 12),
      TotalReinvest = numeric(years * 12),
      TotalNoReinvest = numeric(years * 12),
      MonthlyInvestment = numeric(years * 12),
      TaxReinvest = numeric(years * 12),
      TaxNoReinvest = numeric(years * 12)
    )
    
    currentInvestmentReinvest <- input$investedMoney
    currentInvestmentNoReinvest <- input$investedMoney
    currentMonthlyInvestment <- input$monthlyInvestment
    
    for (year in 1:years) {
      for (month in 1:12) {
        idx <- (year - 1) * 12 + month
        
        # Update the exempt amount for each month
        remainingExemptAmountReinvest <- input$exemptAmount / 12
        remainingExemptAmountNoReinvest <- input$exemptAmount / 12
        
        # Reinvest scenario
        monthlyDividendReinvest <- currentInvestmentReinvest * (input$dividendYield / 100) / 12
        taxFreeDividendReinvest <- min(monthlyDividendReinvest, remainingExemptAmountReinvest)
        taxableDividendReinvest <- monthlyDividendReinvest - taxFreeDividendReinvest
        taxPaidReinvest <- taxableDividendReinvest * taxRate
        netMonthlyDividendReinvest <- monthlyDividendReinvest - taxPaidReinvest
        
        if (year <= stopInvestingYears) {
          currentInvestmentReinvest <- currentInvestmentReinvest + netMonthlyDividendReinvest + currentMonthlyInvestment
        } else {
          currentInvestmentReinvest <- currentInvestmentReinvest 
        }
        
        currentInvestmentReinvest <- currentInvestmentReinvest * (1 + monthlyCAGR)
        
        # No reinvest scenario
        monthlyDividendNoReinvest <- currentInvestmentNoReinvest * (input$dividendYield / 100) / 12
        taxFreeDividendNoReinvest <- min(monthlyDividendNoReinvest, remainingExemptAmountNoReinvest)
        taxableDividendNoReinvest <- monthlyDividendNoReinvest - taxFreeDividendNoReinvest
        taxPaidNoReinvest <- taxableDividendNoReinvest * taxRate
        netMonthlyDividendNoReinvest <- monthlyDividendNoReinvest - taxPaidNoReinvest
        
        if (year <= stopInvestingYears) {
          currentInvestmentNoReinvest <- currentInvestmentNoReinvest + currentMonthlyInvestment
        }
        
        currentInvestmentNoReinvest <- currentInvestmentNoReinvest * (1 + monthlyCAGR)
        
        # Store the data
        data[idx, ] <- c(current_year + year - 1, month, netMonthlyDividendReinvest, netMonthlyDividendNoReinvest,
                         currentInvestmentReinvest, currentInvestmentNoReinvest,
                         ifelse(year <= stopInvestingYears, currentMonthlyInvestment, 0), 
                         taxPaidReinvest, taxPaidNoReinvest)
      }
      
      # Increase the monthly investment at the beginning of each year
      if (year <= stopInvestingYears) {
        currentMonthlyInvestment <- currentMonthlyInvestment * (1 + input$investmentGrowth / 100)
      }
    }
    
    # Round the data to two decimal places
    data <- data %>%
      mutate(
        Year = as.integer(Year),
        Month = as.integer(Month)
      ) %>%
      round(2)
    
    return(data)
  })
  
  output$dividendPlot <- renderPlotly({
    # Create a data frame for plotting
    data <- calculateData()
    
    # Summarize data yearly
    data_yearly <- data %>%
      group_by(Year) %>%
      summarise(
        DividendReinvest = sum(DividendReinvest),
        DividendNoReinvest = sum(DividendNoReinvest)
      )
    
    # Create the plot
    p <- plot_ly(data_yearly, x = ~Year) %>%
      add_lines(y = ~DividendReinvest, name = "Reinvest Dividends", hoverinfo = "text", text = ~paste("Year:", Year, "<br>Dividend:", DividendReinvest)) %>%
      add_lines(y = ~DividendNoReinvest, name = "Do Not Reinvest Dividends", hoverinfo = "text", text = ~paste("Year:", Year, "<br>Dividend:", DividendNoReinvest)) %>%
      layout(title = "Yearly Dividend", xaxis = list(title = "Year"), yaxis = list(title = "Value in €"))
    
    p
  })
  
  output$dividendTable <- renderTable({
    # Display table with all the data
    calculateData()
  }, rownames = TRUE)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dividend_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(calculateData(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny App
shinyApp(ui, server)
