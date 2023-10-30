# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)

# User Interface (UI)
ui <- fluidPage(
  titlePanel("Dividend Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("investedMoney", "Initial Investment (€):", min = 0, max = 1000000, value = 10000, step = 1000),
      sliderInput("dividendYield", "Dividend Yield (%):", min = 1, max = 20, value = 5, step = 0.1),
      sliderInput("cagr", "CAGR (%):", min = 0, max = 20, value = 5, step = 0.1),
      sliderInput("monthlyInvestment", "Monthly Investment (€):", min = 0, max = 10000, value = 100, step = 100),
      sliderInput("numYears", "Number of Years:", min = 1, max = 50, value = 5),
      radioButtons("grossNet", "Dividend Type:", choices = c("Gross", "Net"), selected = "Gross"),
      numericInput("exemptAmount", "Yearly Exempt Amount (€):", value = 0, min = 0),
      checkboxInput("reinvest", "Reinvest Dividends?", value = FALSE),
      width = 3
    ),
    
    mainPanel(
      plotlyOutput("dividendPlot"),
      tableOutput("dividendTable")
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Calculate dividends and other data
  calculateData <- reactive({
    taxRate <- 0.2638
    months <- input$numYears * 12
    monthlyDividends <- rep(0, months)
    totalAmount <- rep(0, months)
    monthlyInvested <- rep(0, months)
    taxPaid <- rep(0, months)
    currentInvestment <- input$investedMoney
    remainingExemptAmount <- input$exemptAmount
    
    for (i in 1:months) {
      monthlyDividend <- currentInvestment * (input$dividendYield / 100) / 12
      
      if (input$grossNet == "Net") {
        taxFreeDividend <- min(monthlyDividend, remainingExemptAmount)
        taxableDividend <- monthlyDividend - taxFreeDividend
        taxPaid[i] <- taxableDividend * taxRate
        netMonthlyDividend <- monthlyDividend - taxPaid[i]
        remainingExemptAmount <- remainingExemptAmount - taxFreeDividend
      } else {
        netMonthlyDividend <- monthlyDividend
        taxPaid[i] <- 0
      }
      
      monthlyDividends[i] <- netMonthlyDividend
      totalAmount[i] <- currentInvestment
      if (input$reinvest) {
        currentInvestment <- currentInvestment + netMonthlyDividend
      }
      
      monthlyInvested[i] <- input$monthlyInvestment
      currentInvestment <- currentInvestment + monthlyInvested[i]
      
      if (i > 12) {
        currentInvestment <- currentInvestment * (1 + input$cagr/100/12)
      }
      
      # Reset the exempt amount every year
      if (i %% 12 == 0) {
        remainingExemptAmount <- input$exemptAmount
      }
    }
    
    return(data.frame(Year = rep(1:input$numYears, each=12), Month = rep(1:12, times=input$numYears),
                      Dividend = monthlyDividends, Tax = taxPaid, Total = totalAmount, 
                      Invested = monthlyInvested))
  })
  
  output$dividendPlot <- renderPlotly({
    # Create a data frame for plotting
    data <- calculateData()
    yearlyDividends <- aggregate(data$Dividend, by=list(Year=data$Year), FUN=sum)
    names(yearlyDividends) <- c("Year", "Dividend")
    
    # Create a ggplot object
    p <- ggplot(yearlyDividends, aes(x = Year, y = Dividend)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Dividends Growth", y = "Yearly Dividend (€)", x = "Year") +
      theme_minimal() +
      scale_x_continuous(breaks = 1:input$numYears)
    
    # Convert the ggplot object to a plotly object
    ggplotly(p, tooltip = "y")
  })
  
  output$dividendTable <- renderTable({
    # Display table with all the data
    calculateData()
  }, rownames = TRUE)
}

# Run the Shiny App
shinyApp(ui, server)
