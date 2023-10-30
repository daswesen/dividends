# Load necessary libraries
library(shiny)
library(ggplot2)

# User Interface (UI)
ui <- fluidPage(
  titlePanel("Dividend Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("investedMoney", "Initial Investment (€):", min = 0, max = 1000000, value = 10000, step = 1000),
      sliderInput("dividendYield", "Dividend Yield (%):", min = 1, max = 20, value = 5, step = 0.1),
      sliderInput("cagr", "CAGR (%):", min = 0, max = 20, value = 5, step = 0.1),
      sliderInput("monthlyInvestment", "Monthly Investment (€):", min = 0, max = 10000, value = 100, step = 100),
      radioButtons("grossNet", "Dividend Type:", choices = c("Gross", "Net"), selected = "Gross"),
      numericInput("exemptAmount", "Yearly Exempt Amount (€):", value = 0, min = 0),
      checkboxInput("reinvest", "Reinvest Dividends?", value = FALSE),
      width = 3
    ),
    
    mainPanel(
      plotOutput("dividendPlot"),
      tableOutput("dividendTable")
    )
  )
)

# Server Logic
server <- function(input, output) {
  
  # Define years and months
  years <- 10
  months <- years * 12
  
  # Calculate dividends and other data
  calculateData <- reactive({
    taxRate <- 0.2638
    monthlyDividends <- rep(0, months)
    totalAmount <- rep(0, months)
    monthlyInvested <- rep(0, months)
    taxPaid <- rep(0, months)
    currentInvestment <- input$investedMoney
    
    for (i in 1:months) {
      monthlyDividend <- currentInvestment * (input$dividendYield / 100) / 12
      taxPaid[i] <- max(monthlyDividend - input$exemptAmount/12, 0) * taxRate
      netMonthlyDividend <- monthlyDividend - taxPaid[i]
      
      monthlyDividends[i] <- netMonthlyDividend
      totalAmount[i] <- currentInvestment
      monthlyInvested[i] <- input$monthlyInvestment + (if (input$reinvest) netMonthlyDividend else 0)
      
      currentInvestment <- currentInvestment + monthlyInvested[i]
      currentInvestment <- currentInvestment * (1 + input$cagr/100/12)
    }
    
    return(data.frame(Year = rep(1:years, each=12), Month = rep(1:12, times=years),
                      Dividend = monthlyDividends, Tax = taxPaid, Total = totalAmount, 
                      Invested = monthlyInvested))
  })
  
  output$dividendPlot <- renderPlot({
    # Create a data frame for plotting
    data <- calculateData()
    
    yearlyDividends <- aggregate(data$Dividend, by=list(Year=data$Year), FUN=sum)
    names(yearlyDividends) <- c("Year", "Dividend")
    
    # Plot
    ggplot(yearlyDividends, aes(x = Year, y = Dividend)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label=sprintf("€%.2f", Dividend)), vjust=-0.5, color="black") +
      labs(title = "Dividends Growth", y = "Yearly Dividend (€)", x = "Year") +
      theme_minimal() +
      scale_x_continuous(breaks = 1:years)
  })
  
  
  output$dividendTable <- renderTable({
    # Display table with all the data
    calculateData()
  }, rownames = TRUE)
}

# Run the Shiny App
shinyApp(ui, server)
