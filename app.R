library(quantmod)
library(shiny)
library(forecast)
library(rugarch)
library(prophet)
library(ggplot2)
library(shinyalert)

ui <- fluidPage(
  
  # browser  title
  title = "Stock/ETF/Bond Analysis and Predictions",
  # Application title
  titlePanel(div("Stock/ETF/Bond Analysis and Predictions", style = "color: #FFFFFF;font-size:25px;font-style: italic;")),
  
  tags$style('.container-fluid {background-color:black;}'),
  
  # Sidebar for stocks
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="tabset",
                  tabPanel("Stocks",   selectInput("Stock", "Stock", choices = c("AAPL", "TSLA", "MSFT", "GOOG", "AMZN"), width = "100px")),
                  tabPanel("ETF", selectInput("ETF", "ETF/Sector", choices = c("VTI", "VOO", "VUG", "AOR", "AOM"), width = "100px")),
                  tabPanel("Bonds",   selectInput("Bonds", "Bonds", choices = c("DEECX","FCBFX"), width = "100px"))
      ),
      
      # Date range
      dateRangeInput("date_range", "Date range:", start = Sys.Date()-200, end = Sys.Date(), width="250px",format = "yyyy-mm-dd"),
      
      # days for prediction ahead
      div(style = "white-space: nowrap;", 
          numericInput("h", "Days to Predict", value = 2, width = "65px")
      ),
      
      # add options for selecting a prediction method
      radioButtons("model", "Forecasting Model Applied",
                   choices = c("Naive", "ARIMA", "GARCH","Prophet","K-NN"),
                   choiceValues = "ARIMA"
      )
      
    ),
    
    # Show a plot of the generated distribution
    # mainPanel(plotOutput("qm_plot"))
    
    fluidRow(
      column(width = 7, plotOutput("qm_plot") ),
      column(width = 7, plotOutput("qm_plot1"))
    )
  )
)

server <- function(input, output, session){
  
  shinyalert(html = TRUE, text = tagList(
    textInput("Disclaimer", "This is for information only. Do not take this as financial advice.", "I understand."),
  ))

  #Show a plot of the ticker historical distribution
  output$qm_plot <- renderPlot({
    
    if(input$tabset =="Stocks")
    {
      ticker = input$Stock
    }

    else if(input$tabset =="Bonds")
    {
      ticker = input$Bonds}
    
    else
    {
      ticker = input$ETF
    }      
    
    stockEnv <- new.env()
    getSymbols(ticker, from=input$date_range[1],to=input$date_range[2], env=stockEnv)
    stock = ls(stockEnv)
    chartSeries(stockEnv[[ls(stockEnv)]], 
                name = ticker, theme=chartTheme('white')) 
    
    MACD2plot_plus   = which(diff(sign(MACD(Cl(stockEnv[[stock]]))[,1])) > 0)
    MACD2plot_minus  = which(diff(sign(MACD(Cl(stockEnv[[stock]]))[,1])) < 0)
    range_y          = 0.5
    print(addPoints(MACD2plot_plus,  stockEnv[[stock]][MACD2plot_plus,  4] - range_y, pch=24, col='blue', offset=1.0))
    print(addPoints(MACD2plot_minus, stockEnv[[stock]][MACD2plot_minus, 4] + range_y, pch=25, col='red',  offset=1.0))
    print(addSMA(n=12, on=1, col = "blue"))
    print(addSMA(n=26, on=1, col = "red"))
    
    
  })
  
  # Section to display forecast models
  output$qm_plot1 <- renderPlot({
    
    if(input$tabset =="Stocks")
    {
      ticker = input$Stock
    }
    else if(input$tabset =="Bonds")
    {
      ticker = input$Bonds
    }
    else
    {
      ticker = input$ETF
    }
    
    stock <- getSymbols(ticker,src="yahoo",from=input$date_range[1],to=input$date_range[2],env=NULL)
    n <- input$h
    end = dim(stock)[1]
    start = 1 #end - 1
    close_price = paste(ticker,".Close",sep="")
    
    if (input$model == "Naive"){
      
      mod <- naive(stock[start : end, close_price])
      # Plot the result. 
      autoplot(forecast(mod, h = n)) +
        ggplot2::labs(y = "Price($)", x = " Time (days)")
      
    } else if (input$model == "ARIMA"){
      
      # Create the Model
      mod <- auto.arima(stock[start : end, close_price])
      # Plot the result
      autoplot(forecast(mod, h = n)) +
        ggplot2::labs(y = "Price($)", x = " Time (days)")
      
    } else if (input$model == "GARCH") {
      
      
      #Set up the GARCH model using ugarchspec. Using sGARCH, which assumes symmetry
      spec <- ugarchspec(mean.model = list(armaOrder = c(1,1)), variance.model = list(model = "sGARCH"))
      
      #Fit the model
      garchfit <- ugarchfit(spec = spec, data = stock[start : end, close_price])
      
      #Get the standard deviation of the residuals
      volatility <- sigma(garchfit)
      
      # Plotting the obtained volatility
      plot(volatility)
      
      
      
    } else if (input$model == "K-NN") {
      
      mod <- nnetar(stock[start : end, close_price])
      # Plot the result
      autoplot(forecast(mod, h = n)) +
        ggplot2::labs(y = "Price($)", x = " Time (days)")
      
    }
    else if (input$model == "Prophet") {
      
      df <- stock[start : end, close_price]
      data_prop <- as.data.frame(df)
      data_prop$Date <- as.Date(rownames(data_prop))
      colnames(data_prop) <- c('y','ds')
      
      mod_prop <- prophet(data_prop,daily.seasonality = TRUE)
      
      future_prop <- make_future_dataframe(mod_prop, periods = n)
      forecast_prop <- predict(mod_prop, future_prop)
      # Plot the result
      #plot(mod_prop, forecast_prop,xlab="Years",ylab="Price")
      plot(mod_prop, forecast_prop, main="Prophet Forecasts", xlab="Month", ylab="Price($)") + ggtitle("Prophet")
      
      
      
    }
    
  }, bg = "White")
  
  
  
}
shinyApp(ui, server)