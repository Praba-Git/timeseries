library(quantmod)
library(shiny)
library(fpp3)

#refer to https://cran.r-project.org/web/packages/rugarch/rugarch.pdf
#also refer to https://users.metu.edu.tr/home402/ozancan/wwwhome/ARCHGARCHTutorial.html
#this is for the GARCH model
library(rugarch)

ui <- fluidPage(
  
  # browser  title
  title = "Stock/ETF Analysis and Predictions",
  # Application title
  titlePanel(div("Stock/ETF Analysis and Predictions", style = "color: #FFFFFF;font-size:25px;font-style: italic;")),
  
  tags$style('.container-fluid {background-color:black;}'),
  
  # Sidebar for stocks
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="tabset",
        #Creates the options in the drop-down menu for Stocks and ETFs
        tabPanel("Stocks",   selectInput("Stock", "Stock", choices = c("AAPL", "TSLA", "MSFT"), width = "100px")),
        tabPanel("ETF", selectInput("ETF", "ETF/Sector", choices = c("VTI", "VOO", "VUG"), width = "100px"))
      ),
      
      # Date range
      dateRangeInput("date_range", "Date range:", start = Sys.Date()-120, end = Sys.Date(),width="250px",format = "yyyy-mm-dd"),
     
       # days for prediction ahead
      div(style = "white-space: nowrap;", 
          numericInput("h", "Days to Predict", value =3,width = "65px")
      ),
      
      # add options for selecting a prediction method
      radioButtons("model", "Forecasting Model Applied",
                   choices = c("naive", "ARIMA", "GARCH","Prophet","K-NN","FF-NN"),
                   choiceValues = "ARIMA")
     
    
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


    #Show a plot of the ticker historical distribution
    output$qm_plot <- renderPlot({
      
      #if the tab 'Stocks' is selected, than the input of 'Stock' defined earlier becomes the ticker
      if(input$tabset =="Stocks")
      {
        ticker = input$Stock
      }
      #if the tab is not 'Stocks', then the remaining tab must be 'ETF'
      else
      {
        #The selected ETF on the 'ETF' tab is the selected input, and therefore now assigned as ticker
        ticker = input$ETF
      }      
      #Creates a new environment 
      stockEnv <- new.env()
      
      #calls data from Yahoo Finance
      getSymbols(ticker, from=input$date_range[1],to=input$date_range[2], env=stockEnv)
      
      #stores the list available in this environment as a list
      stock = ls(stockEnv)
      
      #get stock data and creates a chart
      chartSeries(stockEnv[[ls(stockEnv)]], name = ticker, theme=chartTheme('white')) 
      
      
      #This Does not seem to print anything..
      MACD2plot_plus   = which(diff(sign(MACD(Cl(stockEnv[[stock]]))[,1])) > 0)
      MACD2plot_minus  = which(diff(sign(MACD(Cl(stockEnv[[stock]]))[,1])) < 0)
      range_y          = 0.5
      print(addPoints(MACD2plot_plus,  stockEnv[[stock]][MACD2plot_plus,  4] - range_y, pch=24, col='blue', offset=1.0))
      print(addPoints(MACD2plot_minus, stockEnv[[stock]][MACD2plot_minus, 4] + range_y, pch=25, col='red',  offset=1.0))
      
      #adds moving average
      print(addSMA(n=15, on=1, col = "blue"))
      print(addSMA(n=30, on=1, col = "red"))
      print(addSMA(n=50, on=1, col = "purple"))
      
    
    })

    # Section to display forecast models
    output$qm_plot1 <- renderPlot({
      
      if(input$tabset =="Stocks")
      {
        ticker = input$Stock
      }
      else
      {
        ticker = input$ETF
      }
     
      #variable h is 'days to predict' value
      stock <- getSymbols(ticker,src="yahoo",from=input$date_range[1],to=input$date_range[2],env=NULL)
      n <- input$h
      end = dim(stock)[1]
      start = 1 #end - 1
      
      #paste adds things together, like strings or variable names.
      close_price = paste(ticker,".Close",sep="")
      
      if (input$model == "naive"){
      
        mod_naive <- naive(stock[start : end, close_price])
        # Plot the result
        autoplot(forecast(mod_naive, h = n)) + labs(y = paste(ticker," Price($)"), x = " Time (days)")
    
      } else if (input$model == "ARIMA"){
        
        
        # Create the Model
        mod_arima <- auto.arima(stock[start : end, close_price], seasonal=FALSE, stepwise = FALSE, max.p = 4, max.d = 2, max.q = 4)
        # Plot the result
        autoplot(forecast(mod_arima, h = n)) +
          labs(y = paste(ticker," Price($)"), x = " Time (days)")
        
      } else if (input$model == "GARCH") {
        
       
        garch_spec <- ugarchspec(variance.model=list(model="sGARCH", 
                                                     garchOrder=c(1,1)))
        
        
        fit_garch <- ugarchfit(spec = garch_spec, data = stock[start : end, close_price], method = "ML")
        
        garch_forecast <- ugarchforecast(fit_garch, n.ahead = n)
        
        plot(garch_forecast,  which = 1)
        
        
      } else if (input$model == "K-NN") {
        
       mod <- nnetar(stock[start : end, close_price])
        # Plot the result
      autoplot(forecast(mod, h = n)) +
      labs(y = paste(ticker," Price($)"), x = " Time (days)")
        
      }
      
    })    

}
shinyApp(ui, server)