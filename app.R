library(quantmod)
library(shiny)


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
        tabPanel("Stocks",   selectInput("Stock", "Stock", choices = c("AAPL", "TSLA", "MSFT"), width = "100px")),
        tabPanel("ETF", selectInput("ETF", "ETF/Sector", choices = c("VTI", "VOO", "VUG"), width = "100px"))
      ),
      
      # Date range
      dateRangeInput("date_range", "Date range:", start = Sys.Date()-60, end = Sys.Date()-7,width="250px",format = "yyyy-mm-dd"),
     
       # days for prediction ahead
      div(style = "white-space: nowrap;", 
          numericInput("h", "Days to Predict", value = 10,width = "50px")
      ),
      
      # add options for selecting a prediction method
      radioButtons("model", "Forecasting Model Applied",
                   choices = c("naive", "ARIMA", "GARCH","Prophet","K-NN","FF-NN"),
                   choiceValues = "ARIMA")
     
    ),
   
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("qm_plot")
    )
  )
)

server <- function(input, output, session){

    output$qm_plot <- renderPlot({
    
    if(input$tabset =="Stocks")
    {
      ticker = input$Stock
    }
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

}
shinyApp(ui, server)