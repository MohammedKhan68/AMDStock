library(quantmod)

getSymbols(Symbols = "AMD", src = "yahoo", from = Sys.Date()-365, auto.assign = TRUE ) #Auto.assign creates a variable automatically.

AMD

nrow(AMD)

#Now lets calculate 200 day moving average using TTR Package

library(TTR)

AMD$MAA <- SMA(AMD$AMD.Close, n=200)

AMD$MAA

#For 50 day MAA 
AMD$MAA50 <- SMA(AMD$AMD.Close, n = 50)

#Now lets create live stock price and MA chart

library(shiny)
library(ggplot2)


ui <- fluidPage(titlePanel("AMD Stock Price Alert"),
                plotOutput("StockChart"))

server <- function(input, output){
  output$StockChart <- renderPlot({
    getSymbols(Symbols = "AMD", src = "yahoo", from = Sys.Date()-365, auto.assign = TRUE )
    AMD$MAA <- SMA(AMD$AMD.Close, n=200)
    AMD$MAA50 <- SMA(AMD$AMD.Close, n = 50)
    
    data <- data.frame(Date=index(AMD), Price=AMD$AMD.Close, MA200=AMD$MAA, MA50 = AMD$MAA50 )
    ggplot(data, aes(x=Date))+
      geom_line(aes(y=Price, color = "Stock Price"), size = 1.5, color = "blue")+
      geom_line(aes(y=MA200, color = "200-day-MAA"), size = 1.5)+
      geom_line(aes(y=MA50, color = "50-day-MAA"), size = 1.5)+
      labs(y= "Price", color="legend")+
      theme(axis.text.x = element_text(face="bold", size = 15))+
      theme(axis.text.y = element_text(face="bold", size = 15))+
      theme(axis.title = element_text(face="bold"), size=15)
     
  })
}

shinyApp(ui=ui, server=server)

data

AMD

