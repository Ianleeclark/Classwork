generate_graph <- function(symbol) {
  # Function to retrieve a company's stock data via quantmod (getSymbol())
  # and then graph using a candleChart()
  #
  # Args: 
  #   x: Company's symbol (GOOGL, MON, &c.)
  #
  # Returns:
  #   nothing?
  if(!require("quantmod")) {
    library(quantmod)
  }
  
  getSymbol("MON")
  candleChart(MON, TA=NULL)  
}
