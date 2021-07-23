Market_portfolio_value <- function(x, y, start="2002-07-01", end="2003-06-01"){
  MV_data <- as.matrix(x[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)])
  stock_price_data <- as.matrix(y[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)*12+1)])

  # Erstellen von Dataset
  data_MV_returns <- cbind(MV_data, stock_price_data)
  data_MV_returns <- apply(data_MV_returns, 2, as.numeric)

  output <- list()
  for (i in 0:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)-1)) {
    output[[i+1]] <- subset(data_MV_returns, !is.na(data_MV_returns[,i+1]), select = c((i+1), (i*12+(length(seq(as.Date(start),
                                                                                                                as.Date(end), by = "month",))/12)+1):(i*12+((length(seq(as.Date(start), as.Date(end), by = "month",))/12)+12))))
  }

  a <- list()
  MV_proportional <- list()
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    MV_proportional[[i]] <- output[[i]][,1]/sum(output[[i]] [,1])
    a[[i]] <- cbind(MV_proportional[[i]], output[[i]])
  }

  value <- list()
  value_weighted_returns <- list()
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    for (k in 1:12) {
      value[[k]] <- a[[i]][,1]*a[[i]][,k+2]
    }
    value_weighted_returns[[i]] <- as.matrix(as.data.frame(value))
  }

  col_sums <- list()
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    col_sums[[i]] <- colSums(value_weighted_returns[[i]], na.rm = TRUE)
  }

  market <- as.matrix(unlist(col_sums))
  dates <- as.matrix(seq(as.Date(start), as.Date(end), by = "month",))
  market_simple <- data.frame(market, row.names = seq(as.Date(start), as.Date(end), by = "month"))

  return(market_simple)
}
