long_short_value <- function(x, y, z, somedata_lower_bound=0.3, somedata_upper_bound=0.7, start="2002-07-01", end="2003-06-01"){
  MV_data <- as.matrix(x[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)])
  stock_price_data <- as.matrix(y[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)*12+1)])
  somedata_data <- as.matrix(z[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)])

  # Median Market Value und High minus low f?r somedata
  somedata_percentiles <- as.matrix(apply(somedata_data, 2, quantile, probs = c(somedata_lower_bound,somedata_upper_bound), na.rm = TRUE))
  medians_MV <- as.matrix(apply(MV_data, 2, median, na.rm=TRUE))

  #Erstellen von dataset aus MV, somedata und simple returns
  data_MV_somedata_returns <- cbind(MV_data, somedata_data, stock_price_data)
  data_MV_somedata_returns <- apply(data_MV_somedata_returns, 2, as.numeric)

  #Kreieren von Faktoren
  #Big/High
  output <- list()
  for (i in 0:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)-1)) {
    output[[(i+1)]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+1)]>medians_MV[(i+1)] & data_MV_somedata_returns[,(i+1+(length(seq(as.Date(start), as.Date(end), by = "month",))/12))]>somedata_percentiles[2,(i+1)], select = c((i+1), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+12))))
  }

  a <- list()
  MV_proportional <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    MV_proportional[[i]] <- output[[i]][,1]/sum(output[[i]] [,1])
    a[[i]] <- cbind(MV_proportional[[i]], output[[i]])
  }
  value <- list()
  value_weighted_returns <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    for (k in 1:12) {
      value[[k]] <- a[[i]][,1]*a[[i]][,k+2]
    }
    value_weighted_returns[[i]] <- as.matrix(as.data.frame(value))
  }

  col_sums <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    col_sums[[i]] <- colSums(value_weighted_returns[[i]], na.rm = TRUE)
  }
  Big_High <- as.matrix(unlist(col_sums))
  dates <- as.matrix(seq(as.Date(start), as.Date(end), by = "month",))
  Big_High_simple <- data.frame(Big_High, row.names = seq(as.Date(start), as.Date(end), by = "month"))
  Big_High_log <- log(Big_High_simple +1)

  ################
  #Big/Low

  output <- list()
  for (i in 0:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)-1)) {
    output[[(i+1)]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+1)]>medians_MV[(i+1)] & data_MV_somedata_returns[,(i+1+(length(seq(as.Date(start), as.Date(end), by = "month",))/12))]<somedata_percentiles[1,(i+1)], select = c((i+1), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+12))))
  }

  a <- list()
  MV_proportional <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    MV_proportional[[i]] <- output[[i]][,1]/sum(output[[i]] [,1])
    a[[i]] <- cbind(MV_proportional[[i]], output[[i]])
  }
  value <- list()
  value_weighted_returns <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    for (k in 1:12) {
      value[[k]] <- a[[i]][,1]*a[[i]][,k+2]
    }
    value_weighted_returns[[i]] <- as.matrix(as.data.frame(value))
  }

  col_sums <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    col_sums[[i]] <- colSums(value_weighted_returns[[i]], na.rm = TRUE)
  }
  Big_Low <- as.matrix(unlist(col_sums))
  dates <- as.matrix(seq(as.Date(start), as.Date(end), by = "month",))
  Big_Low_simple <- data.frame(Big_Low, row.names = seq(as.Date(start), as.Date(end), by = "month"))
  Big_Low_log <- log(Big_Low_simple +1)


  ################
  #Small/High

  output <- list()
  for (i in 0:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)-1)) {
    output[[(i+1)]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+1)]<medians_MV[(i+1)] & data_MV_somedata_returns[,(i+1+(length(seq(as.Date(start), as.Date(end), by = "month",))/12))]>somedata_percentiles[2,(i+1)], select = c((i+1), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+12))))
  }

  a <- list()
  MV_proportional <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    MV_proportional[[i]] <- output[[i]][,1]/sum(output[[i]] [,1])
    a[[i]] <- cbind(MV_proportional[[i]], output[[i]])
  }
  value <- list()
  value_weighted_returns <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    for (k in 1:12) {
      value[[k]] <- a[[i]][,1]*a[[i]][,k+2]
    }
    value_weighted_returns[[i]] <- as.matrix(as.data.frame(value))
  }

  col_sums <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    col_sums[[i]] <- colSums(value_weighted_returns[[i]], na.rm = TRUE)
  }
  Small_High <- as.matrix(unlist(col_sums))
  dates <- as.matrix(seq(as.Date(start), as.Date(end), by = "month",))
  Small_High_simple <- data.frame(Small_High, row.names = seq(as.Date(start), as.Date(end), by = "month"))
  Small_High_log <- log(Small_High_simple +1)


  ################
  #Small/Low

  output <- list()
  for (i in 0:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)-1)) {
    output[[(i+1)]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+1)]<medians_MV[(i+1)] & data_MV_somedata_returns[,(i+1+(length(seq(as.Date(start), as.Date(end), by = "month",))/12))]<somedata_percentiles[1,(i+1)], select = c((i+1), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+12))))
  }

  a <- list()
  MV_proportional <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    MV_proportional[[i]] <- output[[i]][,1]/sum(output[[i]] [,1])
    a[[i]] <- cbind(MV_proportional[[i]], output[[i]])
  }
  value <- list()
  value_weighted_returns <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    for (k in 1:12) {
      value[[k]] <- a[[i]][,1]*a[[i]][,k+2]
    }
    value_weighted_returns[[i]] <- as.matrix(as.data.frame(value))
  }

  col_sums <- list()
  for (i in 1:((length(seq(as.Date(start), as.Date(end), by = "month",))/12))) {
    col_sums[[i]] <- colSums(value_weighted_returns[[i]], na.rm = TRUE)
  }
  Small_Low <- as.matrix(unlist(col_sums))
  dates <- as.matrix(seq(as.Date(start), as.Date(end), by = "month",))
  Small_Low_simple <- data.frame(Small_Low, row.names = seq(as.Date(start), as.Date(end), by = "month"))
  Small_Low_log <- log(Small_Low_simple +1)


  LowminusHigh <- 0.5*(Big_Low_simple+Small_Low_simple)-(0.5*(Big_High_simple+Small_High_simple))
  names(LowminusHigh) <- "LowminusHigh"

  Returns <- cbind(Big_Low_simple, Small_Low_simple, Big_High_simple, Small_High_simple, LowminusHigh)

  return(Returns)
}
