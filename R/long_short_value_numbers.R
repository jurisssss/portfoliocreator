long_short_value_numbers <- function(x, y, z, somedata_lower_bound=0.3, somedata_upper_bound=0.7, start="2002-07-01", end="2003-06-01"){
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

  numberofstocks1 <- as.data.frame(rapply(output, nrow, how = "list"))
  names(numberofstocks1) <- paste(seq(substr(start, start = 1, stop = 4), as.numeric(substr(end, start=1, stop=4))-1), "/",
                                  seq(as.numeric(substr(start, start = 1, stop = 4))+1, substr(end, start = 1, stop = 4)))


  ################
  #Big/Low

  output <- list()
  for (i in 0:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)-1)) {
    output[[(i+1)]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+1)]>medians_MV[(i+1)] & data_MV_somedata_returns[,(i+1+(length(seq(as.Date(start), as.Date(end), by = "month",))/12))]<somedata_percentiles[1,(i+1)], select = c((i+1), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+12))))
  }

  numberofstocks2 <- as.data.frame(rapply(output, nrow, how = "list"))
  names(numberofstocks2) <- paste(seq(substr(start, start = 1, stop = 4), as.numeric(substr(end, start=1, stop=4))-1), "/",
                                  seq(as.numeric(substr(start, start = 1, stop = 4))+1, substr(end, start = 1, stop = 4)))


  ################
  #Small/High

  output <- list()
  for (i in 0:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)-1)) {
    output[[(i+1)]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+1)]<medians_MV[(i+1)] & data_MV_somedata_returns[,(i+1+(length(seq(as.Date(start), as.Date(end), by = "month",))/12))]>somedata_percentiles[2,(i+1)], select = c((i+1), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+12))))
  }

  numberofstocks3 <- as.data.frame(rapply(output, nrow, how = "list"))
  names(numberofstocks3) <- paste(seq(substr(start, start = 1, stop = 4), as.numeric(substr(end, start=1, stop=4))-1), "/",
                                  seq(as.numeric(substr(start, start = 1, stop = 4))+1, substr(end, start = 1, stop = 4)))

  ################
  #Small/Low

  output <- list()
  for (i in 0:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)-1)) {
    output[[(i+1)]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+1)]<medians_MV[(i+1)] & data_MV_somedata_returns[,(i+1+(length(seq(as.Date(start), as.Date(end), by = "month",))/12))]<somedata_percentiles[1,(i+1)], select = c((i+1), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+12))))
  }

  numberofstocks4 <- as.data.frame(rapply(output, nrow, how = "list"))
  names(numberofstocks4) <- paste(seq(substr(start, start = 1, stop = 4), as.numeric(substr(end, start=1, stop=4))-1), "/",
                                  seq(as.numeric(substr(start, start = 1, stop = 4))+1, substr(end, start = 1, stop = 4)))


  Number_of_Stocks <- cbind(numberofstocks1, numberofstocks2, numberofstocks3, numberofstocks4)



  return(Number_of_Stocks)
}
