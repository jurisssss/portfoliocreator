deciles_value_numbers <- function(x, y, z, start="2002-07-01", end="2003-06-01"){
  MV_data <- as.matrix(x[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)])
  stock_price_data <- as.matrix(y[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)*12+1)])
  somedata_data <- as.matrix(z[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)])

  # calculate deciles
  somedata_deciles <- as.matrix(apply(somedata_data, 2, quantile, probs = seq(.1,.9, by=.1), na.rm = TRUE))

  # Erstellen von Dataset aus somedata und Returns
  data_MV_somedata_returns <- cbind(MV_data, somedata_data, stock_price_data)
  data_MV_somedata_returns <- apply(data_MV_somedata_returns, 2, as.numeric)


  ## somedata Rating zu NA wenn zugeh???rige Market cap = NA
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    data_MV_somedata_returns[which(is.na(data_MV_somedata_returns[,i])),(i+(length(seq(as.Date(start), as.Date(end), by = "month",))/12))] <- NA
  }

  # calculate deciles
  somedata_deciles <- as.matrix(apply(data_MV_somedata_returns[,((length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1):(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))], 2, quantile, probs = seq(.1,.9, by=.1), na.rm = TRUE))

  #Decile Portfolios
  output <- list()
  output1 <- list()
  for (i in 0:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)-1)) {
    output[[1]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)] <= somedata_deciles[1,(i+1)], select = c((i+1), (i+3), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+12)))
    for (k in 2:9) {
      output[[k]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)]<= somedata_deciles[k,(i+1)] & data_MV_somedata_returns[,(i+(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)] > somedata_deciles[(k-1),(i+1)], select = c((i+1), (i+3), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+12)))
    }
    output[[10]] <- subset(data_MV_somedata_returns, data_MV_somedata_returns[,(i+(length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)] > somedata_deciles[9,(i+1)], select = c((i+1), (i+3), (i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+1):(i*12+(2*(length(seq(as.Date(start), as.Date(end), by = "month",))/12))+12)))
    output1[[i+1]] <- output
  }

  numberofstocks <- list()
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    numberofstocks[[i]] <- as.data.frame(rapply(output1[[i]], nrow, how = "list"))
  }

  return(numberofstocks)
}
