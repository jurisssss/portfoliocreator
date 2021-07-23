deciles_value <- function(x, y, z, start="2002-07-01", end="2003-06-01"){
  MV_data <- as.matrix(x[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)])
  stock_price_data <- as.matrix(y[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)*12+1)])
  some_data <- as.matrix(z[,2:((length(seq(as.Date(start), as.Date(end), by = "month",))/12)+1)])

  # calculate deciles
  somedata_deciles <- as.matrix(apply(some_data, 2, quantile, probs = seq(.1,.9, by=.1), na.rm = TRUE))

  # Erstellen von Dataset aus MV, somedata und Returns
  data_MV_somedata_returns <- cbind(MV_data, some_data, stock_price_data)
  data_MV_somedata_returns <- apply(data_MV_somedata_returns, 2, as.numeric)


  ## somedata zu NA wenn zugeh�rige Market cap = NA
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

  a <- list()
  a1 <- list()
  MV_proportional <- list()
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    for (k in 1:10) {
      MV_proportional[[k]] <- output1[[i]][[k]] [,1]/sum(output1[[i]] [[k]] [,1])
    }
    a1[[i]] <- MV_proportional
  }

  d <- append(a1,output1)
  a <- list()
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    a[[i]] <- mapply(cbind, d[[i]], d[[i+(length(seq(as.Date(start), as.Date(end), by = "month",))/12)]], SIMPLIFY = FALSE)
  }

  value1 <- list()
  value <- list()
  value_weighted_returns <- list()
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    for (j in 1:10) {
      for (k in 1:12) {
        value[[k]] <- a[[i]][[j]][,1]*a[[i]][[j]][,k+3]
      }
      value1[[j]] <- value
    }
    value_weighted_returns[[i]] <- value1
  }

  col_sums3 <- list()
  col_sums2 <- list()
  col_sums <- list()
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    for (j in 1:10) {
      for (k in 1:12) {
        col_sums[[k]] <- sum(value_weighted_returns[[i]][[j]][[k]], na.rm = TRUE)
      }
      col_sums2[[j]] <- col_sums
    }
    col_sums3[[i]] <- col_sums2
  }

  list_empty2 <- list()
  list_empty <- list()
  for (i in 1:(length(seq(as.Date(start), as.Date(end), by = "month",))/12)) {
    for (j in 1:10) {
      list_empty[[j]] <- do.call(rbind, col_sums3[[i]][[j]])
    }
    list_empty2[[i]] <- list_empty
  }

  last_list <- list()
  for (i in 1:10) {
    last_list[[i]] <- rbind(lapply(list_empty2, `[[`,i))
  }

  last_list2<- list()
  for (k in 1:10) {
    last_list2[[k]] <- data.frame(matrix(unlist(last_list[[k]])))
  }

  f <- do.call(cbind, last_list2)
  names(f) <- c("Decile 1", "Decile 2", "Decile 3", "Decile 4", "Decile 5", "Decile 6", "Decile 7", "Decile 8", "Decile 9", "Decile 10")
  f_simple <- data.frame(f, row.names = seq(as.Date(start), as.Date(end), by = "month"))


  return(f_simple)
}
