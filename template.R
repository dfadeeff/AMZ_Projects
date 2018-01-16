
rm(list = ls())

library(ggplot2)
library(forecast)
library(tseries)

mydata = read.table(file = "../../../../adhoc_queries/new/data/weekly_stats.txt",sep='\t', header = F)


generate_Fourier_ARIMA_forecast <- function(container, column, marketplace, horizon) {
  
  #function produces prediction for marketplace using Fourier and ARIMA model
  
  colnames(container) <- c("total_customers","new_customers",'ops','units','cogs','market','date')
  container$date = as.Date(container$date, format = "%Y-%m-%d")
  container$returning_customers <- container$total_customers - container$new_customers
  
  #Subset to get marketplace
  container <- container[ (container$market == marketplace), ]
  container <- container[ (container$date < Sys.Date() - 3), ]
  
  ggplot(data = container,aes(x = container$date, y = container$total_customers)) + geom_line() 
  container <- container[(container$date > '2014-09-27'),] 
  
  #check sorting
  container <- container[order(container$date),]
  
  #Initialise dependent var
  z = which(colnames(container) == column)[1]
  y = container[,z]
  
  #Create clean data series
  count_ts = ts(data = container[,z])
  container$clean_total <- count_ts
  
  h = horizon
  training <- window(container$clean_total, end = length(container$clean_total) - h)
  test <- window(container$clean_total, start = length(container$clean_total) - h + 1)
  count_bats <- tbats(ts(log10(training),freq=365.25/7)) #Here, important!
  pred <- forecast(count_bats, h=h)
  
  
  return(pred)
}

generate_preprocessed_dataframe <- function(container, column, marketplace, horizon) {
  
  #function produces prediction for marketplace using Fourier and ARIMA model
  
  colnames(container) <- c("total_customers","new_customers",'ops','units','cogs','market','date')
  container$date = as.Date(container$date, format = "%Y-%m-%d")
  container$returning_customers <- container$total_customers - container$new_customers
  
  #Subset to get marketplace
  container <- container[ (container$market == marketplace), ]
  container <- container[ (container$date < Sys.Date() - 3), ]
  
  ggplot(data = container,aes(x = container$date, y = container$total_customers)) + geom_line() 
  container <- container[(container$date > '2014-09-27'),] 
  
  #check sorting
  container <- container[order(container$date),]
  
  #Initialise dependent var
  z = which(colnames(container) == column)[1]
  y = container[,z]
  
  #Create clean data series
  count_ts = ts(data = container[,z])
  container$clean_total <- count_ts
    
  return(container)
}


pred = generate_Fourier_ARIMA_forecast(mydata,"new_customers",4,50)
mydata = generate_preprocessed_dataframe(mydata,"new_customers",4,50)


#Plot the prediction
plot(pred)

#Plot everything
autoplot(10^(pred$mean)) + autolayer(ts(mydata$clean_total,freq=365.25/7)) + autolayer(10^(pred$fitted))
