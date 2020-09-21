source("all_function.R")
init_run()
set.seed(72)

for(location in c("Jakarta"))
{
  for(denomination in c("K100000"))
  {
    flow_data<-read_data(location,denomination)
    flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                        frequency=12)
    
    train_test_data<-ts_split(flow_data_xts)
    
    arima.model<-auto.arima(train_test_data$train)
    arima.model2<-auto.arima(train_test_data$train,
                             lambda = "auto")
    
    result<-ts.intersect(train_test_data$train,arima.model$fitted)
    colnames(result)<-c("train_data","arima_fitted")
    
    print(paste0("in-sample MAPE : ",location," ",denomination," ",mape(result[,1],result[,2])))
    
    for (fh in 1:24) {
      frc.arima<-forecast(arima.model,h=fh)
      result.pred<-ts.intersect(train_test_data$test,frc.arima$mean)
      colnames(result.pred)<-c("test_data","arima_fitted")
      
      print(paste0("out-sample MAPE : ",location," ",denomination," ",fh," ",mape(result.pred[,1],result.pred[,2])))
    }
    
  }
}






