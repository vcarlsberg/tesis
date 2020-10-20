init_run()

for (pecahan in c("K100000","K50000","K20000","K10000","K5000"))
{
  flow_data<-read_data("Jakarta",pecahan)
  flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                      frequency=12)
  
  train_test_data<-ts_split(flow_data_xts,sample.out=20)
  
  mlp.model<-mlp(as.ts(train_test_data$train),hd=c(10,8),reps = 1,lags = 1:60)
  
  residual<-train_test_data$train-mlp.model$fitted
  
  arima.model<-auto.arima(residual)
  arima.model2<-auto.arima(residual,lambda = "auto")
  
  
  result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
  colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  
  print(paste0("in-sample MAPE : ",mape(result[,1],result[,2]+result[,3])))
  
}
