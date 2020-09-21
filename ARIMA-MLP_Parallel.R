init_run()

set.seed(72)
location<-"Jakarta"
denomination<-"K50000"

for(location in c("Jakarta"))
{
  for(denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    flow_data<-read_data(location,denomination)
    flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                        frequency=12)
    
    train_test_data<-ts_split(flow_data_xts,sample.out=20)
    
    arima.model<-auto.arima(train_test_data$train)
    arima.model2<-auto.arima(train_test_data$train,lambda = "auto")
    
    mlp.model<-mlp(as.ts(train_test_data$train),hd=c(10,8,5),reps = 1,lags = 1:60)
    
    result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
    colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
    
    print(paste0("In-sample error : ",location," ",denomination," ",mape(result[,1],(0.5*result[,2]+0.5*result[,3]))))
    
    frc.arima<-forecast(arima.model2,h=20)
    frc.mlp<-forecast(mlp.model,h=20)
    
    print(paste0("Out-sample error : ",location," ",denomination," ",mape(train_test_data$test,0.5*frc.arima$mean+0.5*frc.mlp$mean)))
  }
}


