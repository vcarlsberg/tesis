source("all_function.R")
init_run()
set.seed(72)

for(location in c("Jakarta"))
{
  for(denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    flow_data<-read_data(location,denomination)
    
    
    flow_data_xts <- ts(flow_data[,3],
                        start=c(flow_data[1,1], flow_data[1,2]), 
                        end=c(2019, 6),frequency = 12)
    
    train_test_data<-ts_split(flow_data_xts)
    
    arima.model<-auto.arima(train_test_data$train,seasonal = FALSE)
    arima.model2<-auto.arima(train_test_data$train,lambda = "auto",seasonal = FALSE)
    
    residual<-train_test_data$train-arima.model$fitted
    
    mlp.model<-mlp(as.ts(residual),reps = 1,hd = c(10,8,5),lags = 1:60)
    
    result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
    colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
    
    print(paste0("in-sample MAPE : ",mape(result[,1],result[,2]+result[,3])))
    
    for (fh in 1:24)
    {
      frc.arima<-forecast(arima.model,h=fh)
      frc.mlp<-forecast(mlp.model,h=fh)
      
      result.pred<-ts.intersect(train_test_data$test[1:fh],frc.arima$mean,frc.mlp$mean)
      colnames(result.pred)<-c("test_data","arima_pred","mlp_pred")
      
      print(paste0("Out-sample error : ",location," ",denomination," ",fh," ",
                   mape(result.pred[,1],result.pred[,2]+result.pred[,3])))
    }
    
    
  }
}
