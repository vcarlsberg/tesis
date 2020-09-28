source("all_function.R")
init_run()

set.seed(72)

for(location in c("Jakarta"))
{
  for(denomination in c("K100000"))
  {
    flow_data<-read_data(location,denomination)
    
    flow_data_xts <- ts(flow_data[,3:4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                        frequency=12)
    
    train_test_data<-split_data(flow_data_xts,20)
    
    arima.model<-auto.arima(train_test_data$train[,1],xreg =train_test_data$train[,2])
    #arima.model2<-auto.arima(train_test_data$train,lambda = "auto",xreg =EIDULFITR)
    
    residual<-train_test_data$train[,1]-arima.model$fitted
    
    mlp.model<-mlp(as.ts(residual),hd=c(10,8,5),reps=1,
                   xreg =as.data.frame(flow_data_xts[,2]),lags = 1:60,
                   xreg.lags=list(0),xreg.keep=list(TRUE))
    
    result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
    colnames(result)<-c("train_data","external_regressors","mlp_fitted","arima_fitted")
    
    print(paste0("in-sample MAPE : ",mape(result[,1],0.5*result[,2]+0.5*result[,3])))
    
    for (fh in 1:24)
    {
      frc.arima<-forecast(arima.model,h=fh,xreg = train_test_data$test[,2][1:fh])
      frc.mlp<-forecast(mlp.model,h=fh,
                        xreg = as.data.frame(flow_data_xts[,2]))
      
      result.pred<-ts.intersect(train_test_data$test[1:fh],frc.arima$mean,frc.mlp$mean)
      colnames(result.pred)<-c("test_data","arima_pred","mlp_pred")
      
      print(paste0("Out-sample error : ",location," ",denomination," ",fh," ",
                   mape(result.pred[,1],0.5*result.pred[,2]+0.5*result.pred[,3])))
    }
    
  }
}

