init_run()
set.seed(72)

for(location in c("Jakarta"))
{
  for(denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    flow_data<-read_data(location,denomination)
    flow_data_xts <- ts(flow_data[,3:4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                        frequency=12)
    
    train_test_data<-ts_split(flow_data_xts,sample.out = 20)
    
    mlp.model<-mlp(train_test_data$train[,1],hd=c(10,8,5),
                   reps = 1,
                   lags = 1:60,
                   xreg = as.data.frame(flow_data_xts[,2]))
    
    result<-ts.intersect(train_test_data$train,mlp.model$fitted)
    colnames(result)<-c("train_data","external_regressor","mlp_fitted")
    
    print(paste0("in-sample MAPE : ",location," ",denomination," ",mape(result[,1],result[,3])))
    
    
    frc.mlp<-forecast(mlp.model,h=20,xreg = as.data.frame(flow_data_xts[,2]))
    
    result.pred<-ts.intersect(train_test_data$test,frc.mlp$mean)
    colnames(result.pred)<-c("test_data","external_regressors","mlp_fitted")
    
    print(paste0("out-sample MAPE : ",location," ",denomination," ",
                 mape(result.pred[,1],result.pred[,3])))
  }
}




