init_run()
set.seed(72)

flow_data<-read_data("Jakarta","K100000")

flow_data_xts <- ts(flow_data[,3],
                    start=c(flow_data[1,1], flow_data[1,2]), 
                    end=c(2019, 6),frequency = 12)
  
train_test_data<-ts_split(flow_data_xts,sample.out = 20)

arima.model<-auto.arima(train_test_data$train,seasonal = FALSE)
arima.model2<-auto.arima(train_test_data$train,lambda = "auto",seasonal = FALSE)
  
residual<-train_test_data$train-arima.model2$fitted

mlp.model<-mlp(as.ts(residual),reps = 1,hd = c(10,8,5),lags = 1:60)

result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  
print(paste0("in-sample MAPE : ",mape(result[,1],result[,2]+result[,3])))
  
frc.arima<-forecast(arima.model2,h=20)
frc.mlp<-forecast(mlp.model,h=20)
print(paste0("out-sample MAPE : ",mape(train_test_data$test,frc.arima$mean+frc.mlp$mean)))
