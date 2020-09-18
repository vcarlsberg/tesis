init_run()
set.seed(72)

flow_data<-read_data("Jakarta","K50000") %>% add_eidulfitr_regressor()
summary(flow_data)
plot(density(flow_data[,3]))

flow_data_xts <- ts(flow_data[,3:4],
                    start=c(flow_data[1,1], flow_data[1,2]), 
                    end=c(2019, 6),frequency = 12)
  
train_test_data<-ts_split(flow_data_xts)

arima.model<-auto.arima(train_test_data$train,seasonal = FALSE)
arima.model2<-auto.arima(train_test_data$train,lambda = "auto",seasonal = FALSE)
  
residual<-train_test_data$train-arima.model2$fitted

mlp.model<-mlp(as.ts(residual),reps = 1,hd = c(10,8,5),lags = 1:60)
summary(mlp.model)
plot(mlp.model)

result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  
print(paste0("in-sample MAPE : ",mape(result[,1],result[,2]+result[,3])))
  

