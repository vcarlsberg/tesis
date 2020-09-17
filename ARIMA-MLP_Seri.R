init_run()

pecahan<-"L1000"
flow_data<-read_data("Surabaya","K100000")

index<-flow_data[pecahan]==0
flow_data[pecahan][index]<-NA
na.omit(na.approx(flow_data))

flow_data<-flow_data %>% mutate(L1000 = ifelse(L1000 == 0,NA,L1000))
data_outflow<-na.omit(na.approx(flow_data))
data_outflow<-as.data.frame(flow_data)

flow_data[,3]<-ifelse(flow_data[,3]==0.000,NA,flow_data[,3])



flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)
  
train_test_data<-ts_split(flow_data_xts,sample.out=20)

arima.model<-auto.arima(train_test_data$train)
arima.model2<-auto.arima(train_test_data$train,lambda = "auto")
  
residual<-train_test_data$train-arima.model$fitted

mlp.model<-mlp(as.ts(residual),hd=c(10,8,5),reps = 1,lags = 1:60)

result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  
print(paste0("in-sample MAPE : ",mape(result[,1],result[,2]+result[,3])))
  


