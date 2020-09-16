init_run()

set.seed(72)

flow_data<-read_data("Jakarta","K50000")
flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                   frequency=12)

train_test_data<-ts_split(flow_data_xts,sample.out=20)

arima.model<-auto.arima(train_test_data$train)
arima.model2<-auto.arima(train_test_data$train,lambda = "auto")

mlp.model<-mlp(as.ts(train_test_data$train),hd=c(10,8),reps = 1,lags = 1:60)

result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model2$fitted)
colnames(result)<-c("train_data","mlp_fitted","arima_fitted")

mape(result[,1],(0.5*result[,2]+0.5*result[,3]))

