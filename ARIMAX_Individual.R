init_run()
set.seed(72)

flow_data<-read_data("Jakarta","K50000") %>% add_eidulfitr_regressor()
flow_data_xts <- ts(flow_data[,3:4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

train_test_data<-ts_split(flow_data_xts,sample.out=12)

arima.model<-auto.arima(train_test_data$train[,1],xreg =train_test_data$train[,2] )
arima.model2<-auto.arima(train_test_data$train[,1],xreg =train_test_data$train[,2],
                         lambda = "auto")

result<-ts.intersect(train_test_data$train[,1],arima.model2$fitted)
colnames(result)<-c("train_data","arima_fitted")

mape(result[,1],result[,2])

