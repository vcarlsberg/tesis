library(xts)
library(forecast)
library(nnfor)
library(TSrepr)

flow_data<-read_data("Jakarta","K50000")
flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                   frequency=12)

train_data<-window(flow_data_xts,1994,c(2018,12))
test_data<-window(flow_data_xts,2019)

arima.model<-auto.arima(train_data)
arima.model2<-auto.arima(train_data,lambda = "auto")

mlp.model<-mlp(as.ts(train_data),hd=c(5,7,8,3))

length(mlp.model$fitted)
length(train_data)

result<-ts.intersect(train_data,mlp.model$fitted,arima.model2$fitted)
colnames(result)<-c("train_data","mlp_fitted","arima_fitted")

mape(result[,1],(0.5*result[,2]+0.5*result[,3]))

