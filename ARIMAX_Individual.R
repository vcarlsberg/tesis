library(xts)
library(forecast)
library(nnfor)
library(TSrepr)
library(TSstudio)

flow_data<-read_data("Jakarta","K50000")
flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

train_test_data<-ts_split(flow_data_xts,sample.out=6)

EIDULFITR<-c(
  0,0,0,0,0,0,0,0, #1994
  0,0,1,0,0,0,0,0,0,0,0,0, #1995
  0,1,0,0,0,0,0,0,0,0,0,0, #1996
  0,1,0,0,0,0,0,0,0,0,0,0, #1997
  1,0,0,0,0,0,0,0,0,0,0,0, #1998
  1,0,0,0,0,0,0,0,0,0,0,0, #1999
  1,0,0,0,0,0,0,0,0,0,0,0, #2000
  0,0,0,0,0,0,0,0,0,0,0,1, #2001
  0,0,0,0,0,0,0,0,0,0,0,1, #2002
  0,0,0,0,0,0,0,0,0,0,1,0, #2003
  0,0,0,0,0,0,0,0,0,0,1,0, #2004
  0,0,0,0,0,0,0,0,0,0,1,0, #2005
  0,0,0,0,0,0,0,0,0,1,0,0, #2006
  0,0,0,0,0,0,0,0,0,1,0,0, #2007
  0,0,0,0,0,0,0,0,0,1,0,0, #2008
  0,0,0,0,0,0,0,0,1,0,0,0, #2009
  0,0,0,0,0,0,0,0,1,0,0,0, #2010
  0,0,0,0,0,0,0,1,0,0,0,0, #2011
  0,0,0,0,0,0,0,1,0,0,0,0, #2012
  0,0,0,0,0,0,0,1,0,0,0,0, #2013
  0,0,0,0,0,0,1,0,0,0,0,0, #2014
  0,0,0,0,0,0,1,0,0,0,0,0, #2015
  0,0,0,0,0,0,1,0,0,0,0,0, #2016
  0,0,0,0,0,1,0,0,0,0,0,0, #2017
  0,0,0,0,0,1,0,0,0,0,0,0 #2018
)


arima.model<-auto.arima(train_test_data$train,xreg =EIDULFITR )
arima.model2<-auto.arima(train_test_data$train,lambda = "auto",xreg =EIDULFITR)

result<-ts.intersect(train_test_data$train,arima.model2$fitted)
colnames(result)<-c("train_data","arima_fitted")

mape(result[,1],result[,2])

