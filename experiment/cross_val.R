#ARIMA with CV
library(lubridate)
library(scales)
library(timetk)
init_run()
source("all_function.R")

flow_data<-read_data("Jakarta","K100000","Outflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                         frequency=12)

###Modelling ARIMA####
arima.model<-(flow_data_xts %>% split_data(precentage_test = 20))$train %>% 
  Arima(order=c(0,1,2),seasonal=c(1,0,0),
        xreg=(flow_data_xts_xreg %>% split_data(precentage_test = 20))$train)

dlnn.model<-nnfor::mlp(y=(flow_data_xts %>% split_data(precentage_test = 20))$train,
                       m=12,
                       hd=c(10,12),
                       difforder = 0,outplot = TRUE,retrain = TRUE,
                       allow.det.season = FALSE,
                       reps = 1,
                       lags = 1:2,
                       sel.lag = FALSE
                       )



for(h in c(1:24))
{
  vec_res_train<-c()
  vec_res_test<-c()
  
  timeSlice<-caret::createTimeSlices(index(flow_data_xts),initialWindow = round(length(flow_data_xts)*0.8),horizon = h)
  
  for(a in c(189:201))
  {
    subj<-paste0("Training",a)
    subj2<-paste0("Testing",a)
    
    data_train_cv<-ts(
      (flow_data_xts)[timeSlice[["train"]][[subj]]],
      start=index(flow_data_xts)[timeSlice[["train"]][[subj]]]%>%min(),
      end=index(flow_data_xts)[timeSlice[["train"]][[subj]]]%>%max(),
      frequency = 12
    )
    
    data_train_xreg_cv<-ts(
      (flow_data_xts_xreg)[timeSlice[["train"]][[subj]]],
      start=index(flow_data_xts)[timeSlice[["train"]][[subj]]]%>%min(),
      end=index(flow_data_xts)[timeSlice[["train"]][[subj]]]%>%max(),
      frequency = 12
    )
    
    
    train_set_cv_arima<-data_train_cv %>% Arima(order = c(0,1,2),seasonal = c(1,0,0),
                                                xreg=data_train_xreg_cv)
    
    train_set_cv_dlnn<-data_train_cv %>% nnfor::mlp(m=12,
                                                    hd=c(10,12),
                                                    difforder = 0,outplot = TRUE,retrain = TRUE,
                                                    allow.det.season = FALSE,
                                                    reps = 1,
                                                    lags = 1:2,
                                                    sel.lag = FALSE
    )
    
    combine_train_cv<-ts.intersect(data_train_cv,
                                   fitted(train_set_cv_arima),
                                   fitted(train_set_cv_dlnn))
    rmse_train_cv<-TSrepr::mape(combine_train_cv[,1],
                                0.5*combine_train_cv[,2]+0.5*combine_train_cv[,3])
    vec_res_train<-append(vec_res_train,values=rmse_train_cv)
    
    data_test_cv<-ts(
      (flow_data_xts)[timeSlice[["test"]][[subj2]]],
      start=index(flow_data_xts)[timeSlice[["test"]][[subj2]]]%>%min(),
      end=index(flow_data_xts)[timeSlice[["test"]][[subj2]]]%>%max(),
      frequency = 12
    )
    
    data_test_xreg_cv<-ts(
      (flow_data_xts_xreg)[timeSlice[["test"]][[subj2]]],
      start=index(flow_data_xts)[timeSlice[["test"]][[subj2]]]%>%min(),
      end=index(flow_data_xts)[timeSlice[["test"]][[subj2]]]%>%max(),
      frequency = 12
    )
    
    test_set_cv_arima<-forecast(train_set_cv_arima,h = h,xreg = data_test_xreg_cv)$mean
    test_set_cv_dlnn<-forecast(train_set_cv_dlnn,h = h)$mean
    
    combine_test_cv<-ts.union(data_test_cv,
                              test_set_cv_arima,
                              test_set_cv_dlnn)
    rmse_test_cv<-TSrepr::mape(combine_test_cv[,1],
                               0.5*combine_test_cv[,2]+
                                 0.5*combine_test_cv[,3])
    vec_res_test<-append(vec_res_test,values=rmse_test_cv)
    
  }
  print(paste(mean(vec_res_test),mean(vec_res_train)))
  
}

