source("~/tesis/all_function.R")
init_run()
set.seed(72)

if(!exists("compile")){
  compile <- data.frame(ID=character(),
                        DateExecuted=character(),
                        Model=character(),
                        InOutSample=character(),
                        Location=character(),
                        Denomination=character(),
                        fh=numeric(), 
                        MAPE=numeric(),
                        RMSE=numeric(),
                        linearmodel=character(),
                        nonlinearmodel=character(),
                        preprocessing=character())
  
}

id<-random_id()
dateexecuted<-Sys.time()

for(location in c("Jakarta"))
{
  for(denomination in c("K100000"))
  {
    flow_data<-read_data(location,denomination)
    flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                        frequency=12)
    
    train_test_data<-split_data(flow_data_xts,20)
    
    arima.model<-auto.arima(train_test_data$train,seasonal = FALSE)
    arima.model2<-auto.arima(train_test_data$train,
                             lambda = "auto")
    
    result<-ts.intersect(train_test_data$train,arima.model$fitted)
    colnames(result)<-c("train_data","arima_fitted")
    
    print(paste0("in-sample MAPE : ",location," ",denomination," ",mape(result[,1],result[,2])))
    
    compile<-rbind(compile,data.frame(Model="ARIMA-Individual",
                            InOutSample="In Sample",
                            Location=location,
                            Denomination=denomination,
                            fh=0,
                            MAPE=mape(result[,1],result[,2]),
                            RMSE=rmse(result[,1],result[,2]),
                            linearmodel=as.character(arima.model),
                            nonlinearmodel="",
                            preprocessing="",
                            ID=id,
                            DateExecuted=dateexecuted
                            ))
    
    for (fh in 1:24) {
      frc.arima<-forecast(arima.model,h=fh)
      result.pred<-ts.intersect(train_test_data$test[1:fh],frc.arima$mean)
      colnames(result.pred)<-c("test_data","arima_fitted")
      
      print(paste0("out-sample MAPE : ",location," ",denomination," ",fh," ",
                   mape(result.pred[,1],result.pred[,2])))
      
      compile<-rbind(compile,data.frame(Model="ARIMA-Individual",
                              InOutSample="Out Sample",
                              Location=location,
                              Denomination=denomination,
                              fh=fh,
                              MAPE=mape(result.pred[,1],result.pred[,2]),
                              RMSE=rmse(result.pred[,1],result.pred[,2]),
                              linearmodel=as.character(arima.model),
                              nonlinearmodel="" ,
                              preprocessing="",
                              ID=id,
                              DateExecuted=dateexecuted
                              ))
    }
    
  }
}






