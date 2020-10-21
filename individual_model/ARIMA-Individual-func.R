ARIMA_Individual<-function(preprocessing,location,denomination)
{
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
  
  flow_data<-read_data(location,denomination)
  flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                      frequency=12)
  
  if(preprocessing==TRUE)
  {
    lambda<-BoxCox.lambda(flow_data_xts)
    flow_data_transformed<-BoxCox(flow_data_xts,lambda=lambda)
  }else{
    flow_data_transformed<-flow_data_xts
  }
  
  train_test_data<-split_data(flow_data_transformed,20)
  
  arima.model<-auto.arima(train_test_data$train)
  
  result<-ts.intersect(train_test_data$train,arima.model$fitted)
  colnames(result)<-c("train_data","arima_fitted")
  
  if(preprocessing==TRUE){
    result<-ts.intersect(result[,1],result[,2])
    result<-result %>% InvBoxCox(lambda=lambda)
    colnames(result)<-c("train_data","fitted")
  }else{
    result<-ts.intersect(result[,1],result[,2])
    colnames(result)<-c("train_data","fitted")
  }
  
  preprocessing.candidate<-if(preprocessing==TRUE) paste("Box-Cox lambda",lambda) else ""
  
  compile<-rbind(compile,data.frame(Model="ARIMA-Individual",
                                    InOutSample="In Sample",
                                    Location=location,
                                    Denomination=denomination,
                                    fh=0,
                                    MAPE=TSrepr::mape(result[,1],result[,2]),
                                    RMSE=TSrepr::rmse(result[,1],result[,2]),
                                    linearmodel=as.character(arima.model),
                                    nonlinearmodel="",
                                    preprocessing=preprocessing.candidate,
                                    ID=id,
                                    DateExecuted=dateexecuted))
  
  for (fh in 1:24) {
    frc.arima<-forecast(arima.model,h=fh)
    result.pred<-ts.intersect(train_test_data$test[1:fh],frc.arima$mean)

    
    if(preprocessing==TRUE){
      result.pred<-ts.intersect(train_test_data$test[1:fh],frc.arima$mean) %>%InvBoxCox(lambda=lambda)  
    }else{
      result.pred<-ts.intersect(train_test_data$test[1:fh],frc.arima$mean)  
    }
    
    colnames(result.pred)<-c("test_data","arima_fitted")
    
    compile<-rbind(compile,data.frame(Model="ARIMA-Individual",
                                      InOutSample="Out Sample",
                                      Location=location,
                                      Denomination=denomination,
                                      fh=fh,
                                      MAPE=TSrepr::mape(result.pred[,1],result.pred[,2]),
                                      RMSE=TSrepr::rmse(result.pred[,1],result.pred[,2]),
                                      linearmodel=as.character(arima.model),
                                      nonlinearmodel="" ,
                                      preprocessing=preprocessing.candidate,
                                      ID=id,
                                      DateExecuted=dateexecuted))
  }
  
  return(compile)

}