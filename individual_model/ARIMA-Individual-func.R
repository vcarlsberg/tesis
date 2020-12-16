ARIMA_Individual<-function(preprocessing,location,denomination,flow)
{
  source("~/tesis/all_function.R")
  init_run()
  set.seed(72)
  
  
    compile <- data.frame(Flow=character(),
                          ID=character(),
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
                          preprocessing=character(),
                          weightingMethod=character(),
                          weightingModel1=numeric(),
                          weightingModel2=numeric())
    
  
  id<-random_id()
  dateexecuted<-Sys.time()
  
  flow_data<-read_data(location,denomination,flow)
  flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                      frequency=12)
  
  lambda<-preprocessing
  flow_data_transformed<-BoxCox(flow_data_xts,lambda=lambda)
  
  train_test_data<-split_data(flow_data_transformed,20)
  
  if(adf.test(train_test_data$train)$p.value>0.05){
    arima.model<-auto.arima(train_test_data$train,d = 1,D=1)
  }else{
    arima.model<-auto.arima(train_test_data$train,d = 0,D=0)
  }
  
  
  result<-ts.intersect(train_test_data$train,arima.model$fitted)
  colnames(result)<-c("train_data","arima_fitted")
  
  result<-ts.intersect(result[,1],result[,2])
  result<-result %>% InvBoxCox(lambda=lambda)
  colnames(result)<-c("train_data","fitted")

  preprocessing.candidate<-paste("Box-Cox lambda",lambda)
  
  compile<-rbind(compile,data.frame(Flow=flow,
                                    Model="ARIMA-Individual",
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
                                    DateExecuted=dateexecuted,
                                    weightingMethod="",
                                    weightingModel1=0,
                                    weightingModel2=0))
  
  for (fh in 1:24) {
    frc.arima<-forecast(arima.model,h=fh)
    result.pred<-ts.intersect(train_test_data$test[1:fh],frc.arima$mean)%>%InvBoxCox(lambda=lambda) 
    
    colnames(result.pred)<-c("test_data","arima_fitted")
    
    compile<-rbind(compile,data.frame(Flow=flow,
                                      Model="ARIMA-Individual",
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
                                      DateExecuted=dateexecuted,
                                      weightingMethod="",
                                      weightingModel1=0,
                                      weightingModel2=0))
  }
  
  return(compile)

}