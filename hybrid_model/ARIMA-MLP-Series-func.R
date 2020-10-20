preprocessing=TRUE
weighting="lm"
MLP_layer=1
location="Jakarta"
denomination="K100000"

ARIMA_MLP_Series(preprocessing=preprocessing,weighting=weighting,MLP_layer=MLP_layer,
                   location=location,denomination=denomination)

ARIMA_MLP_Series<-function(preprocessing,MLP_layer,location,denomination)
{
  source("~/tesis/all_function.R")
  init_run()
  set.seed(72)
  
  id<-random_id()
  dateexecuted<-Sys.time()
  
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
                          preprocessing=character(),
                          weighting=character())
    
  }
  
  data<-read_data(location,denomination)
  
  flow_data_xts <- ts(data[,3],start=c(data[1,1],data[1,2]), end=c(2019, 6), 
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
  residual<-train_test_data$train-arima.model$fitted
  
  if(MLP_layer==1)
  {
    testFun <- function(x)
    {
      mlp.model<-mlp(residual,hd=c(x[1]),
                     reps = 1,
                     lags = 1:60)
      mlp.model$MSE
    }
    sol <- gridSearch(fun = testFun, levels = list(1:20))
    
  }else if(MLP_layer==2){
    testFun <- function(x)
    {
      mlp.model<-mlp(residual,hd=c(x[1],x[2]),
                     reps = 1,
                     lags = 1:60)
      mlp.model$MSE
    }
    
    sol <- gridSearch(fun = testFun, levels = list(1:20,1:20))
  }
  
  mlp.model<-mlp(residual,hd=c(sol$minlevels),
                 reps = 1,
                 lags = 1:60)
  
  result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
  colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  
  if(preprocessing==TRUE){
    result<-ts.intersect(result[,1],result[,2]+result[,3])
    result<-result %>% InvBoxCox(lambda=lambda)
    colnames(result)<-c("train_data","fitted")
  }else{
    result<-ts.intersect(result[,1],result[,2]+result[,3])
    colnames(result)<-c("train_data","fitted")
  }
  

  linearmodel.candidate<-as.character(arima.model)
  nonlinearmodel.candidate<- if(MLP_layer==1) paste(sol$minlevels[1]) else paste(sol$minlevels[1],sol$minlevels[2],sep = "-")
  preprocessing.candidate<-if(preprocessing==TRUE) paste("Box-Cox lambda",lambda) else ""
  
  
  compile<-rbind(compile,data.frame(Model="ARIMA-MLP-Series",
                                    InOutSample="In Sample",
                                    Location=location,
                                    Denomination=denomination,
                                    fh=0,
                                    MAPE=TSrepr::mape(result[,1],result[,2]),
                                    RMSE=TSrepr::rmse(result[,1],result[,2]),
                                    linearmodel=linearmodel.candidate,
                                    nonlinearmodel=nonlinearmodel.candidate,
                                    preprocessing=preprocessing.candidate,
                                    ID=id,
                                    DateExecuted=dateexecuted,
                                    weighting=""))
  
  
  
  for (fh in 1:24) {
    frc.mlp<-forecast(mlp.model,h=fh)
    frc.arima<-forecast(arima.model,h=fh)
    
    if(preprocessing==TRUE){
      result.pred<-ts.intersect(train_test_data$test[1:fh],frc.mlp$mean+frc.arima$mean) %>%InvBoxCox(lambda=lambda)  
    }else{
      result.pred<-ts.intersect(train_test_data$test[1:fh],frc.mlp$mean+frc.arima$mean)  
    }
    
    
    compile<-rbind(compile,data.frame(Model="ARIMA-MLP-Series",
                                      InOutSample="Out Sample",
                                      Location=location,
                                      Denomination=denomination,
                                      fh=fh,
                                      MAPE=TSrepr::mape(result.pred[,1],result.pred[,2]),
                                      RMSE=TSrepr::rmse(result.pred[,1],result.pred[,2]),
                                      linearmodel=linearmodel.candidate,
                                      nonlinearmodel=nonlinearmodel.candidate,
                                      preprocessing=preprocessing.candidate,
                                      ID=id,
                                      DateExecuted=dateexecuted,
                                      weighting=""))

  }

  
return(compile)  
    
  
  
  
}


