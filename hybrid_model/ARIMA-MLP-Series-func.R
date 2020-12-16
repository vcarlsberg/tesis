ARIMA_MLP_Series<-function(preprocessing,MLP_layer,location,denomination,flow,lag)
{
  source("~/tesis/all_function.R")
  init_run()
  set.seed(72)
  
  id<-random_id()
  dateexecuted<-Sys.time()
  

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
    

  

    gridsearchNN <- data.frame(ID=character(),
                               DateExecuted=character(),
                               layer1=character(),
                               layer2=character(),
                               error=numeric(),
                               inputLayer=numeric()
    )
    

  
  data<-read_data(location,denomination,flow)
  
  flow_data_xts <- ts(data[,3],start=c(data[1,1],data[1,2]), end=c(2019, 6), 
                      frequency=12)
  
  lambda<-preprocessing
  flow_data_transformed<-BoxCox(flow_data_xts,lambda=lambda)
  
  train_test_data<-split_data(flow_data_transformed,20)

  if(adf.test(train_test_data$train)$p.value>0.05){
    arima.model<-auto.arima(train_test_data$train,d = 1,D=1,ic = "aicc")
  }else{
    arima.model<-auto.arima(train_test_data$train,d = 0,D=0,ic = "aicc")
  }
  
  residual<-train_test_data$train-arima.model$fitted
  
  if(MLP_layer==1)
  {
    testFun <- function(x)
    {
      mlp.model<-mlp(residual,hd=c(x[1]),
                     lags = lag,
                     sel.lag = FALSE,
      )
      rmse_oos<-TSrepr::rmse(x=train_test_data$test,
                             y=forecast(mlp.model,h=length(train_test_data$test))$mean)
    }
    sol <- gridSearch(fun = testFun, levels = list(1:20))
	
	  gs.result<-cbind(t(as.data.frame(sol[["levels"]])),0,as.data.frame(sol$values),id,dateexecuted,length(lag))
    row.names(gs.result)<-NULL
    colnames(gs.result)<-c("layer1","layer2","error","ID","DateExecuted","inputLayer")
    gridsearchNN<-rbind(gridsearchNN,gs.result)
    
  }else if(MLP_layer==2){
    testFun <- function(x)
    {
      mlp.model<-mlp(residual,hd=c(x[1],x[2]),
                     lags = lag,
                     sel.lag = FALSE,
      )
      rmse_oos<-TSrepr::rmse(x=train_test_data$test,
                             y=forecast(mlp.model,h=length(train_test_data$test))$mean)
    }
    
    sol <- gridSearch(fun = testFun, levels = list(1:20,1:20))
	  
    gs.result<-cbind(t(as.data.frame(sol[["levels"]])),as.data.frame(sol$values),id,dateexecuted,length(lag))
    row.names(gs.result)<-NULL
    colnames(gs.result)<-c("layer1","layer2","error","ID","DateExecuted","inputLayer")
    gridsearchNN<-rbind(gridsearchNN,gs.result)
	
    
  }
  
  mlp.model<-mlp(residual,hd=c(sol$minlevels),
                 lags = lag,
                 sel.lag = FALSE)
  
  result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted) %>% InvBoxCox(lambda=lambda)
  colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  
  result<-ts.intersect(result[,1],result[,2]+result[,3])
  colnames(result)<-c("train_data","fitted")

  linearmodel.candidate<-as.character(arima.model)
  nonlinearmodel.candidate<- if(MLP_layer==1) paste(sol$minlevels[1]) else paste(sol$minlevels[1],sol$minlevels[2],sep = "-")
  preprocessing.candidate<-paste("Box-Cox lambda",lambda)
  
  
  compile<-rbind(compile,data.frame(Flow=flow,
                                    Model="ARIMA-MLP-Series",
                                    InOutSample="In Sample",
                                    Location=location,
                                    Denomination=denomination,
                                    fh=0,
                                    MAPE=TSrepr::mape(result[,1],result[,2]),
                                    RMSE=TSrepr::rmse(result[,1],result[,2]),
                                    linearmodel=linearmodel.candidate,
                                    nonlinearmodel=paste0(length(lag),"-",nonlinearmodel.candidate,"-",1),
                                    preprocessing=preprocessing.candidate,
                                    ID=id,
                                    DateExecuted=dateexecuted,
                                    weightingMethod="",
                                    weightingModel1=0,
                                    weightingModel2=0))
  
  
  
  for (fh in 1:24) {
    frc.mlp<-forecast(mlp.model,h=fh)
    frc.arima<-forecast(arima.model,h=fh)
    
    result.pred<-ts.intersect(train_test_data$test[1:fh],frc.mlp$mean,frc.arima$mean) %>%InvBoxCox(lambda=lambda)  
    colnames(result.pred)<-c("train_data","mlp_fitted","arima_fitted")
    
    result.pred<-ts.intersect(result.pred[,1],result.pred[,2]+result.pred[,3])
    colnames(result.pred)<-c("test_data","forecast")
    
    compile<-rbind(compile,data.frame(Flow=flow,
                                      Model="ARIMA-MLP-Series",
                                      InOutSample="Out Sample",
                                      Location=location,
                                      Denomination=denomination,
                                      fh=fh,
                                      MAPE=TSrepr::mape(result.pred[,1],result.pred[,2]),
                                      RMSE=TSrepr::rmse(result.pred[,1],result.pred[,2]),
                                      linearmodel=linearmodel.candidate,
                                      nonlinearmodel=paste0(length(lag),"-",nonlinearmodel.candidate,"-",1),
                                      preprocessing=preprocessing.candidate,
                                      ID=id,
                                      DateExecuted=dateexecuted,
                                      weightingMethod="",
                                      weightingModel1=0,
                                      weightingModel2=0))

  }

  
  return(list("modelResult"=compile,"gridsearchNN"=gridsearchNN)) 
    
  
  
  
}


