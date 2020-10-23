MLP_Individual<-function(preprocessing,MLP_layer,location,denomination)
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
                          preprocessing=character(),
                          weightingMethod=character(),
                          weightingModel1=numeric(),
                          weightingModel2=numeric())
    
  }
  
  if(!exists("gridsearchNN")){
    gridsearchNN <- data.frame(ID=character(),
                          DateExecuted=character(),
                          layer1=character(),
                          layer2=character(),
                          error=numeric()
                          )
    
  }
  
  id<-random_id()
  dateexecuted<-Sys.time()
  
  data<-read_data(location,denomination)
  
  flow_data_xts <- ts(data[,3],start=c(data[1,1],data[1,2]), end=c(2019, 6), 
                      frequency=12)
  xreg_xts<-ts(data[,4],start=c(data[1,1],data[1,2]), end=c(2019, 6), 
               frequency=12)
  
  if(preprocessing==TRUE)
  {
    lambda<-BoxCox.lambda(flow_data_xts)
    flow_data_transformed<-BoxCox(flow_data_xts,lambda=lambda)
  }else{
    flow_data_transformed<-flow_data_xts
  }
  
  train_test_data<-split_data(flow_data_transformed,20)
  xreg_data<-split_data(xreg_xts,20)
  
  if(MLP_layer==1)
  {
    testFun <- function(x)
    {
      mlp.model<-mlp(train_test_data$train,hd=c(x[1]),
                     reps = 1,
                     lags = 1:60,
                     xreg =as.data.frame(xreg_data$train),
                     xreg.lags=list(0),xreg.keep=list(TRUE))
      mlp.model$MSE
    }
    sol <- gridSearch(fun = testFun, levels = list(1:20))
    
    gs.result<-cbind(t(as.data.frame(sol[["levels"]])),"",as.data.frame(sol$values),id,dateexecuted)
    row.names(gs.result)<-NULL
    colnames(gs.result)<-c("layer1","layer2","error","ID","DateExecuted")
    gridsearchNN<-rbind(gridsearchNN,gs.result)
    }else if(MLP_layer==2){
    testFun <- function(x)
    {
      mlp.model<-mlp(train_test_data$train,hd=c(x[1],x[2]),
                     reps = 1,
                     lags = 1:60,
                     xreg =as.data.frame(xreg_data$train),
                     xreg.lags=list(0),xreg.keep=list(TRUE))
      mlp.model$MSE
    }
    sol <- gridSearch(fun = testFun, levels = list(1:20,1:20))
    
    gs.result<-cbind(t(as.data.frame(sol[["levels"]])),as.data.frame(sol$values),id,dateexecuted)
    row.names(gs.result)<-NULL
    colnames(gs.result)<-c("layer1","layer2","error","ID","DateExecuted")
    gridsearchNN<-rbind(gridsearchNN,gs.result)
    
  }
  
  mlp.model<-mlp(train_test_data$train,hd=c(sol$minlevels),
                 reps = 1,
                 lags = 1:60,
                 xreg =as.data.frame(xreg_data$train),
                 xreg.lags=list(0),xreg.keep=list(TRUE))
  
  result<-ts.intersect(train_test_data$train,mlp.model$fitted)
  colnames(result)<-c("train_data","mlp_fitted")
  
  nonlinearmodel.candidate<- if(MLP_layer==1) paste(sol$minlevels[1]) else paste(sol$minlevels[1],sol$minlevels[2],sep = "-")
  preprocessing.candidate<-if(preprocessing==TRUE) paste("Box-Cox lambda",lambda) else ""
  
  compile<-rbind(compile,data.frame(Model="MLP-Individual",
                                    InOutSample="In Sample",
                                    Location=location,
                                    Denomination=denomination,
                                    fh=0,
                                    MAPE=TSrepr::mape(result[,1],result[,2]),
                                    RMSE=TSrepr::rmse(result[,1],result[,2]),
                                    linearmodel="",
                                    nonlinearmodel=nonlinearmodel.candidate,
                                    preprocessing=preprocessing.candidate,
                                    ID=id,
                                    DateExecuted=dateexecuted,
                                    weightingMethod="",
                                    weightingModel1="",
                                    weightingModel2=""))
  
  for (fh in 1:24) {
    frc.mlp<-forecast(mlp.model,h=fh,xreg = as.data.frame(xreg_xts))
    result.pred<-ts.intersect(train_test_data$test[1:fh],frc.mlp$mean)
    colnames(result.pred)<-c("test_data","mlp_fitted")
    
    mlp.mean<-if (preprocessing==FALSE) frc.mlp$mean else frc.mlp$mean%>%InvBoxCox(lambda=lambda)
    test.data<-if (preprocessing==FALSE) train_test_data$test[1:fh] else train_test_data$test[1:fh]%>%InvBoxCox(lambda=lambda)
    
    result.pred<-ts.intersect(test.data,mlp.mean)
    colnames(result.pred)<-c("train_data","mlp_fitted")
    
    compile<-rbind(compile,data.frame(Model="MLP-Individual",
                                      InOutSample="Out Sample",
                                      Location=location,
                                      Denomination=denomination,
                                      fh=fh,
                                      MAPE=TSrepr::mape(result.pred[,1],result.pred[,2]),
                                      RMSE=TSrepr::rmse(result.pred[,1],result.pred[,2]),
                                      linearmodel="",
                                      nonlinearmodel=nonlinearmodel.candidate,
                                      preprocessing=preprocessing.candidate,
                                      ID=id,
                                      DateExecuted=dateexecuted,
                                      weightingMethod="",
                                      weightingModel1="",
                                      weightingModel2=""))
  }
  
  return(list("modelResult"=compile,"gridsearchNN"=gridsearchNN))

}
