ARIMA_MLP_Parallel<-function(preprocessing,weighting,MLP_layer,location,denomination)
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
  
  if(!exists("gridsearchNN")){
    gridsearchNN <- data.frame(ID=character(),
                               DateExecuted=character(),
                               layer1=character(),
                               layer2=character(),
                               error=numeric()
    )
    
  }
  
  if(!exists("weightingInfo")){
    weightingInfo <- data.frame(ID=character(),
                                DateExecuted=character(),
                                WeightModel1=numeric(),
                                WeightModel2=numeric(),
                                Method=character()
    )
    
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
  
  if(MLP_layer==1)
  {
    testFun <- function(x)
    {
      mlp.model<-mlp(train_test_data$train,hd=c(x[1]),
                     reps = 1,
                     lags = 1:60)
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
                     lags = 1:60)
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
                 lags = 1:60)
  
  result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
  colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  
  if(preprocessing==TRUE){
    result<-result %>% InvBoxCox(lambda=lambda)
    colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  }
  
  ##weighting##
  if(weighting=="equal"){
    result_weight<-ts.intersect(result[,1],0.5*result[,2],0.5*result[,3])
    colnames(result_weight)<-c("train_data","mlp_fitted","arima_fitted")
    weight1<-0.5
    weight2<-0.5
  }else if(weighting=="lm"){
    lm.model<-lm(train_data~0+mlp_fitted+arima_fitted,data=result)
    result_weight<-ts.intersect(result[,1],
                                as.numeric(lm.model$coefficients[1])*result[,2],
                                as.numeric(lm.model$coefficients[2])*result[,3])
    colnames(result_weight)<-c("train_data","mlp_fitted","arima_fitted")
    weight1<-as.numeric(lm.model$coefficients[1])
    weight2<-as.numeric(lm.model$coefficients[2])
  }else if(weighting=="ga"){
    weight_kecil<-function(w1,w2) 
    {
      sse(result[,1],
          w1*na.omit(result[,2])+w2*na.omit(result[,3]))
    }
    
    set.seed(72)
    GA <- ga(type = "real-valued",pmutation=0.5,
             fitness = function(w) -weight_kecil(w[1],w[2]),
             lower =c(-1,-1), upper = c(1,1),
             maxiter=500,parallel=TRUE,seed=72,monitor = FALSE)
    
    result_weight<-ts.intersect(result[,1],
                                GA@solution[1]*result[,2],
                                GA@solution[2]*result[,3])
    colnames(result_weight)<-c("train_data","mlp_fitted","arima_fitted")
    weight1<-GA@solution[1]
    weight2<-GA@solution[2]
    
  }
  
  
  
  linearmodel.candidate<-as.character(arima.model)
  nonlinearmodel.candidate<- if(MLP_layer==1) paste(sol$minlevels[1]) else paste(sol$minlevels[1],sol$minlevels[2],sep = "-")
  preprocessing.candidate<-if(preprocessing==TRUE) paste("Box-Cox lambda",lambda) else ""
  
  weightingInfo<-rbind(weightingInfo,data.frame(
    ID=id,
    DateExecuted=dateexecuted,
    WeightModel1=weight1,
    WeightModel2=weight2,
    Method=weighting
  ))
  
  compile<-rbind(compile,data.frame(Model="ARIMA-MLP-Parallel",
                                    InOutSample="In Sample",
                                    Location=location,
                                    Denomination=denomination,
                                    fh=0,
                                    MAPE=TSrepr::mape(result_weight[,1],result_weight[,2]+result_weight[,3]),
                                    RMSE=TSrepr::rmse(result_weight[,1],result_weight[,2]+result_weight[,3]),
                                    linearmodel=linearmodel.candidate,
                                    nonlinearmodel=nonlinearmodel.candidate,
                                    preprocessing=preprocessing.candidate,
                                    ID=id,
                                    DateExecuted=dateexecuted,
                                    weighting=weighting))
  
  
  
  for (fh in 1:24) {
    frc.mlp<-forecast(mlp.model,h=fh)
    frc.arima<-forecast(arima.model,h=fh)
    
    mlp.mean<-if (preprocessing==FALSE) frc.mlp$mean else frc.mlp$mean%>%InvBoxCox(lambda=lambda)
    arima.mean<-if (preprocessing==FALSE) frc.arima$mean else frc.arima$mean%>%InvBoxCox(lambda=lambda)
    test.data<-if (preprocessing==FALSE) train_test_data$test[1:fh] else train_test_data$test[1:fh]%>%InvBoxCox(lambda=lambda)
    
    if(weighting=="equal"){
      result.pred.weight<-ts.intersect(test.data,0.5*mlp.mean,0.5*arima.mean)
      colnames(result.pred.weight)<-c("train_data","mlp_fitted","arima_fitted")
    }else if(weighting=="lm"){
      result.pred.weight<-ts.intersect(test.data,
                                       as.numeric(lm.model$coefficients[1])*mlp.mean,
                                       as.numeric(lm.model$coefficients[2])*arima.mean)
      colnames(result.pred.weight)<-c("train_data","mlp_fitted","arima_fitted")
    }else if(weighting=="ga"){
      result.pred.weight<-ts.intersect(test.data,
                                       GA@solution[1]*mlp.mean,
                                       GA@solution[2]*arima.mean)
      colnames(result.pred.weight)<-c("train_data","mlp_fitted","arima_fitted")
    }
    
    compile<-rbind(compile,data.frame(Model="ARIMA-MLP-Parallel",
                                      InOutSample="Out Sample",
                                      Location=location,
                                      Denomination=denomination,
                                      fh=fh,
                                      MAPE=TSrepr::mape(result.pred.weight[,1],result.pred.weight[,2]+result.pred.weight[,3]),
                                      RMSE=TSrepr::rmse(result.pred.weight[,1],result.pred.weight[,2]+result.pred.weight[,3]),
                                      linearmodel=linearmodel.candidate,
                                      nonlinearmodel=nonlinearmodel.candidate,
                                      preprocessing=preprocessing.candidate,
                                      ID=id,
                                      DateExecuted=dateexecuted,
                                      weighting=weighting))
    
  }
  
  
  return(list("modelResult"=compile,"gridsearchNN"=gridsearchNN,"weightingInfo"=weightingInfo))
  
  
  
  
}


