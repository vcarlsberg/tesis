ARIMAX_MLPX_Parallel<-function(preprocessing,MLP_layer,location,denomination,flow,lag)
{
  source("all_function.R")
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
                               layer1=numeric(),
                               layer2=numeric(),
                               error=numeric(),
                               inputLayer=numeric()
    )
    
  
  
  
  data<-read_data(location,denomination,flow)
  
  flow_data_xts <- ts(data[,3],start=c(data[1,1],data[1,2]), end=c(2019, 6), 
                      frequency=12)
  xreg_xts<-ts(data[,c(4:22,24)],start=c(data[1,1],data[1,2]), end=c(2019, 6), 
               frequency=12)
  

  lambda<-preprocessing
  flow_data_transformed<-forecast::BoxCox(flow_data_xts,lambda=lambda)

  train_test_data<-split_data(flow_data_transformed,20)
  xreg_data<-split_data(xreg_xts,20)
  
  arima.model<-auto.arima(train_test_data$train,d = 0,D=0,xreg = xreg_data$train,ic = "aicc",seasonal = TRUE)
  
  if(MLP_layer==1)
  {
    testFun <- function(x)
    {
      mlp.model<-mlp(train_test_data$train,hd=c(x[1]),
                     reps = 1,
                     lags = lag,
                     sel.lag = FALSE,
                     xreg =as.data.frame(xreg_data$train),
                     xreg.lags=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                     xreg.keep=c(T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T)
      )
      rmse_oos<-TSrepr::rmse(x=train_test_data$test,
                             y=forecast(mlp.model,h=length(train_test_data$test),
                                        xreg=as.data.frame(xreg_xts))$mean)
    }
    sol <- gridSearch(fun = testFun, levels = list(1:20),
                      method = "multicore",
                      mc.control = list(mc.preschedule = FALSE)
                      )
    
    gs.result<-cbind(t(as.data.frame(sol[["levels"]])),0,as.data.frame(sol$values),id,dateexecuted,length(lag))
    row.names(gs.result)<-NULL
    colnames(gs.result)<-c("layer1","layer2","error","ID","DateExecuted","inputLayer")
    gridsearchNN<-rbind(gridsearchNN,gs.result)
    
    
    
  }else if(MLP_layer==2){
    testFun <- function(x)
    {
      mlp.model<-mlp(train_test_data$train,hd=c(x[1],x[2]),
                     reps = 1,
                     lags = lag,
                     sel.lag = FALSE,
                     xreg =as.data.frame(xreg_data$train),
                     xreg.lags=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                     xreg.keep=c(T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T))
      rmse_oos<-TSrepr::rmse(x=train_test_data$test,
                             y=forecast(mlp.model,h=length(train_test_data$test),
                                        xreg=as.data.frame(xreg_xts))$mean)
    }
    sol <- gridSearch(fun = testFun, levels = list(1:20,1:20),
                      method = "multicore",
                      mc.control = list(mc.preschedule = FALSE)
                      )
    
    gs.result<-cbind(t(as.data.frame(sol[["levels"]])),as.data.frame(sol$values),id,dateexecuted,length(lag))
    row.names(gs.result)<-NULL
    colnames(gs.result)<-c("layer1","layer2","error","ID","DateExecuted","inputLayer")
    gridsearchNN<-rbind(gridsearchNN,gs.result)
    
    
  }
  
  mlp.model<-mlp(train_test_data$train,hd=c(sol$minlevels),
                 reps = 1,
                 lags = lag,
                 sel.lag = FALSE,
                 xreg =as.data.frame(xreg_data$train),
                 xreg.lags=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                 xreg.keep=c(T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T))
  
  result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
  colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  
  result<-result %>% InvBoxCox(lambda=lambda)
  colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
  
  linearmodel.candidate<-as.character(arima.model)
  nonlinearmodel.candidate<- if(MLP_layer==1) paste(sol$minlevels[1]) else paste(sol$minlevels[1],sol$minlevels[2],sep = "-")
  preprocessing.candidate<-paste("Box-Cox lambda",lambda)
  
  
  for(weighting in c("equal","lm","ga"))
  {
    if(weighting=="equal")
    {
      result_weight<-ts.intersect(result[,1],0.5*result[,2],0.5*result[,3])
      colnames(result_weight)<-c("train_data","mlp_fitted","arima_fitted")
      weight1<-0.5
      weight2<-0.5
      
      compile<-rbind(compile,data.frame(Flow=flow,
                                        Model=if (MLP_layer==1) "ARIMAX-FFNNX-Paralel (SA)" else "ARIMAX-DLNNX-Paralel (SA)",
                                        InOutSample="In Sample",
                                        Location=location,
                                        Denomination=denomination,
                                        fh=0,
                                        MAPE=TSrepr::mape(result_weight[,1],result_weight[,2]+result_weight[,3]),
                                        RMSE=TSrepr::rmse(result_weight[,1],result_weight[,2]+result_weight[,3]),
                                        linearmodel=linearmodel.candidate,
                                        nonlinearmodel=paste0(length(lag),"-",nonlinearmodel.candidate,"-",1),
                                        preprocessing=preprocessing.candidate,
                                        ID=id,
                                        DateExecuted=dateexecuted,
                                        weightingMethod=weighting,
                                        weightingModel1=weight1,
                                        weightingModel2=weight2))
      
      for (fh in 1:24) {
        frc.mlp<-forecast(mlp.model,h=fh,xreg = as.data.frame(xreg_xts))
        frc.arima<-forecast(arima.model,xreg = xreg_data$test)$mean[1:fh]
        
        mlp.mean<-frc.mlp$mean%>%InvBoxCox(lambda=lambda) 
        arima.mean<-frc.arima%>%InvBoxCox(lambda=lambda) 
        test.data<-train_test_data$test[1:fh]%>%InvBoxCox(lambda=lambda)
        
        result_pred_weight<-ts.intersect(test.data,0.5*mlp.mean,0.5*arima.mean)
        colnames(result_pred_weight)<-c("train_data","mlp_fitted","arima_fitted")
        
        compile<-rbind(compile,data.frame(Flow=flow,
                                          Model=if (MLP_layer==1) "ARIMAX-FFNNX-Paralel (SA)" else "ARIMAX-DLNNX-Paralel (SA)",
                                          InOutSample="Out Sample",
                                          Location=location,
                                          Denomination=denomination,
                                          fh=fh,
                                          MAPE=TSrepr::mape(result_pred_weight[,1],result_pred_weight[,2]+result_pred_weight[,3]),
                                          RMSE=TSrepr::rmse(result_pred_weight[,1],result_pred_weight[,2]+result_pred_weight[,3]),
                                          linearmodel=linearmodel.candidate,
                                          nonlinearmodel=paste0(length(lag),"-",nonlinearmodel.candidate,"-",1),
                                          preprocessing=preprocessing.candidate,
                                          ID=id,
                                          DateExecuted=dateexecuted,
                                          weightingMethod=weighting,
                                          weightingModel1=weight1,
                                          weightingModel2=weight2))
        
      }
      
    }else if(weighting=="lm"){
      lm.model<-lm(train_data~0+mlp_fitted+arima_fitted,data=result)
      result_weight<-ts.intersect(result[,1],
                                  as.numeric(lm.model$coefficients[1])*result[,2],
                                  as.numeric(lm.model$coefficients[2])*result[,3])
      colnames(result_weight)<-c("train_data","mlp_fitted","arima_fitted")
      weight1<-as.numeric(lm.model$coefficients[1])
      weight2<-as.numeric(lm.model$coefficients[2])
      
      compile<-rbind(compile,data.frame(Flow=flow,
                                        Model=if (MLP_layer==1) "ARIMAX-FFNNX-Paralel (OLS)" else "ARIMAX-DLNNX-Paralel (OLS)",
                                        InOutSample="In Sample",
                                        Location=location,
                                        Denomination=denomination,
                                        fh=0,
                                        MAPE=TSrepr::mape(result_weight[,1],result_weight[,2]+result_weight[,3]),
                                        RMSE=TSrepr::rmse(result_weight[,1],result_weight[,2]+result_weight[,3]),
                                        linearmodel=linearmodel.candidate,
                                        nonlinearmodel=paste0(length(lag),"-",nonlinearmodel.candidate,"-",1),
                                        preprocessing=preprocessing.candidate,
                                        ID=id,
                                        DateExecuted=dateexecuted,
                                        weightingMethod=weighting,
                                        weightingModel1=weight1,
                                        weightingModel2=weight2))
      
      for (fh in 1:24) {
        frc.mlp<-forecast(mlp.model,h=fh,xreg = as.data.frame(xreg_xts))
        frc.arima<-forecast(arima.model,xreg = xreg_data$test)$mean[1:fh]
        
        mlp.mean<-frc.mlp$mean%>%InvBoxCox(lambda=lambda) 
        arima.mean<-frc.arima%>%InvBoxCox(lambda=lambda) 
        test.data<-train_test_data$test[1:fh]%>%InvBoxCox(lambda=lambda)
        
        result_pred_weight<-ts.intersect(test.data,weight1*mlp.mean,weight2*arima.mean)
        colnames(result_pred_weight)<-c("train_data","mlp_fitted","arima_fitted")
        
        compile<-rbind(compile,data.frame(Flow=flow,
                                          Model=if (MLP_layer==1) "ARIMAX-FFNNX-Paralel (OLS)" else "ARIMAX-DLNNX-Paralel (OLS)",
                                          InOutSample="Out Sample",
                                          Location=location,
                                          Denomination=denomination,
                                          fh=fh,
                                          MAPE=TSrepr::mape(result_pred_weight[,1],result_pred_weight[,2]+result_pred_weight[,3]),
                                          RMSE=TSrepr::rmse(result_pred_weight[,1],result_pred_weight[,2]+result_pred_weight[,3]),
                                          linearmodel=linearmodel.candidate,
                                          nonlinearmodel=paste0(length(lag),"-",nonlinearmodel.candidate,"-",1),
                                          preprocessing=preprocessing.candidate,
                                          ID=id,
                                          DateExecuted=dateexecuted,
                                          weightingMethod=weighting,
                                          weightingModel1=weight1,
                                          weightingModel2=weight2))
        
      }
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
      
      compile<-rbind(compile,data.frame(Flow=flow,
                                        Model=if (MLP_layer==1) "ARIMAX-FFNNX-Paralel (GA)" else "ARIMAX-DLNNX-Paralel (GA)",
                                        InOutSample="In Sample",
                                        Location=location,
                                        Denomination=denomination,
                                        fh=0,
                                        MAPE=TSrepr::mape(result_weight[,1],result_weight[,2]+result_weight[,3]),
                                        RMSE=TSrepr::rmse(result_weight[,1],result_weight[,2]+result_weight[,3]),
                                        linearmodel=linearmodel.candidate,
                                        nonlinearmodel=paste0(length(lag),"-",nonlinearmodel.candidate,"-",1),
                                        preprocessing=preprocessing.candidate,
                                        ID=id,
                                        DateExecuted=dateexecuted,
                                        weightingMethod=weighting,
                                        weightingModel1=weight1,
                                        weightingModel2=weight2))
      
      for (fh in 1:24) {
        frc.mlp<-forecast(mlp.model,h=fh,xreg = as.data.frame(xreg_xts))
        frc.arima<-forecast(arima.model,xreg = xreg_data$test)$mean[1:fh]
        
        mlp.mean<-frc.mlp$mean%>%InvBoxCox(lambda=lambda) 
        arima.mean<-frc.arima%>%InvBoxCox(lambda=lambda) 
        test.data<-train_test_data$test[1:fh]%>%InvBoxCox(lambda=lambda) 
        
        result_pred_weight<-ts.intersect(test.data,weight1*mlp.mean,weight2*arima.mean)
        colnames(result_pred_weight)<-c("train_data","mlp_fitted","arima_fitted")
        
        compile<-rbind(compile,data.frame(Flow=flow,
                                          Model=if (MLP_layer==1) "ARIMAX-FFNNX-Paralel (GA)" else "ARIMAX-DLNNX-Paralel (GA)",
                                          InOutSample="Out Sample",
                                          Location=location,
                                          Denomination=denomination,
                                          fh=fh,
                                          MAPE=TSrepr::mape(result_pred_weight[,1],result_pred_weight[,2]+result_pred_weight[,3]),
                                          RMSE=TSrepr::rmse(result_pred_weight[,1],result_pred_weight[,2]+result_pred_weight[,3]),
                                          linearmodel=linearmodel.candidate,
                                          nonlinearmodel=paste0(length(lag),"-",nonlinearmodel.candidate,"-",1),
                                          preprocessing=preprocessing.candidate,
                                          ID=id,
                                          DateExecuted=dateexecuted,
                                          weightingMethod=weighting,
                                          weightingModel1=weight1,
                                          weightingModel2=weight2))
        
      }
      
    }
  }
  
  return(list("modelResult"=compile,"gridsearchNN"=gridsearchNN))
  
  
  
  
}


