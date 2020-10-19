source("all_function.R")
init_run()
set.seed(72)

if(!exists("compile")){
  compile <- data.frame(Model=character(),
                        InOutSample=character(),
                        Location=character(),
                        Denomination=character(),
                        fh=numeric(), 
                        MAPE=numeric())
  
}


for(location in c("Jakarta"))
{
  for(denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000","L1000","L500","L200","L100","L50"))
  {
    flow_data<-read_data(location,denomination)
    flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                        frequency=12)
    
    train_test_data<-ts_split(flow_data_xts)
    
    arima.model<-auto.arima(train_test_data$train,seasonal = FALSE)
    arima.model2<-auto.arima(train_test_data$train,lambda = "auto")
    
    mlp.model<-mlp(as.ts(train_test_data$train),hd=c(9),reps = 1,lags = 1:60)
    
    result<-ts.intersect(train_test_data$train,mlp.model$fitted,arima.model$fitted)
    colnames(result)<-c("train_data","mlp_fitted","arima_fitted")
    
    print(paste0("In-sample error : ",location," ",denomination," ",mape(result[,1],(0.5*result[,2]+0.5*result[,3]))))
    
    compile<-rbind(compile,data.frame(Model="ARIMA-MLP_Parallel",
                                      InOutSample="In Sample",
                                      Location=location,
                                      Denomination=denomination,
                                      fh=0,
                                      MAPE=mape(result[,1],(0.5*result[,2]+0.5*result[,3]))
                                      )
                   )
    
    for (fh in 1:24){
      frc.arima<-forecast(arima.model,h=fh)
      frc.mlp<-forecast(mlp.model,h=fh)
      result.pred<-ts.intersect(train_test_data$test[1:fh],frc.arima$mean,frc.mlp$mean)
      colnames(result.pred)<-c("test_data","arima_pred","mlp_pred")
      
      print(paste0("Out-sample error : ",location," ",denomination," ",fh," ",
                   mape(result.pred[,1],0.5*result.pred[,2]+0.5*result.pred[,3])))
      
      compile<-rbind(compile,data.frame(Model="ARIMA-MLP_Parallel",
                                        InOutSample="Out Sample",
                                        Location=location,
                                        Denomination=denomination,
                                        fh=fh,
                                        MAPE=mape(result.pred[,1],(0.5*result.pred[,2]+0.5*result.pred[,3]))
      )
      )
    }

  }
}


