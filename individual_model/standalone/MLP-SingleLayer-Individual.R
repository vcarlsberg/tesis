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
    
    testFun <- function(x)
    {
      mlp.model<-mlp(train_test_data$train,hd=c(x[1]),
                     reps = 1,
                     lags = 1:60)
      mlp.model$MSE
    }
    
    sol <- gridSearch(fun = testFun, levels = list(1:20))
    sol$values
    head(sol$levels)
    
    
    mlp.model<-mlp(train_test_data$train,hd=c(sol$minlevels),
                   reps = 1,
                   lags = 1:60)
    
    result<-ts.intersect(train_test_data$train,mlp.model$fitted)
    colnames(result)<-c("train_data","mlp_fitted")
    
    print(paste0("in-sample MAPE : ",location," ",denomination," ",
                 mape(result[,1],result[,2])))
    
    compile<-rbind(compile,data.frame(Model="MLP-SingleLayer-Individual",
                            InOutSample="In Sample",
                            Location=location,
                            Denomination=denomination,
                            fh=0,
                            MAPE=mape(result[,1],result[,2]),
                            RMSE=rmse(result[,1],result[,2]),
                            linearmodel="",
                            nonlinearmodel=paste(sol$minlevels[1]),
                            preprocessing="",
                            ID=id,
                            DateExecuted=dateexecuted
                            ))
    
    for (fh in 1:24) {
      frc.mlp<-forecast(mlp.model,h=fh)
      result.pred<-ts.intersect(train_test_data$test[1:fh],frc.mlp$mean)
      colnames(result.pred)<-c("test_data","mlp_fitted")
      
      print(paste0("out-sample MAPE : ",location," ",denomination," ",fh," ",
                   mape(result.pred[,1],result.pred[,2])))
      
      compile<-rbind(compile,data.frame(Model="MLP-SingleLayer-Individual",
                              InOutSample="Out Sample",
                              Location=location,
                              Denomination=denomination,
                              fh=fh,
                              MAPE=mape(result.pred[,1],result.pred[,2]),
                              RMSE=rmse(result.pred[,1],result.pred[,2]),
                              linearmodel="",
                              nonlinearmodel=paste(sol$minlevels[1]),
                              preprocessing="",
                              ID=id,
                              DateExecuted=dateexecuted
                              ))
    }

  }
}



