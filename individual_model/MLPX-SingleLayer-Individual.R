source("~/tesis/all_function.R")
init_run()
set.seed(72)

if(!exists("compile")){
  compile <- data.frame(Model=character(),
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


for(location in c("Jakarta"))
{
  for(denomination in c("K100000"))
  {
    flow_data<-read_data(location,denomination)
    flow_data_xts <- ts(flow_data[,3:4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                        frequency=12)
    
    
    train_test_data<-split_data(flow_data_xts,20)
    
    testFun <- function(x)
    {
      mlp.model<-mlp(train_test_data$train[,1],hd=c(x[1]),
                     reps = 1,
                     lags = 1:60,
                     xreg = as.data.frame(flow_data_xts[,2]),
                     xreg.lags=list(0),xreg.keep=list(TRUE))
      mlp.model$MSE
    }
      
    sol <- gridSearch(fun = testFun, levels = list(1:20))  
    
    mlp.model<-mlp(train_test_data$train[,1],hd=c(sol$minlevels),
                   reps = 1,
                   lags = 1:60,
                   xreg = as.data.frame(flow_data_xts[,2]),
                   xreg.lags=list(0),xreg.keep=list(TRUE))
    
    result<-ts.intersect(train_test_data$train,mlp.model$fitted)
    colnames(result)<-c("train_data","external_regressor","mlp_fitted")
    
    print(paste0("in-sample MAPE : ",location," ",denomination," ",
                 mape(result[,1],result[,3])))
    
    compile<-rbind(compile,data.frame(Model="MLPX-SingleLayer-Individual",
                            InOutSample="In Sample",
                            Location=location,
                            Denomination=denomination,
                            fh=0,
                            MAPE=mape(result[,1],result[,3]),
                            RMSE=rmse(result[,1],result[,2]),
                            linearmodel="",
                            nonlinearmodel=(sol$minlevels),
                            preprocessing=""
                            ))
    
     for (fh in 1:24){
       frc.mlp<-forecast(mlp.model,h=fh,
                         xreg = as.data.frame(flow_data_xts[,2]))
       
       result.pred<-ts.intersect(train_test_data$test[1:fh],frc.mlp$mean)
       colnames(result.pred)<-c("test_data","mlp_fitted")
       
       print(paste0("out-sample MAPE : ",location," ",denomination," ",
                    mape(result.pred[,1],result.pred[,2])))
       
       compile<-rbind(compile,data.frame(Model="MLPX-SingleLayer-Individual",
                               InOutSample="Out Sample",
                               Location=location,
                               Denomination=denomination,
                               fh=fh,
                               MAPE=mape(result.pred[,1],result.pred[,2]),
                               RMSE=rmse(result.pred[,1],result.pred[,2]),
                               linearmodel="",
                               nonlinearmodel=(sol$minlevels),
                               preprocessing=""
                               ))
     }

  }
}




