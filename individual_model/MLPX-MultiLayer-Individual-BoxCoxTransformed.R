
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
    flow_data_xts <- ts(flow_data[,3:4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                        frequency=12)
    
    lambda<-BoxCox.lambda(flow_data_xts[,1])
    flow_data_xts_transformed<-ts.intersect(BoxCox(flow_data_xts[,1],lambda = lambda),
                                            flow_data_xts[,2])
    colnames(flow_data_xts_transformed)<-colnames(flow_data_xts)
    
    train_test_data<-split_data(flow_data_xts_transformed,20)
    
    testFun <- function(x)
    {
      mlp.model<-mlp(train_test_data$train[,1],hd=c(x[1],x[2]),
                     reps = 1,
                     lags = 1:60,
                     xreg = as.data.frame(flow_data_xts[,2]),
                     xreg.lags=list(0),xreg.keep=list(TRUE))
      mlp.model$MSE
    }
    
    sol <- gridSearch(fun = testFun, levels = list(1:20,1:20))  
    
    mlp.model<-mlp(train_test_data$train[,1],hd=c(sol$minlevels),
                   reps = 1,
                   lags = 1:60,
                   xreg = as.data.frame(flow_data_xts[,2]),
                   xreg.lags=list(0),xreg.keep=list(TRUE))
    
    result<-ts.intersect(train_test_data$train,mlp.model$fitted)
    result[,1]<-result[,1] %>% InvBoxCox(lambda=lambda)
    result[,3]<-result[,3] %>% InvBoxCox(lambda=lambda)
    colnames(result)<-c("train_data","external_regressor","mlp_fitted")
    
    print(paste0("in-sample MAPE : ",location," ",denomination," ",
                 mape(result[,1],result[,3])))
    
    compile<-rbind(compile,data.frame(Model="MLPX-MultiLayer-Individual-BoxCoxTransformed",
                            InOutSample="In Sample",
                            Location=location,
                            Denomination=denomination,
                            fh=0,
                            MAPE=mape(result[,1],result[,3]),
                            RMSE=rmse(result[,1],result[,2]),
                            linearmodel="",
                            nonlinearmodel=paste(sol$minlevels[1],sol$minlevels[2],sep = "-"),
                            preprocessing=paste("Box-Cox",lambda),
                            ID=id,
                            DateExecuted=dateexecuted
                            ))
    
     for (fh in 1:24){
       frc.mlp<-forecast(mlp.model,h=fh,
                         xreg = as.data.frame(flow_data_xts[,2]))
       
       result.pred<-ts.intersect(train_test_data$test[1:fh],frc.mlp$mean) %>% InvBoxCox(lambda=lambda)
       colnames(result.pred)<-c("test_data","mlp_fitted")
       
       print(paste0("out-sample MAPE : ",location," ",denomination," ",
                    mape(result.pred[,1],result.pred[,2])))
       
       compile<-rbind(compile,data.frame(Model="MLPX-MultiLayer-Individual-BoxCoxTransformed",
                               InOutSample="Out Sample",
                               Location=location,
                               Denomination=denomination,
                               fh=fh,
                               MAPE=mape(result.pred[,1],result.pred[,2]),
                               RMSE=rmse(result.pred[,1],result.pred[,2]),
                               linearmodel="",
                               nonlinearmodel=paste(sol$minlevels[1],sol$minlevels[2],sep = "-"),
                               preprocessing=paste("Box-Cox",lambda),
                               ID=id,
                               DateExecuted=dateexecuted
                               ))
     }

  }
}


