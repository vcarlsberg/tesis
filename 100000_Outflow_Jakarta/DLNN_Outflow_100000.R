library(lubridate)
library(scales)
library(plot.matrix)
library(reshape2)
source("all_function.R")
init_run()


flow_data<-read_data("Jakarta","K100000","Outflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                         frequency=12)

dlnn_gridsearch<-data.frame(HiddenNodes1=numeric(),
                           HiddenNodes2=numeric(),
                           InputNodes=numeric(),
                           InSampleRMSE=numeric(),
                           InSampleMAPE=numeric(),
                           OutSampleRMSE=numeric(),
                           OutSampleMAPE=numeric()
)

set.seed(72)

# Gridsearch tiap input node, layer1 & llayer 2 node
for(nn1 in c(1:2))
{
  for(nn2 in c(1:2))
  {
    for(input in c(1:2))
    {
      dlnn.model<-mlp(split_data(flow_data_xts,20)$train,
                     hd=c(nn1,nn2),
                     difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
                     reps = 1,
                     lags = 1:input,
                     sel.lag = FALSE)
      
      dlnn.frc<-forecast(dlnn.model,h=47)$mean
      
      intersect.datatrain.dlnnfit<-ts.intersect(split_data(flow_data_xts,20)$train,
                                               dlnn.model$fitted)
      intersect.datatest.dlnnpred<-ts.intersect(split_data(flow_data_xts,20)$test,
                                               dlnn.frc)
      
      result.df<-data.frame(HiddenNodes1=nn1,
                            HiddenNodes2=nn2,
                            InputNodes=input,
                            InSampleRMSE=TSrepr::rmse(intersect.datatrain.dlnnfit[,1],intersect.datatrain.dlnnfit[,2]),
                            InSampleMAPE=TSrepr::mape(intersect.datatrain.dlnnfit[,1],intersect.datatrain.dlnnfit[,2]),
                            OutSampleRMSE=TSrepr::rmse(intersect.datatest.dlnnpred[,1],intersect.datatest.dlnnpred[,2]),
                            OutSampleMAPE=TSrepr::mape(intersect.datatest.dlnnpred[,1],intersect.datatest.dlnnpred[,2])
                            )
      
      dlnn_gridsearch<-rbind(dlnn_gridsearch,result.df)
      
      
    }
  }
}
