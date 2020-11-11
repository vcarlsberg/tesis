library(lubridate)
library(scales)
library(plot.matrix)
library(reshape2)
init_run()
source("all_function.R")

flow_data<-read_data("Jakarta","K100000","Outflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                         frequency=12)

mlp_gridsearch<-data.frame(HiddenNodes=numeric(),
                           InputNodes=numeric(),
                           InSampleRMSE=numeric(),
                           InSampleMAPE=numeric(),
                           OutSampleRMSE=numeric(),
                           OutSampleMAPE=numeric()
                           )
set.seed(72)
for(nn in c(1:20))
{
  for(input in c(1:20))
  {
    mlp.model<-mlp(split_data(flow_data_xts,20)$train,
                   hd=c(nn),
                   difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
                   reps = 1,
                   lags = 1:input,
                   sel.lag = FALSE)
    
    mlp.frc<-forecast(mlp.model,h=47)$mean
    
    intersect.datatrain.mlpfit<-ts.intersect(split_data(flow_data_xts,20)$train,
                                            mlp.model$fitted)
    intersect.datatest.mlppred<-ts.intersect(split_data(flow_data_xts,20)$test,
                                             mlp.frc)
    
    
    
    result.df<-data.frame(HiddenNodes=nn,
                          InputNodes=input,
                          InSampleRMSE=TSrepr::rmse(intersect.datatrain.mlpfit[,1],intersect.datatrain.mlpfit[,2]),
                          InSampleMAPE=TSrepr::mape(intersect.datatrain.mlpfit[,1],intersect.datatrain.mlpfit[,2]),
                          OutSampleRMSE=TSrepr::rmse(intersect.datatest.mlppred[,1],intersect.datatest.mlppred[,2]),
                          OutSampleMAPE=TSrepr::mape(intersect.datatest.mlppred[,1],intersect.datatest.mlppred[,2])
                          )
    
    mlp_gridsearch<-rbind(mlp_gridsearch,result.df)
  }
}

which(mlp_gridsearch ==min(mlp_gridsearch), 
      arr.ind = TRUE)

plot(mlp_gridsearch)

melt_mlp_gridsearch<-melt(mlp_gridsearch)
colnames(melt_mlp_gridsearch)<-c("HiddenNodes","InputNodes","Error")

melt_mlp_gridsearch %>% 
  ggplot( aes(x = HiddenNodes, y = sqrt(Error)/1000,
              group=as.factor(InputNodes),
              col=as.factor(InputNodes)))+
  geom_line(size = 1.5)+
  scale_y_continuous(name="Nilai RMSE (milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(color = "InputNodes")+
  theme(text = element_text(size=14))
  


mlp.model<-mlp(split_data(flow_data_xts,20)$train,
               hd=c(20),
               difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
               reps = 1,
               lags = 1:12,sel.lag = FALSE)
plot(mlp.model$net)
forecast(mlp.model,h=47)
