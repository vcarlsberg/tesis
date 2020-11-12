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

mlp_gridsearch<-data.frame(HiddenNodes=numeric(),
                           InputNodes=numeric(),
                           InSampleRMSE=numeric(),
                           InSampleMAPE=numeric(),
                           OutSampleRMSE=numeric(),
                           OutSampleMAPE=numeric()
                           )
set.seed(72)
for(nn in c(4))
{
  for(input in c(15))
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

#melt_mlp_gridsearch<-melt(mlp_gridsearch)
#colnames(melt_mlp_gridsearch)<-c("HiddenNodes","InputNodes","Error")

mlp_gridsearch %>% filter(InputNodes==c(1,10,15,20))%>%
  ggplot( aes(x = HiddenNodes, y = (OutSampleRMSE)/1000,
              group=as.factor(InputNodes),
              col=as.factor(InputNodes)))+
  geom_line(size = 1.5)+
  scale_y_continuous(name="Nilai Out-Sample RMSE (milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(color = "InputNodes")+
  theme(text = element_text(size=14))
  
  


mlp.model<-mlp(split_data(flow_data_xts,20)$train,
               hd=c(20),
               difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
               reps = 1,
               lags = 1:20,sel.lag = FALSE)
fit_ffnn<-fitted(mlp.model)
frc_ffnn<-forecast(mlp.model,h=47)$mean
fit_frc_ffnn<-ts(c(fit_ffnn,frc_ffnn),
                 start=c(2001, 2), 
                 end=c(2019, 6),frequency = 12)

checkresiduals(flow_data_xts-mlp.model$fitted)

Box.test(flow_data_xts-mlp.model$fitted,lag = 30)


compile_ffnn<-ts.intersect(flow_data_xts,fit_frc_ffnn) %>% 
  data.frame() %>% 
  cbind(date=index(fit_frc_ffnn)%>%yearmon()) %>%
  rename(Outflow=flow_data_xts,Predicted=fit_frc_ffnn)%>%
  mutate(Outflow=Outflow/1000,Predicted=Predicted/1000)%>%
  gather(key="variable",value="value",-date)%>%
  ggplot( aes(x = date, y = value))+theme_minimal()+
  geom_line(aes(color = variable), size = 0.75)+
  geom_rect(fill="grey",xmin=2015.6666,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.01)+
  scale_x_yearmon(format="%b-%Y",breaks=pretty_breaks(20))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))+
  scale_y_continuous(name="Nilai Outflow (milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  annotate("text", x = 2018, y = 25000, label = "Out-of-Sample")+
  annotate("text", x = 2004, y = 25000, label = "In-Sample")+
  xlab("Bulan-Tahun")+
  theme(text = element_text(size=14))

compile_ffnn


plot(mlp.model)
View(mlp.model$net$result.matrix)
forecast(mlp.model,h=47)

df.mape.oos<-data.frame(fh=numeric(),
                        mape=numeric())

for(h in c(1:24))
{
  intersect_data<-ts.intersect(forecast(mlp.model,h = h)$mean,
                               split_data(flow_data_xts,20)$test[1:h])
                               
  df.mape.oos<-rbind(df.mape.oos,data.frame(fh=h,
                                            mape=TSrepr::mape(intersect_data[,2],
                                                              intersect_data[,1])
                                            )
  )
}

df.mape.oos %>% ggplot(aes(x=fh,y=mape)) + geom_line(size=1) +theme_minimal()+
  xlab("Forecast Horizon")+ylab("MAPE (%)")+ theme(text = element_text(size=16))

