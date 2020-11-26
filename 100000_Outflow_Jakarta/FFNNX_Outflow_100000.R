library(lubridate)
library(scales)
library(plot.matrix)
library(reshape2)
source("all_function.R")
init_run()


flow_data<-read_data("Jakarta","K100000","Outflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,4:11],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
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
                   xreg =as.data.frame(split_data(flow_data_xts_xreg,20)$train),
                   xreg.lags=list(0,0,0,0,0,0,0,0),
                   xreg.keep=list(T,T,T,T,T,T,T,T),
                   reps = 1,
                   lags = 1:input,
                   sel.lag = FALSE)
    
    mlp.frc<-forecast(mlp.model,h=47,
                      xreg = as.data.frame(flow_data_xts_xreg))
    
    intersect.datatrain.mlpfit<-ts.intersect(split_data(flow_data_xts,20)$train,
                                             mlp.model$fitted)
    intersect.datatest.mlppred<-ts.intersect(split_data(flow_data_xts,20)$test,
                                             mlp.frc$mean)
    
    
    
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

plot(mlp_gridsearch)

mlp_gridsearch %>% filter(InputNodes==c(1,10,15,20))%>%
  ggplot( aes(x = HiddenNodes, y = (OutSampleRMSE)/1000,
              group=as.factor(InputNodes),
              col=as.factor(InputNodes)))+
  geom_line(size = 1.5)+
  scale_y_continuous(name="Nilai Out-Sample RMSE (milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(color = "InputNodes")+
  theme(text = element_text(size=14))



set.seed(72)
ffnnx.model<-mlp(split_data(flow_data_xts,20)$train,
               hd=c(1),
               difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
               reps = 1,
               lags = c(1,12,13,23,24,25,35,36,48,49),
               sel.lag = FALSE,
               xreg =as.data.frame(split_data(flow_data_xts_xreg,20)$train),
               xreg.lags=c(0,0,0,0,0,0,0,0),
               xreg.keep=c(T,T,T,T,T,T,T,T),
               )
fit_ffnnx<-fitted(ffnnx.model)
frc_ffnnx<-forecast(ffnnx.model,h=47,
                   xreg = as.data.frame(flow_data_xts_xreg))$mean
fit_frc_ffnnx<-ts(c(fit_ffnnx,frc_ffnnx),
                 start=c(2003, 12), 
                 end=c(2019, 6),frequency = 12)

plot(ffnnx.model$net)

intesect_train<-ts.intersect(flow_data_xts,fit_ffnnx)
TSrepr::mape(ts.intersect(flow_data_xts,fit_ffnnx)[,1],ts.intersect(flow_data_xts,fit_ffnnx)[,2])
TSrepr::rmse(ts.intersect(flow_data_xts,fit_ffnnx)[,1],ts.intersect(flow_data_xts,fit_ffnnx)[,2])
TSrepr::mape(ts.intersect(flow_data_xts,frc_ffnnx)[,1],ts.intersect(flow_data_xts,frc_ffnnx)[,2])
TSrepr::rmse(ts.intersect(flow_data_xts,frc_ffnnx)[,1],ts.intersect(flow_data_xts,frc_ffnnx)[,2])

plot(ffnnx.model$net)
ffnnx.model$net$result.matrix %>% View()

checkresiduals(flow_data_xts-ffnnx.model$fitted)

Box.test(flow_data_xts-mlp.model$fitted,lag = 30)


compile_ffnnx<-ts.intersect(flow_data_xts,fit_frc_ffnnx) %>% 
  data.frame() %>% 
  cbind(date=index(fit_frc_ffnnx)%>%yearmon()) %>%
  rename(Outflow=flow_data_xts,Predicted=fit_frc_ffnnx)%>%
  mutate(Outflow=Outflow/1000,Predicted=Predicted/1000)%>%
  gather(key="variable",value="value",-date)%>%
  ggplot( aes(x = date, y = value))+theme_minimal(base_size=16)+
  geom_line(aes(color = variable), size = 0.75)+
  geom_rect(fill="grey",xmin=2015.6666,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.01)+
  scale_x_yearmon(format="%b-%Y",breaks=pretty_breaks(20))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))+
  scale_y_continuous(name="Nilai RMSE Outflow \n Out-of-Sample (milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  annotate("text", x = 2018, y = 25000, label = "Out-of-Sample")+
  annotate("text", x = 2004, y = 25000, label = "In-Sample")+
  xlab("Bulan-Tahun")
compile_ffnnx

#PLOT FFNN & FFNNX
compile_ffnnx<-ts.intersect(flow_data_xts,fit_frc_ffnnx,fit_frc_ffnn) %>% 
  data.frame() %>% 
  cbind(date=index(fit_frc_ffnnx)%>%yearmon()) %>%
  rename(Outflow=flow_data_xts,Predicted_FFNNX=fit_frc_ffnnx,Predicted_FFNN=fit_frc_ffnn)%>%
  mutate(Outflow=Outflow/1000,Predicted_FFNNX=Predicted_FFNNX/1000,Predicted_FFNN=Predicted_FFNN/1000)%>%
  gather(key="variable",value="value",-date)%>%
  ggplot( aes(x = date, y = value))+theme_minimal(base_size=16)+
  geom_line(aes(color = variable), size = 0.75)+
  geom_rect(fill="grey",xmin=2015.6666,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.01)+
  scale_x_yearmon(format="%b-%Y",breaks=pretty_breaks(20))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))+
  scale_y_continuous(name="Nilai RMSE Outflow \n Out-of-Sample (milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  annotate("text", x = 2018, y = 25000, label = "Out-of-Sample")+
  annotate("text", x = 2004, y = 25000, label = "In-Sample")+
  xlab("Bulan-Tahun")
compile_ffnnx

df.mape.oos<-data.frame(fh=numeric(),
                        mape=numeric())

for(h in c(1:24))
{
  intersect_data<-ts.intersect(forecast(ffnnx.model,h=h,
                                        xreg = as.data.frame(flow_data_xts_xreg))$mean,
                               split_data(flow_data_xts,20)$test[1:h])
  
  df.mape.oos<-rbind(df.mape.oos,data.frame(fh=h,
                                            mape=TSrepr::mape(intersect_data[,2],
                                                              intersect_data[,1])
  )
  )
}

df.mape.oos %>% mutate(predicate=case_when(
  mape<10 ~ "Akurasi Tinggi",
  mape>=10 & mape<=20 ~ "Baik",
  mape>20 & mape<=50 ~ "Cukup",
  mape>50  ~ "Tidak Akurat"
))%>% ggplot(aes(x=fh,y=mape,color=factor(predicate))) + geom_path(aes(group=2),size=1)+
  theme_minimal(base_size=16)+
  xlab("Forecast Horizon")+ylab("MAPE (%)")+ 
  theme(legend.position = "top")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  labs(color='Predikat Akurasi Peramalan') 

