library(lubridate)
library(scales)
library(plot.matrix)
library(reshape2)
source("all_function.R")
init_run()


flow_data<-read_data("Jakarta","K100000","Outflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,c(4:22,24)],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                         frequency=12)

dlnnx_gridsearch<-data.frame(HiddenNodes1=numeric(),
                            HiddenNodes2=numeric(),
                            InSampleRMSE=numeric(),
                            InSampleMAPE=numeric(),
                            OutSampleRMSE=numeric(),
                            OutSampleMAPE=numeric()
)

set.seed(72)

# Gridsearch tiap input node, layer1 & llayer 2 node
for(nn1 in c( 1:20))
{
  for(nn2 in c(1:20))
  {
      print(paste(nn1,nn2))
      tryCatch({
        dlnnx.model<-mlp(split_data(flow_data_xts,20)$train,
                        hd=c(nn1,nn2),
                        difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
                        reps = 1,
                        lags = c(1,2,3,4,5,12,24),
                        sel.lag = FALSE,
                        xreg =as.data.frame(split_data(flow_data_xts_xreg,20)$train),
                        xreg.lags=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                        xreg.keep=c(T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T))
        
        dlnnx.frc<-forecast(dlnnx.model,h=47,
                            xreg = as.data.frame(flow_data_xts_xreg))$mean
        
        intersect.datatrain.dlnnxfit<-ts.intersect(split_data(flow_data_xts,20)$train,
                                                  dlnnx.model$fitted)
        intersect.datatest.dlnnxpred<-ts.intersect(split_data(flow_data_xts,20)$test,
                                                  dlnnx.frc)
        
        result.df<-data.frame(HiddenNodes1=nn1,
                              HiddenNodes2=nn2,
                              InSampleRMSE=TSrepr::rmse(intersect.datatrain.dlnnxfit[,1],intersect.datatrain.dlnnxfit[,2]),
                              InSampleMAPE=TSrepr::mape(intersect.datatrain.dlnnxfit[,1],intersect.datatrain.dlnnxfit[,2]),
                              OutSampleRMSE=TSrepr::rmse(intersect.datatest.dlnnxpred[,1],intersect.datatest.dlnnxpred[,2]),
                              OutSampleMAPE=TSrepr::mape(intersect.datatest.dlnnxpred[,1],intersect.datatest.dlnnxpred[,2])
        )
        
        dlnnx_gridsearch<-rbind(dlnnx_gridsearch,result.df)
      },error=function(e){
        print(e)
      })
      
      
      
    
  }
}



dlnn_gridsearch %>% group_by(HiddenNodes2) %>%
  filter(InputNodes==c(20) & HiddenNodes2 %in% c(1,5,9,10,15,17,18,20) )%>%
  ggplot( aes(x = HiddenNodes1, y = (InSampleRMSE)/1000,
              group=as.factor(HiddenNodes2),
              col=as.factor(HiddenNodes2)))+
  geom_line(size = 1)+
  labs(color = "HiddenNodes2")+
  theme(text = element_text(size=14))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(legend.position="bottom")+
  scale_y_continuous(name="Nilai In-Sample RMSE \n(milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))


#extract weight
set.seed(72)
dlnnx.model<-mlp(split_data(flow_data_xts,20)$train,
                 hd=c(2,7),
                 difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
                 reps = 1,
                 lags = c(1,2,3,4,5,12,24),
                 sel.lag = FALSE,
                 xreg =as.data.frame(split_data(flow_data_xts_xreg,20)$train),
                 xreg.lags=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                 xreg.keep=c(T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T)
)

plot(dlnnx.model$net)
fit_dlnnx<-fitted(dlnnx.model)
frc_dlnnx<-forecast(dlnnx.model,h=47,
                    xreg = as.data.frame(flow_data_xts_xreg))$mean
fit_frc_dlnnx<-ts(c(fit_dlnnx,frc_dlnnx),
                  start=c(2001, 11), 
                  end=c(2019, 6),frequency = 12)

intersect.datatrain.dlnnxfit<-ts.intersect(split_data(flow_data_xts,20)$train,
                                           dlnnx.model$fitted)
intersect.datatest.dlnnxpred<-ts.intersect(split_data(flow_data_xts,20)$test,
                                           forecast(dlnnx.model,
                                                    h=47,
                                                    xreg = as.data.frame(flow_data_xts_xreg))$mean)

TSrepr::rmse(intersect.datatrain.dlnnxfit[,1],intersect.datatrain.dlnnxfit[,2])
TSrepr::mape(intersect.datatrain.dlnnxfit[,1],intersect.datatrain.dlnnxfit[,2])
TSrepr::rmse(intersect.datatest.dlnnxpred[,1],intersect.datatest.dlnnxpred[,2])
TSrepr::mape(intersect.datatest.dlnnxpred[,1],intersect.datatest.dlnnxpred[,2])

dlnnx.model$net$result.matrix %>% View()

#plot ts & fitted value & forecast
ts.intersect(flow_data_xts,fit_frc_dlnnx) %>% 
  data.frame() %>% 
  cbind(date=index(fit_frc_dlnnx)%>%yearmon()) %>%
  rename(Outflow=flow_data_xts,Predicted=fit_frc_dlnnx)%>%
  mutate(Outflow=Outflow/1000,
         Predicted=Predicted/1000)%>%
  gather(key="variable",value="value",-date)%>%
  ggplot( aes(x = date, y = value))+theme_minimal()+
  geom_line(aes(color = variable), size = 0.75)+
  geom_rect(fill="grey",xmin=2015.6666,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.01)+
  scale_x_yearmon(format="%b-%Y",breaks=pretty_breaks(20))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))+
  scale_y_continuous(name="Nilai Outflow \n (milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  annotate("text", x = 2018, y = 25000, label = "Out-of-Sample")+
  annotate("text", x = 2005, y = 25000, label = "In-Sample")+
  xlab("Bulan-Tahun")+
  theme(text = element_text(size=14))

#plot ts & fitted value & forecast
ts.intersect(flow_data_xts,fit_frc_dlnnx,fit_frc_dlnn,index(fit_frc_dlnn)%>%yearmon()) %>% 
  data.frame() %>% 
  `colnames<-`(c("Outflow", "Predicted_DLNNX", "Predicted_DLNN","date")) %>%
  mutate(Outflow=Outflow/1000,
         Predicted_DLNN=Predicted_DLNN/1000,
         Predicted_DLNNX=Predicted_DLNNX/1000)%>%
  gather(key="variable",value="value",-date)%>%
  ggplot( aes(x = date, y = value))+theme_minimal()+
  geom_line(aes(color = variable), size = 0.75)+
  geom_rect(fill="grey",xmin=2015.6666,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.01)+
  scale_x_yearmon(format="%b-%Y",breaks=pretty_breaks(20))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))+
  scale_y_continuous(name="Nilai Outflow \n (milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  annotate("text", x = 2018, y = 25000, label = "Out-of-Sample")+
  annotate("text", x = 2004, y = 25000, label = "In-Sample")+
  xlab("Bulan-Tahun")+
  theme(text = element_text(size=14))


plot(dlnnx.model$net)
write_csv(as.data.frame(dlnnx.model$net$result.matrix),"dlnn.model$net$result.matrix.csv")

#residual analysis
Box.test(flow_data_xts-dlnnx.model$fitted,lag = 30)

#cek oos vs fh
df.mape.oos<-data.frame(fh=numeric(),
                        mape=numeric())

for(h in c(1:24))
{
  intersect_data<-ts.intersect(forecast(dlnnx.model,h=h,
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


#plot insample, outsample data, fitted & forecast data
fit_dlnn<-fitted(dlnn.model)
frc_dlnn<-forecast(dlnn.model,h=47)$mean
fit_frc_dlnn<-ts(c(fit_dlnn,frc_dlnn),
                 start=c(2001, 2), 
                 end=c(2019, 6),frequency = 12)

ts.intersect(flow_data_xts,fit_frc_dlnn) %>% 
  data.frame() %>% 
  cbind(date=index(fit_frc_dlnn)%>%yearmon()) %>%
  rename(Outflow=flow_data_xts,Predicted=fit_frc_dlnn)%>%
  mutate(Outflow=Outflow/1000,Predicted=Predicted/1000)%>%
  gather(key="variable",value="value",-date)%>%
  ggplot( aes(x = date, y = value))+theme_minimal()+
  geom_line(aes(color = variable), size = 0.75)+
  geom_rect(fill="grey",xmin=2015.6666,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.01)+
  scale_x_yearmon(format="%b-%Y",breaks=pretty_breaks(20))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))+
  scale_y_continuous(name="Nilai Outflow \n(milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  annotate("text", x = 2018, y = 25000, label = "Out-of-Sample")+
  annotate("text", x = 2004, y = 25000, label = "In-Sample")+
  xlab("Bulan-Tahun")+
  theme(text = element_text(size=14))

