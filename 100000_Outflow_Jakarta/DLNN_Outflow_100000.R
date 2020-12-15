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
                           InSampleRMSE=numeric(),
                           InSampleMAPE=numeric(),
                           OutSampleRMSE=numeric(),
                           OutSampleMAPE=numeric()
)

set.seed(72)

# Gridsearch tiap input node, layer1 & llayer 2 node
for(nn1 in c(1:20))
{
  for(nn2 in c(1:20))
  {
      print(paste(nn1,nn2))
      tryCatch({
        dlnn.model<-mlp(split_data(flow_data_xts,20)$train,
                        hd=c(nn1,nn2),
                        difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
                        reps = 1,
                        lags = c(1,12,13,24,25,36,37),
                        sel.lag = FALSE)
        
        dlnn.frc<-forecast(dlnn.model,h=47)$mean
        
        intersect.datatrain.dlnnfit<-ts.intersect(split_data(flow_data_xts,20)$train,
                                                  dlnn.model$fitted)
        intersect.datatest.dlnnpred<-ts.intersect(split_data(flow_data_xts,20)$test,
                                                  dlnn.frc)
        
        result.df<-data.frame(HiddenNodes1=nn1,
                              HiddenNodes2=nn2,
                              InSampleRMSE=TSrepr::rmse(intersect.datatrain.dlnnfit[,1],intersect.datatrain.dlnnfit[,2]),
                              InSampleMAPE=TSrepr::mape(intersect.datatrain.dlnnfit[,1],intersect.datatrain.dlnnfit[,2]),
                              OutSampleRMSE=TSrepr::rmse(intersect.datatest.dlnnpred[,1],intersect.datatest.dlnnpred[,2]),
                              OutSampleMAPE=TSrepr::mape(intersect.datatest.dlnnpred[,1],intersect.datatest.dlnnpred[,2])
        )
        
        dlnn_gridsearch<-rbind(dlnn_gridsearch,result.df)
      },error=function(e){
        print(e)
      })
      
      
      
    
  }
}

dlnn.model<-mlp(split_data(flow_data_xts,20)$train,
                hd=c(2,3),
                difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
                reps = 1,
                lags = c(1,12,13,24,25,36,37),
                sel.lag = FALSE)
dlnn.model$MSE

#plot insample, outsample data, fitted & forecast data
set.seed(72)
fit_dlnn<-fitted(dlnn.model)
frc_dlnn<-forecast(dlnn.model,h=47)$mean
fit_frc_dlnn<-ts(c(fit_dlnn,frc_dlnn),
                 start=c(2002, 12), 
                 end=c(2019, 6),frequency = 12)


TSrepr::mape(ts.intersect(flow_data_xts,fit_dlnn)[,1],ts.intersect(flow_data_xts,fit_dlnn)[,2])
TSrepr::rmse(ts.intersect(flow_data_xts,fit_dlnn)[,1],ts.intersect(flow_data_xts,fit_dlnn)[,2])
TSrepr::mape(ts.intersect(flow_data_xts,frc_dlnn)[,1],ts.intersect(flow_data_xts,frc_dlnn)[,2])
TSrepr::rmse(ts.intersect(flow_data_xts,frc_dlnn)[,1],ts.intersect(flow_data_xts,frc_dlnn)[,2])



dlnn_gridsearch %>% group_by(HiddenNodes2) %>%
  filter(HiddenNodes2 %in% c(1,3,5,13,15,20) )%>%
  ggplot( aes(x = HiddenNodes1, y = (OutSampleRMSE)/1000,
              group=as.factor(HiddenNodes2),
              col=as.factor(HiddenNodes2)))+
  geom_line(size = 0.8)+
  labs(color = "HiddenNodes2")+
  theme_minimal(base_size=16)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(legend.position="bottom")+
  scale_y_continuous(name="Nilai Out-Sample RMSE \n(milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))





plot(dlnn.model$net)
write_csv(as.data.frame(dlnn.model$net$result.matrix),"dlnn.model$net$result.matrix.csv")

#residual analysis
Box.test(flow_data_xts-dlnn.model$fitted,lag = 36)

#cek oos vs fh
df.mape.oos<-data.frame(fh=numeric(),
                        mape=numeric())

for(h in c(1:24))
{
  dlnn.frc<-forecast(dlnn.model,h=h)$mean
  
  intersect_data<-ts.intersect(dlnn.frc,
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

