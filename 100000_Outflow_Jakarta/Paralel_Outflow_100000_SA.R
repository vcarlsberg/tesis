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

set.seed(72)
dlnn.model<-mlp(split_data(flow_data_xts,20)$train,
                hd=c(10,12),
                difforder = 0,outplot = TRUE,retrain = TRUE,allow.det.season = FALSE,
                reps = 1,
                lags = 1:2,
                sel.lag = FALSE)
fit_dlnn<-fitted(dlnn.model)
frc_dlnn<-forecast(dlnn.model,h=47)$mean

fit_frc_dlnn<-ts(c(fit_dlnn,frc_dlnn),
                  start=c(2000, 1), 
                  end=c(2019, 6),frequency = 12)


arimax_indiv<-Arima(split_data(flow_data_xts,20)$train,
                    xreg = split_data(flow_data_xts_xreg,20)$train,
                    order=c(0,1,2),seasonal = c(1,0,0))
fit_arimax<-fitted(arimax_indiv)
frc_arimax<-forecast(arimax_indiv,h=47,
                     xreg = split_data(flow_data_xts_xreg,20)$test)$mean
fit_frc_arimax<-ts(c(fit_arimax,frc_arimax),
                 start=c(1999, 11), 
                 end=c(2019, 6),frequency = 12)

weight_kecil<-function(w1,w2) 
{
  Metrics::sse(raw_dlnn_arimax_train[,1],
      w1*na.omit(raw_dlnn_arimax_train[,2])+
        w2*na.omit(raw_dlnn_arimax_train[,3]))
}

set.seed(72)
GA <- ga(type = "real-valued",pmutation=0.01,
         fitness = function(w) -weight_kecil(w[1],w[2]),
         lower =c(-2,-2), upper = c(2,2),
         maxiter=100,parallel=TRUE,seed=72,monitor = FALSE)

plot(GA)
GA@solution

Box.test(raw_dlnn_arimax_train[,1]-raw_dlnn_arimax_train[,2],lag = 30)

raw_dlnn_arimax_train<-ts.intersect(flow_data_xts,0.053*fit_dlnn+1.061*fit_arimax)
TSrepr::mape(raw_dlnn_arimax_train[,1],raw_dlnn_arimax_train[,2])
TSrepr::rmse(raw_dlnn_arimax_train[,1],raw_dlnn_arimax_train[,2])
raw_dlnn_arimax_test<-ts.intersect(flow_data_xts,0.744*frc_dlnn+0.281*frc_arimax)
TSrepr::mape(raw_dlnn_arimax_test[,1],raw_dlnn_arimax_test[,2])
TSrepr::rmse(raw_dlnn_arimax_test[,1],raw_dlnn_arimax_test[,2])

raw_dlnn_arimax<-ts.intersect(flow_data_xts,0.744*fit_frc_dlnn,0.281*fit_frc_arimax)
raw_dlnn_arimax<-raw_dlnn_arimax %>% data.frame()
colnames(raw_dlnn_arimax)<-c("Outflow","DLNN","ARIMAX")

lm(Outflow~.+0,data = raw_dlnn_arimax) %>% summary()


raw_dlnn_arimax%>%
  cbind(date=index(fit_frc_dlnn)%>%yearmon()) %>%
  mutate(Hybrid=(DLNN+ARIMAX)/1000,
         DLNN=DLNN/1000,ARIMAX=ARIMAX/1000,
         Outflow=Outflow/1000)%>%
  select(c("Outflow","date","Hybrid"))%>%
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

#combined forecast
df.mape.oos<-data.frame(fh=numeric(),
                        mape=numeric())

for(h in c(1:24))
{
  frc_dlnn<-forecast(dlnn.model,h=h)$mean
  frc_arimax<-forecast(arimax_indiv,h=h,
                       xreg = split_data(flow_data_xts_xreg,20)$test)$mean
  
  intersect_data<-ts.intersect(frc_dlnn+frc_arimax,
                               split_data(flow_data_xts,20)$test[1:h])
  df.mape.oos<-rbind(df.mape.oos,data.frame(fh=h,
                                            mape=TSrepr::mape(intersect_data[,2],
                                                              intersect_data[,1]))
  )
}

df.mape.oos %>% ggplot(aes(x=fh,y=mape)) + geom_line(size=1) +theme_minimal()+
  xlab("Forecast Horizon")+ylab("MAPE (%)")+ theme(text = element_text(size=16))
