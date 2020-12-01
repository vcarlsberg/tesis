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


arima_indiv<-Arima(split_data(flow_data_xts,20)$train,
                   order = c(0,1,0),seasonal = c(2,1,2))
fit_arima<-fitted(arima_indiv)
frc_arima<-forecast(arima_indiv,h=47)$mean
fit_frc_arima<-ts(c(fit_arima,frc_arima),
                  start=c(1999, 11), 
                  end=c(2019, 6),frequency = 12)

weight_kecil<-function(w1,w2) 
{
  Metrics::sse(raw_ffnnx_arima[,1],
               w1*na.omit(raw_ffnnx_arima[,2])+
                 w2*na.omit(raw_ffnnx_arima[,3]))
}

set.seed(72)
raw_ffnnx_arima<-ts.intersect(flow_data_xts,fit_ffnnx,fit_arima)
GA <- ga(type = "real-valued",pmutation=0.01,
         fitness = function(w) -weight_kecil(w[1],w[2]),
         lower =c(-2,-2), upper = c(2,2),
         maxiter=100,parallel=TRUE,seed=72,monitor = FALSE)

plot(GA)
GA@solution

raw_dlnn_arimax_train<-ts.intersect(flow_data_xts,
                                    GA@solution[1]*fit_ffnnx+GA@solution[2]*fit_arima)

TSrepr::mape(raw_dlnn_arimax_train[,1],raw_dlnn_arimax_train[,2])
TSrepr::rmse(raw_dlnn_arimax_train[,1],raw_dlnn_arimax_train[,2])

raw_dlnn_arimax_test<-ts.intersect(flow_data_xts,
                                   GA@solution[1]*frc_ffnnx+GA@solution[2]*frc_arima)
TSrepr::mape(raw_dlnn_arimax_test[,1],raw_dlnn_arimax_test[,2])
TSrepr::rmse(raw_dlnn_arimax_test[,1],raw_dlnn_arimax_test[,2])

#raw_dlnn_arimax<-ts.intersect(flow_data_xts,0.744*fit_frc_dlnn,0.281*fit_frc_arimax)
#raw_ffnnx_arima<-raw_ffnnx_arima %>% data.frame()
colnames(raw_ffnnx_arima)<-c("Outflow","FFNNX","ARIMA")

raw_ffnnx_arima_train<-ts.intersect(flow_data_xts,
                                    GA@solution[1]*fit_ffnnx+GA@solution[2]*fit_arima)
raw_ffnnx_arima_test<-ts.intersect(flow_data_xts,
                                   GA@solution[1]*frc_ffnnx+GA@solution[2]*frc_arima)
ffnnx_arima<-ts(c(raw_ffnnx_arima_train[,2],raw_ffnnx_arima_test[,2]),
                start=c(2003, 12), 
                end=c(2019, 6),frequency = 12)
raw_ffnnx_arima<-ts.intersect(flow_data_xts,ffnnx_arima)
colnames(raw_ffnnx_arima)<-c("Outflow","Hybrid")

raw_ffnnx_arima%>%data.frame()%>%
  mutate(Outflow=Outflow/1000,
         Predicted=Hybrid/1000,
         date=index(raw_ffnnx_arima)%>%yearmon())%>%
  select(c("Outflow","date","Predicted"))%>%
  gather(key="variable",value="value",-date)%>%
  ggplot( aes(x = date, y = value))+theme_minimal(base_size=16)+
  geom_line(aes(color = variable), size = 0.75)+
  geom_rect(fill="grey",xmin=2015.6666,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.01)+
  scale_x_yearmon(format="%b-%Y",breaks=pretty_breaks(20))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))+
  scale_y_continuous(name="Nilai Outflow \n(milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  annotate("text", x = 2018, y = 25000, label = "Out-of-Sample")+
  annotate("text", x = 2004, y = 25000, label = "In-Sample")+
  xlab("Bulan-Tahun")

#combined forecast
df.mape.oos<-data.frame(fh=numeric(),
                        mape=numeric())

for(h in c(1:24))
{
  frc_ffnnx<-forecast(ffnnx.model,h=h,
                      xreg = as.data.frame(flow_data_xts_xreg))$mean
  frc_arima<-forecast(arima_indiv,h=h)$mean
  
  intersect_data<-ts.intersect(GA@solution[1]*frc_ffnnx+GA@solution[2]*frc_arima,
                               split_data(flow_data_xts,20)$test[1:h])
  df.mape.oos<-rbind(df.mape.oos,data.frame(fh=h,
                                            mape=TSrepr::mape(intersect_data[,2],
                                                              intersect_data[,1]))
  )
}

df.mape.oos %>% mutate(predicate=case_when(
  mape<10 ~ "Akurasi Tinggi",
  mape>=10 & mape<=20 ~ "Baik",
  mape>20 & mape<=50 ~ "Cukup",
  mape>50  ~ "Tidak Akurat"
))%>% ggplot(aes(x=fh,y=mape,color=factor(predicate))) + geom_path(aes(group=2),size=1)+
  theme_minimal()+
  xlab("Forecast Horizon")+ylab("MAPE (%)")+ 
  theme(text = element_text(size=16))+ theme(legend.position = "top")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  labs(color='Predikat Akurasi Peramalan') 
