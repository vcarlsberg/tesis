library(lubridate)
library(scales)
init_run()
source("all_function.R")

flow_data<-read_data("Jakarta","K100000","Outflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,5:8],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

data_m<-flow_data %>% mutate(date = (as.yearmon(make_date(Tahun, Bulan))))%>%
  mutate(ds=ds*25819.783,K100000=K100000/1000)%>%
  select(K100000,ds,date)%>%`colnames<-`(c("Nilai Outflow","Variasi Kalender","date"))%>%
  gather(key="variable",value="value",-date) %>% ggplot( aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 0.6) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()+  scale_x_yearmon(format="%b-%Y",breaks=pretty_breaks(20))+
  scale_y_continuous(name="Nilai Outflow (milyar Rp)",
                     sec.axis = sec_axis(~./25819.783,name="Variasi Kalender"),
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE)
                     )+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))

data_m

arima_grid_search<-data.frame(Model=as.character(),
                              AIC=as.numeric(),
                              RMSE_In_Sample=as.numeric(),
                              MAPE_In_Sample=as.numeric(),
                              RMSE_Out_Sample=as.numeric(),
                              MAPE_Out_Sample=as.numeric())

split_data(flow_data_xts,20)$train
acf(diff(split_data(flow_data_xts,20)$train,differences = 1))
pacf(diff(split_data(flow_data_xts,20)$train,differences = 1))


for(p in c(0:2))
{
  for(q in c(0:2))
  {
    for(P in c(0:2))
    {
      for(Q in c(0:2))
      {
        arima_indiv<-Arima(split_data(flow_data_xts,20)$train,
                           order = c(p,1,q),seasonal = c(P,1,Q))
        
        fit_arima<-fitted(arima_indiv)
        frc_arima<-forecast(arima_indiv,h=length(split_data(flow_data_xts,20)$test))$mean
        
        fit_train_intersect_arima<-ts.intersect(split_data(flow_data_xts,20)$train,fit_arima)
        frc_test_intersect_arima<-ts.intersect(split_data(flow_data_xts,20)$test,
                                               frc_arima)
        
        arima_grid_search<-rbind(arima_grid_search,data.frame(Model=as.character(arima_indiv),
                                                              AIC=arima_indiv$aic,
                                                              RMSE_In_Sample=TSrepr::rmse(split_data(flow_data_xts,20)$train,fit_arima),
                                                              MAPE_In_Sample=TSrepr::mape(split_data(flow_data_xts,20)$train,fit_arima),
                                                              RMSE_Out_Sample=TSrepr::rmse(split_data(flow_data_xts,20)$test,frc_arima),
                                                              MAPE_Out_Sample=TSrepr::mape(split_data(flow_data_xts,20)$test,frc_arima)))
        
        
        
        #fit_frc_arima<-ts(c(fit_arima,frc_arima),start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6),frequency = 12)
        
        
      }
    }
  }
}

arima_indiv<-Arima(split_data(flow_data_xts,20)$train,
                   order = c(0,1,0),seasonal = c(2,1,2))
lmtest::coeftest(arima_indiv)

residual_arima_indiv<-split_data(flow_data_xts,20)$train-arima_indiv$fitted
Box.test(residual_arima_indiv,lag=30)
mean(residual_arima_indiv)

fit_arima<-fitted(arima_indiv)
frc_arima<-forecast(arima_indiv,h=length(split_data(flow_data_xts,20)$test))$mean

fit_train_intersect_arima<-ts.intersect(split_data(flow_data_xts,20)$train,fit_arima)
frc_test_intersect_arima<-ts.intersect(split_data(flow_data_xts,20)$test,
                                       frc_arima)

fit_frc_arima<-ts(c(fit_arima,frc_arima),start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6),frequency = 12)

ts.intersect(flow_data_xts,fit_frc_arima) %>% time() %>% as.yearmon()

compile_arima<-cbind(flow_data_xts %>% as.data.frame(),
               fit_frc_arima%>% as.data.frame(),
               flow_data_xts %>%time() %>% as.yearmon(),
               c(rep(0,189),rep(1,47))
               )
colnames(compile_arima)<-c("Outflow","Predicted","date","dum")


compile %>% mutate(Outflow=Outflow/1000,Predicted=Predicted/1000)%>%select(-dum)%>%
  gather(key="variable",value="value",-date) %>%
  ggplot( aes(x = date, y = value))+geom_line(aes(color = variable), size = 0.75)+
  geom_rect(fill="grey",xmin=2015.6666,xmax=Inf,ymin=-Inf,ymax=Inf,alpha=0.01)+
  theme_minimal()+
  scale_x_yearmon(format="%b-%Y",breaks=pretty_breaks(20))+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))+
  scale_y_continuous(name="Nilai Outflow (milyar Rp)",
                     labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  annotate("text", x = 2018, y = 25000, label = "Out-of-Sample")+
  annotate("text", x = 2004, y = 25000, label = "In-Sample")


df.mape.oos<-data.frame(fh=numeric(),
                        mape=numeric())
for(h in c(1:24))
{
  intersect_data<-ts.intersect(forecast(arima_indiv,h=h)$mean,
                               split_data(flow_data_xts,20)$test[1:h])
  df.mape.oos<-rbind(df.mape.oos,data.frame(fh=h,
                                            mape=TSrepr::mape(intersect_data[,1],intersect_data[,2]))
                     )
}

df.mape.oos %>% mutate(predicate=case_when(
  mape<10 ~ "Akurasi Tinggi",
  mape>=10 & mape<=20 ~ "Baik",
  mape>20 & mape<=50 ~ "Cukup",
  mape>50  ~ "Tidak Akurat"
))%>% ggplot(aes(x=fh,y=mape,color=factor(predicate))) + geom_line(size=1) +
  theme_minimal()+
  xlab("Forecast Horizon")+ylab("MAPE (%)")+ 
  theme(text = element_text(size=16))+ theme(legend.position = "top")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(color='Predikat Akurasi Peramalan') 
  

  
  
