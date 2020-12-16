library(lubridate)
library(scales)
init_run()
source("all_function.R")

flow_data<-read_data("Jakarta","K50000","Outflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,c(4:22,24)],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                         frequency=12)



arimax_indiv<-auto.arima(split_data(flow_data_xts,20)$train,
                         xreg = split_data(flow_data_xts_xreg,20)$train,d = 0,D = 0)
lmtest::coeftest(arimax_indiv)
summary(arimax_indiv)
fit_arimax<-fitted(arimax_indiv)
frc_arimax<-forecast(arimax_indiv,
                     xreg = split_data(flow_data_xts_xreg,20)$test)$mean
fit_frc_arimax<-ts(c(fit_arimax,frc_arimax),
                   start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6),frequency = 12)

frc.arimax_indiv<-forecast(arimax_indiv,
                           xreg = split_data(flow_data_xts_xreg,20)$test)

TSrepr::mape(split_data(flow_data_xts,20)$train,fit_arimax)
TSrepr::rmse(split_data(flow_data_xts,20)$train,fit_arimax)
TSrepr::mape(split_data(flow_data_xts,20)$test,frc.arimax_indiv$mean)
TSrepr::rmse(split_data(flow_data_xts,20)$test,frc.arimax_indiv$mean)

residual_arimax_indiv<-split_data(flow_data_xts,20)$train-arimax_indiv$fitted
Box.test(residual_arimax_indiv,lag = 30)

compile_arimax<-cbind(flow_data_xts %>% as.data.frame(),
                      fit_frc_arimax%>% as.data.frame(),
                      flow_data_xts %>%time() %>% as.yearmon()
)
colnames(compile_arimax)<-c("Outflow","Predicted","date")


compile_arimax %>% mutate(Outflow=Outflow/1000,Predicted=Predicted/1000)%>%
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
  annotate("text", x = 2004, y = 25000, label = "In-Sample")+
  xlab("Bulan-Tahun")+
  theme(text = element_text(size=14))


df.mape.oos_arimax<-data.frame(fh=numeric(),
                        mape=numeric())

for(h in c(1:24))
{
  intersect_data<-ts.intersect(
    forecast(arimax_indiv,
             xreg = split_data(flow_data_xts_xreg,20)$test)$mean,
    split_data(flow_data_xts,20)$test
  )
  #print(intersect_data)
  df.mape.oos_arimax<-rbind(df.mape.oos_arimax,data.frame(fh=h,
                                            mape=TSrepr::mape(intersect_data[1:h,2],
                                                              intersect_data[1:h,1]))
  )
}

df.mape.oos_arimax %>% mutate(predicate=case_when(
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
