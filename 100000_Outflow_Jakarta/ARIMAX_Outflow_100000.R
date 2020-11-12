library(lubridate)
library(scales)
init_run()
source("all_function.R")

flow_data<-read_data("Jakarta","K100000","Outflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)


arimax_indiv<-Arima(split_data(flow_data_xts,20)$train,
                   xreg = split_data(flow_data_xts_xreg,20)$train,
                   order=c(0,1,2),seasonal = c(1,0,0))
lmtest::coeftest(arimax_indiv)
summary(arimax_indiv)
fit_arimax<-fitted(arimax_indiv)
frc_arimax<-forecast(arimax_indiv,xreg = split_data(flow_data_xts_xreg,20)$test)$mean
fit_frc_arimax<-ts(c(fit_arimax,frc_arimax),start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6),frequency = 12)

frc.arimax_indiv<-forecast(arimax_indiv,
                           xreg = split_data(flow_data_xts_xreg,20)$test)
TSrepr::mape(split_data(flow_data_xts,20)$test,frc.arimax_indiv$mean)
TSrepr::rmse(split_data(flow_data_xts,20)$test,frc.arimax_indiv$mean)

Box.test(residuals(arimax_indiv),lag = 30)

compile_arimax<-cbind(flow_data_xts %>% as.data.frame(),
               fit_frc_arimax%>% as.data.frame(),
               flow_data_xts %>%time() %>% as.yearmon(),
               c(rep(0,189),rep(1,47))
)
colnames(compile_arimax)<-c("Outflow","Predicted","date","dum")


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
  annotate("text", x = 2004, y = 25000, label = "In-Sample")+
  xlab("Bulan-Tahun")+
  theme(text = element_text(size=14))

  
df.mape.oos<-data.frame(fh=numeric(),
                        mape=numeric())

for(h in c(1:24))
{
  intersect_data<-ts.intersect(forecast(arimax_indiv,
                                        xreg = split_data(flow_data_xts_xreg,20)$test[1:h])$mean,
                               split_data(flow_data_xts,20)$test[1:h])
  df.mape.oos<-rbind(df.mape.oos,data.frame(fh=h,
                                            mape=TSrepr::mape(intersect_data[,1],
                                                              intersect_data[,2]))
  )
}

df.mape.oos %>% ggplot(aes(x=fh,y=mape)) + geom_line(size=1) +theme_minimal()+
  xlab("Forecast Horizon")+ylab("MAPE (%)")+ theme(text = element_text(size=16))
