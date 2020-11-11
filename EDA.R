
library(gsheet)
library(tidyverse)
library(reshape2)
library(zoo)
library(dlookr)
library(scales)
library(gridExtra)


source("all_function.R")
init_run()

set.seed(72)


Dataset <- read_csv("Data_Refit.csv")


#null_loc<-Dataset==""
#Dataset[null_loc]<-NA
Dataset$Kota<-as.factor(Dataset$Kota)
Dataset$Tahun<-as.factor(Dataset$Tahun)
Dataset$Bulan<-as.factor(Dataset$Bulan)

dataset_melt<-melt(Dataset)

dataset_melt %>% 
  filter(Kota=="Jakarta" & variable==c("K100000","K50000","K20000","K10000","K5000","K2000","K1000")) %>% 
  ggplot(aes(x=variable, y=(value)/1000000,fill=variable)) + 
  geom_boxplot() + xlab("Denominasi") + ylab("Outflow (dalam trilyun Rupiah)") + 
  theme(legend.position = "none") + 
  ggtitle("Distribusi Nilai Outflow tiap Denominasi Mata Uang di Kota Jakarta") +
  scale_y_continuous(labels = comma)

desc<-dataset_melt %>% group_by(variable,Kota) %>% describe()
desc

desc<-dataset_melt %>% 
  filter(Kota=="Jakarta" & variable==c("K100000")) %>% describe()
desc

subset<-dataset_melt %>% 
  filter(Kota=="Jakarta") %>% 
  filter(variable=="K50000") %>% na.omit() %>% normality()

row.names(subset) <- NULL
subset$Tahun<-as.numeric(subset$Tahun)
subset$Bulan<-as.numeric(subset$Bulan)

View(subset %>% describe())

flow_data<-read_data("Jakarta","K100000","Outflow")

flow_data$date<-as.yearmon(paste(flow_data$Tahun,flow_data$Bulan),"%Y %m")

df<-flow_data %>% select("K100000","ds","date") 
colnames(df)<-c("Outflow K100000","Variasi Kalender","date")
df[,1]<-norm_min_max(df[,1])

df<-df%>% 
  gather(key="variable",value="value",-date)


ggplot(df, aes(x = as.Date(date), y = value)) + 
  geom_line(aes(color = variable), size = 0.75) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()+scale_x_date(date_labels = "%b-%Y",date_breaks = "1 year")+
  theme(axis.text.x = element_text(angle = 45))+
  ylab("Nilai")+xlab("Periode")+theme(legend.position = "bottom")+
  theme(text = element_text(size=12))

+  
  theme(axis.text.x = element_text(margin = margin(r = 0)))

  
  scale_x_discrete(breaks = scales::pretty_breaks(n = 100))+
  theme(axis.text.x = element_text(angle = 90))+
  geom_vline(xintercept = seq(1999,2019,by=1),color="grey")

fff<-cbind(index(flow_data_xts_xreg),as.numeric(flow_data_xts_xreg)) %>% as.data.frame()
colnames(fff)<-c("index","value")
fff<-fff %>% filter(value==1) %>% select(index) %>% as.vector()
fff[,1]

ccc<-(seq(1,2,by = 0.2))




flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)
autoplot(flow_data_xts)

flow_data_xts_xreg <- ts(flow_data[,4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)
autoplot(flow_data_xts_xreg)

auto.arima(flow_data_xts,xreg =flow_data_xts_xreg )


#as.ts(rnorm(100))
plot(decompose(flow_data_xts))
adf.test(flow_data_xts)
kpss.test(flow_data_xts)
pp.test(flow_data_xts)
ndiffs(flow_data_xts)
diff(flow_data_xts,differences = 1)

Acf(flow_data_xts,main="Plot ACF 100000 - Jakarta (Outflow)",lag.max = 60)
Pacf(flow_data_xts,main="Plot PACF 100000 - Jakarta (Outflow)",lag.max = 60)

auto.arima(arima.sim(model=list(order=c(0,1,0)),n=1000,mean=1)) %>% as.character()

split_data(flow_data_xts,20)$train

auto.arima(split_data(flow_data_xts,20)$train,trace = TRUE,seasonal = TRUE)

df_arima<-data.frame()

for(p in c(0:2))
{
  for(q in c(0:2))
  {
    for(P in c(0:2))
    {
      for(Q in c(0:2))
      {
        tryCatch(
          {
            arima_indiv<-Arima(split_data(flow_data_xts,20)$train,
                               order = c(p,1,q),seasonal = c(P,0,Q),
                               xreg = split_data(flow_data_xts_xreg,20)$train)
            summary_arima<-summary(arima_indiv)
            
            frc.arima_indiv<-forecast(arima_indiv,
                                      xreg = split_data(flow_data_xts_xreg,20)$test)%>%summary()
            mape_out<-TSrepr::mape(split_data(flow_data_xts,20)$test,frc.arima_indiv$`Point Forecast`)
            rmse_out<-TSrepr::rmse(split_data(flow_data_xts,20)$test,frc.arima_indiv$`Point Forecast`)
            
            df_arima<-rbind(df_arima,data.frame(arima_model=as.character(arima_indiv),
                                                AIC=AIC(arima_indiv),
                                                RMSE=summary_arima[,2],
                                                MAPE=summary_arima[,5],
                                                RMSE_out=rmse_out,
                                                MAPE_out=mape_out))
            
          },error=function(e)
          {
            print(e)
          }
        )

      }
    }
  }
}

arima_indiv<-Arima(split_data(flow_data_xts,20)$train,order = c(0,1,1),seasonal = c(0,0,1))
arima_indiv<-auto.arima(split_data(flow_data_xts,20)$train)
plot(flow_data_xts,col="red")
lines(fit_frc_arima,col="blue")
fit_arima<-fitted(arima_indiv)
frc_arima<-forecast(arima_indiv,h=47)
fit_frc_arima<-ts(c(fit_arima,frc_arima$mean),start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6),frequency = 12)
autoplot(flow_data_xts)
ts.plot(ts.intersect(flow_data_xts,fit_frc_arima),gpars= list(col=rainbow(3),main="Ngono"))

#ts(c(fitted(arima_indiv),forecast(arima_indiv,h=47)))
lines(forecast(arima_indiv,h=47),col="black")

ts.plot(arima_indiv)
plot(arima_indiv)
checkresiduals(arima_indiv)
Box.test(residuals(arima_indiv),lag = 30)
library(lmtest)
coeftest(arima_indiv)

frc.arima_indiv<-forecast(arima_indiv,h=47)%>%summary()
TSrepr::mape(split_data(flow_data_xts,20)$test,frc.arima_indiv$`Point Forecast`)
TSrepr::rmse(split_data(flow_data_xts,20)$test,frc.arima_indiv$`Point Forecast`)

ts.plot(flow_data_xts,flow_data_xts_xreg)

autoplot(((flow_data_xts)/1000000),color="red")
autoplot(((flow_data_xts_xreg)),color="blue")

autoplot(ts(a_xts,start=c(2019,4,16),frequency = 365.25))
+autolayer(ts(b_xts,start=c(2019,4,16),frequency = 365.25))

(as.xts(flow_data_xts)/1000) %>% autoplot() + 
  labs(x="Tahun",y="Nilai (dalam milyar)",title="Outflow Jakarta K100000")+
  geom_vline(xintercept = seq(from=1999, to=2019, by=1),color="grey")+ 
  scale_x_continuous(breaks = seq(1999, 2020, 1))+theme(text = element_text(size=14))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))

pa<-(as.xts(flow_data_xts)['2010']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai (dalam \n milyar \n Rupiah)",title="Outflow Jakarta K100000 (th 2010)")+
  geom_vline(xintercept = seq(from=2010, to=(2011-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))
p0<-(as.xts(flow_data_xts)['2011']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai (dalam \n milyar \n Rupiah)",title="Outflow Jakarta K100000 (th 2011)")+
  geom_vline(xintercept = seq(from=2011, to=(2012-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))
p1<-(as.xts(flow_data_xts)['2012']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai (dalam \n milyar \n Rupiah)",title="Outflow Jakarta K100000 (th 2012)")+
  geom_vline(xintercept = seq(from=2012, to=(2013-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))
p2<-(as.xts(flow_data_xts)['2013']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai (dalam \n milyar \n Rupiah)",title="Outflow Jakarta K100000 (th 2013)")+
  geom_vline(xintercept = seq(from=2013, to=(2014-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))
p3<-(as.xts(flow_data_xts)['2014']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai (dalam \n milyar \n Rupiah)",title="Outflow Jakarta K100000 (th 2014)")+
  geom_vline(xintercept = seq(from=2014, to=(2015-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))
p4<-(as.xts(flow_data_xts)['2015']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai (dalam \n milyar \n Rupiah)",title="Outflow Jakarta K100000 (th 2015)")+
  geom_vline(xintercept = seq(from=2015, to=(2016-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))

grid.arrange(pa,p0,p1,p2,p3,p4,ncol=2)

(as.xts(flow_data_xts)['2004']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai",title="Outflow Jakarta K100000 (th 2015)")+
  geom_vline(xintercept = seq(from=2004, to=(2005-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels = comma)


auto.arima(flow_data_xts)


ggplot(aes(y), data = fortify(flow_data_xts, melt = TRUE)) +
  geom_histogram(bins=20) + labs(y="n",x="Arus Outflow",title="Distribusi Nilai Arus Outflow \nDenominasi Rp.50.000 - Wilayah DKI Jakarta") 

ts_plot(flow_data_xts,title = "Outflow Jakarta K100000")
abline(v=15, col="blue")

ts_decompose(flow_data_xts)

