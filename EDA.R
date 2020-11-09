source("all_function.R")
init_run()

set.seed(72)

desc.compile<-data.frame()
norm.compile<-data.frame()
adf.compile<-data.frame()


for(location in c("Jakarta"))
{
  for(denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000","L1000","L500","L200","L100","L50"))
  {
    flow_data<-read_data(location,denomination)
    flow_data_xts <- ts(flow_data[,3:4],
                        start=c(flow_data[1,1], flow_data[1,2]), 
                        end=c(2019, 6),frequency = 12)
    desc<-describe(flow_data,3)
    desc<-cbind(desc,data.frame(loc=location))
    desc.compile<-rbind(desc.compile,desc)
    
    norm<-normality(flow_data,3)
    norm<-cbind(norm,data.frame(loc=location))
    norm.compile<-rbind(norm.compile,norm)
    
    adf<-adf.test(flow_data_xts[,1])
    df.adf<-data.frame(
            statistic=adf[["statistic"]][["Dickey-Fuller"]],
            lag=adf[["parameter"]][["Lag order"]],
            pval=adf[["p.value"]],
            variable=denomination,
            loc=location
            )
    adf.compile<-rbind(adf.compile,df.adf)
    
  }
  
  write.csv(desc.compile,file = paste0("DescriptiveStatistics_",location,".csv"))
  write.csv(norm.compile,file = paste0("NormalityTest_",location,".csv"))
  write.csv(adf.compile,file = paste0("ADFTest_",location,".csv"))
}

library(gsheet)
library(tidyverse)
library(reshape2)
library(zoo)
library(dlookr)
library(scales)
library(gridExtra)

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

flow_data<-read_data("Jakarta","K50000","Inflow")

#normality(subset$)

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)
autoplot(flow_data_xts)

flow_data_xts_xreg <- ts(flow_data[,4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)
autoplot(flow_data_xts_xreg)

grid.arrange(autoplot(flow_data_xts),autoplot(flow_data_xts_xreg), ncol=1)

auto.arima(flow_data_xts,xreg =flow_data_xts_xreg )


#as.ts(rnorm(100))
plot(decompose(flow_data_xts))
adf.test(flow_data_xts)
kpss.test(flow_data_xts)
pp.test(flow_data_xts)
ndiffs(flow_data_xts)

Acf(flow_data_xts,lag.max = 60,main="Plot ACF 100000 - Jakarta (Outflow)")
Pacf(flow_data_xts,lag.max = 60,main="Plot PACF 100000 - Jakarta (Outflow)")

auto.arima(arima.sim(model=list(order=c(0,1,0)),n=1000,mean=1)) %>% as.character()

auto.arima(flow_data_xts)

#mlpo<-mlp(flow_data_xts,hd = c(3,2),lags = 1:60)

(as.xts(flow_data_xts)/1000) %>% autoplot() + labs(x="Tahun",y="Nilai (dalam milyar)",title="Outflow Jakarta K100000")+
  geom_vline(xintercept = seq(from=2000, to=2019, by=1),color="grey")+ 
  scale_x_continuous(breaks = seq(2000, 2020, 1))+theme(text = element_text(size=14))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))

p0<-(as.xts(flow_data_xts)['2011']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai",title="Outflow Jakarta K100000 (th 2011)")+
  geom_vline(xintercept = seq(from=2011, to=(2012-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels = comma)
p1<-(as.xts(flow_data_xts)['2012']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai",title="Outflow Jakarta K100000 (th 2012)")+
  geom_vline(xintercept = seq(from=2012, to=(2013-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels = comma)
p2<-(as.xts(flow_data_xts)['2013']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai",title="Outflow Jakarta K100000 (th 2013)")+
  geom_vline(xintercept = seq(from=2013, to=(2014-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels = comma)
p3<-(as.xts(flow_data_xts)['2014']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai",title="Outflow Jakarta K100000 (th 2014)")+
  geom_vline(xintercept = seq(from=2014, to=(2015-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels = comma)
p4<-(as.xts(flow_data_xts)['2015']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai",title="Outflow Jakarta K100000 (th 2015)")+
  geom_vline(xintercept = seq(from=2015, to=(2016-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels = comma)

grid.arrange(p0,p1,p2,p3,p4, ncol=1)

(as.xts(flow_data_xts)['2004']/1000) %>% autoplot() + labs(x="Tahun",y="Nilai",title="Outflow Jakarta K100000 (th 2015)")+
  geom_vline(xintercept = seq(from=2004, to=(2005-1/12), by=1/12),color="grey")+
  scale_y_continuous(labels = comma)


auto.arima(flow_data_xts)


ggplot(aes(y), data = fortify(flow_data_xts, melt = TRUE)) +
  geom_histogram(bins=20) + labs(y="n",x="Arus Outflow",title="Distribusi Nilai Arus Outflow \nDenominasi Rp.50.000 - Wilayah DKI Jakarta") 

ts_plot(flow_data_xts,title = "Outflow Jakarta K100000")
abline(v=15, col="blue")

ts_decompose(flow_data_xts)

x = ts(cumsum(rnorm(1000)),start = c(1990,12),frequency = 12)
ts_decompose(x)

p <- ggplot(flow_data_xts)
            
            , aes(x=day, y=value)) +
  geom_line() + 
  xlab("")
p

as.numeric(subset[1,1])
write.csv(describe(Dataset))
normality(Dataset)

library(ggplot2)

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Most basic violin chart
p <- ggplot(data, aes(x=name, y=value, fill=name)) + # fill=name allow to automatically dedicate a color for each group
  geom_violin()
p

ggplot(mtcars, aes(x = mpg)) +
  geom_dotplot(binwidth = 1.5, stackdir = "center")
