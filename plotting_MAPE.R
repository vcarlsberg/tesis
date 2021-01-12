library(RMySQL)
mydb = dbConnect(RMySQL::MySQL(), 
                 user='admin', 
                 dbname='tesis', 
                 host='51.79.167.101',
                 port=3300,
                 password=as.character(read.csv(file.choose(),header = FALSE)[1,1]))

################################## plot grafik ################################
library(lubridate)
library(scales)
init_run()
source("all_function.R")

flow_data<-read_data("Semarang","K1000","Inflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,c(4:22,24)],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                         frequency=12)


adf.test(split_data(flow_data_xts,20)$train)
  ndiffs(split_data(flow_data_xts,20)$train)

(as.xts(flow_data_xts)/1000) %>% autoplot() + 
  labs(x="Tahun",y="Nilai (dalam milyar Rupiah)",title="Inflow Semarang K1000")+
  geom_vline(xintercept = seq(from=1994, to=2019, by=1),color="grey")+ 
  scale_x_continuous(breaks = seq(1994, 2020, 1))+theme(text = element_text(size=14))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))

ts_decompose(flow_data_xts/1000)


################################## plot MAPE ################################

library(tidyverse)
library(googlesheets4)

data_oos<-dbGetQuery(mydb, "SELECT * FROM result WHERE Location='Semarang';")

# data_oos<-read_sheet("1RbIf2cMINRJiRv3S_YC8wQviTUFSrxh86VE9MbMkkqU",sheet = "oos") %>% as.data.frame()


data_oos %>% filter(InOutSample=="Out Sample",Flow=="Inflow",
                    Denomination=="K1000")%>%
  select(Model,MAPE,fh)%>%
  gather(key="variable",value="value",MAPE)%>%select(-c(variable))%>%distinct()%>%
  ggplot( aes(x = fh, y = (value),color=Model))+geom_line(size = 0.75)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),name="MAPE",
                     labels=function(x) format(x, big.mark = ".",small.mark = ",", scientific = FALSE))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 30),name="Forecast Horizon")+
  theme(text = element_text(size=14))+ 
  theme(legend.position = "bottom")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=0, ymax=10, alpha=0.1, fill="green")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=10, ymax=20, alpha=0.1, fill="yellow1")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=20, ymax=50, alpha=0.1, fill="orange2")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=50, ymax=Inf, alpha=0.1, fill="red")


data_oos %>% filter(InOutSample=="Out Sample",Flow=="Inflow",
                    Denomination=="K1000")%>%
  select(Model,MAPE,fh)%>%
  ggplot( aes(x = fh, y = log10(MAPE),color=Model))+geom_line(size = 0.75)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 30),name="log10(MAPE)",
                     labels=function(x) format(x, big.mark = ".",decimal.mark = ",", scientific = FALSE))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 30),name="Forecast Horizon")+
  theme(text = element_text(size=14))+ 
  theme(legend.position = "bottom")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=log10(10), alpha=0.08, fill="green")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(10), ymax=log10(20), alpha=0.08, fill="yellow1")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(20), ymax=log10(50), alpha=0.08, fill="orange2")+
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(50), ymax=Inf, alpha=0.08, fill="red")+
  annotate("segment", x=-Inf, xend=Inf, y=log10(10), yend=log10(10),alpha=0.5)+
  annotate("segment", x=-Inf, xend=Inf, y=log10(20), yend=log10(20),alpha=0.5)+
  annotate("segment", x=-Inf, xend=Inf, y=log10(50), yend=log10(50),alpha=0.5)+
  annotate("text", x = 22, y = 0.9, label = "Akurasi Tinggi")+
  annotate("text", x = 22, y = 1.15, label = "Akurasi Baik")+
  annotate("text", x = 22, y = 1.5, label = "Akurasi Cukup")+
  annotate("text", x = 22, y = 2, label = "Akurasi Rendah")

data_oos %>% filter(InOutSample=="Out Sample",Flow=="Inflow",
                    Denomination=="K1000",MAPE<=10) %>% arrange(desc(fh),MAPE)

 data_oos %>% filter(InOutSample=="Out Sample",Flow=="Inflow",
                    Denomination=="K100000",
                    MAPE==data_oos %>% filter(InOutSample=="Out Sample",Flow=="Inflow",
                                              Denomination=="K100000") %>% select(MAPE) %>% min())


################################## plot all denom #########################################

Dataset <- read_delim("Data_Refit.csv", ";", 
                      escape_double = FALSE, trim_ws = TRUE)


#null_loc<-Dataset==""
#Dataset[null_loc]<-NA
Dataset$Kota<-as.factor(Dataset$Kota)
Dataset$Tahun<-as.factor(Dataset$Tahun)
Dataset$Bulan<-as.factor(Dataset$Bulan)

dataset_melt<-Dataset %>% reshape2::melt(id=c("Kota"))

Dataset %>% 
  filter(Kota=="Semarang" & Flow=="Inflow") %>%
  select("K100000","K50000","K20000","K10000","K5000","K2000","K1000") %>% reshape2::melt() %>% 
  na.omit() %>%
  ggplot(aes(x=variable, y=(value)/1000,fill=variable)) + 
  geom_boxplot() + xlab("Denominasi") +
  theme(legend.position = "none",text = element_text(size=14)) + 
  ggtitle("Distribusi Nilai Inflow tiap Denominasi Mata Uang di Wilayah Kediri") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),name="Inflow (dalam milyar Rupiah)",
                     labels=function(x) format(x, big.mark = ".",decimal.mark = ",", scientific = FALSE))

Dataset %>% 
  filter(Kota=="Kediri" & Flow=="Outflow") %>%
  select("K100000","K50000","K20000","K10000","K5000","K2000","K1000") %>% 
  mutate(K100000=K100000/1000,K50000=K50000/1000,K20000=K20000/1000,K10000=K10000/1000,
         K5000=K5000/1000,K2000=K2000/1000,K1000=K1000/1000) %>% describe() %>% View()

Dataset %>% 
  filter(Kota=="Semarang" & Flow=="Inflow") %>%
            select("K100000","K50000","K20000","K10000","K5000","K2000","K1000") %>% 
            mutate(K100000=K100000/1000,K50000=K50000/1000,K20000=K20000/1000,K10000=K10000/1000,
                   K5000=K5000/1000,K2000=K2000/1000,K1000=K1000/1000) %>% describe() %>%
  write.csv("Descriptive_Stats_Inflow_Semurung.csv")
  
Dataset %>% 
  filter(Kota=="Tasikmalaya" & Flow=="Outflow") %>%
  select("K100000","K50000","K20000","K10000","K5000","K2000","K1000") %>% 
  mutate(K100000=K100000/1000,K50000=K50000/1000,K20000=K20000/1000,K10000=K10000/1000,
         K5000=K5000/1000,K2000=K2000/1000,K1000=K1000/1000) %>% summary()

Dataset %>% 
  filter(Kota=="Cirebon" & Flow=="Outflow") %>%
  select("K100000","K50000","K20000","K10000","K5000","K2000","K1000") %>% 
  mutate(K100000=K100000/1000,K50000=K50000/1000,K20000=K20000/1000,K10000=K10000/1000,
         K5000=K5000/1000,K2000=K2000/1000,K1000=K1000/1000) %>%
