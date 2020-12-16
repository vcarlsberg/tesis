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

flow_data<-read_data("Jakarta","K50000","Outflow")

flow_data_xts <- ts(flow_data[,3],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                    frequency=12)

flow_data_xts_xreg <- ts(flow_data[,4],start=c(flow_data[1,1], flow_data[1,2]), end=c(2019, 6), 
                         frequency=12)

(as.xts(flow_data_xts)/1000) %>% autoplot() + 
  labs(x="Tahun",y="Nilai (dalam milyar)",title="Outflow Jakarta K50000")+
  geom_vline(xintercept = seq(from=1994, to=2019, by=1),color="grey")+ 
  scale_x_continuous(breaks = seq(1994, 2020, 1))+theme(text = element_text(size=14))+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))

ts_decompose(flow_data_xts)
