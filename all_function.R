my_function<- function(x){
  x*2
  
}

add_eidulfitr_regressor<-function(data,flow){
  library(prophet)
  holidays<-prophet::generated_holidays
  
  holidays<-holidays %>% filter(country=="ID" & holiday=="Eid al-Fitr")
  
  add_1994_2<-data.frame("1994-03-15","Eid al-Fitr","ID",1994)
  add_1993_2<-data.frame("1993-03-26","Eid al-Fitr","ID",1993)
  names(add_1994_2)<-c("ds","holiday","country","year")
  names(add_1993_2)<-c("ds","holiday","country","year")
  holidays<-rbind(holidays,add_1994_2)
  holidays<-rbind(holidays,add_1993_2)
  
  holidays<-holidays[order(as.Date(holidays$ds)),]  
  row.names(holidays)<-NULL
  names(holidays)<-c("ds","holiday","country","year")
  
  
  if(flow=="Outflow")
  {
    #tanggal_int<-as.integer(format(as.Date(holidays$ds), "%d"))
    #tanggal_int<=10
    #subtract<-if(tanggal_int<=15) -1 
    holidays$bulan<-as.integer(format(as.Date(holidays$ds), "%m"))
    holidays$tanggal<-as.integer(format(as.Date(holidays$ds), "%d"))
    indices<-holidays$tanggal<=10
    holidays$ds2<-as.Date(holidays$ds)
    holidays$ds2[indices]<-holidays$ds2[indices]-30
    holidays$year2<-as.integer(format(as.Date(holidays$ds2), "%Y"))
    holidays$bulan2<-as.integer(format(as.Date(holidays$ds2), "%m"))
    holidays$tanggal2<-as.integer(format(as.Date(holidays$ds2), "%d"))

    holidays<-holidays %>% select(c(year2,bulan2,ds))
    names(holidays)<-c("Tahun","Bulan","ds")
  }else{
    #tanggal_int<-as.integer(format(as.Date(holidays$ds), "%d"))
    #tanggal_int<=10
    #subtract<-if(tanggal_int<=15) -1 
    holidays$bulan<-as.integer(format(as.Date(holidays$ds), "%m"))
    holidays$tanggal<-as.integer(format(as.Date(holidays$ds), "%d"))
    indices<-holidays$tanggal>=20
    holidays$ds2<-as.Date(holidays$ds)
    holidays$ds2[indices]<-holidays$ds2[indices]+30
    holidays$year2<-as.integer(format(as.Date(holidays$ds2), "%Y"))
    holidays$bulan2<-as.integer(format(as.Date(holidays$ds2), "%m"))
    holidays$tanggal2<-as.integer(format(as.Date(holidays$ds2), "%d"))
    
    holidays<-holidays %>% select(c(year2,bulan2,ds))
    names(holidays)<-c("Tahun","Bulan","ds")
  }

  

  
  data<-data %>% left_join(holidays,copy = TRUE) %>% mutate(ds=ifelse(is.na(ds),yes = 0,no=1))
  #data<-data %>% mutate(ds=ifelse(is.na(ds),yes = 0,no=1))
  #colnames(data)[colnames(data) == 'ds'] <- 'eid'
  #data<-as_tibble(data) %>% rename(ds=eid)
  #%>%  %>% 
}

read_data<-function(kota,pecahan,flow){
  library(gsheet)
  library(tidyverse)
  file<-read.csv("Data_Refit.csv")
  #url<-"https://drive.google.com/file/d/1WES1QCc0OHqZt9kiFkwRp2qx2RGxOA97/view?usp=sharing"
  #dataset<-gsheet2tbl(url) %>% filter(Kota == kota) %>% select(Kota,Tahun,Bulan,pecahan)
  dataset<-file %>% filter(Kota == kota, Flow==flow) %>% select(Kota,Tahun,Bulan,pecahan)
  
  index<-dataset[,4]<=0.0000
  dataset[,4][index]<-NA
  dataset<-dataset %>% na.trim()
  dataset[,4]<-na.approx(dataset[,4])
  dataset<-as.data.frame(dataset)
  dataset<-add_eidulfitr_regressor(dataset,flow)
  dataset<-dataset %>% select(-Kota)
  
  #dataset[,1]<-as.factor(dataset[,1])
  #dataset[,2]<-as.factor(dataset[,2])
  #dataset[,3]<-as.factor(dataset[,3])
  #dataset[,4]<-as.numeric(dataset[,4])
  #dataset[,5]<-as.integer(dataset[,5])
  
  #holidays<-eidulfitr_regressor(country = "ID",holiday = "Eid al-Fitr")
  #data_outflow<-left_join(data_outflow,holidays,copy=TRUE)
  #data_outflow<-data_outflow %>% select(tahun:ds)
  #data_outflow<-data_outflow %>% mutate(ds=ifelse(is.na(ds),yes = 0,no=1))
  
}

init_run<-function(){
  library(xts)
  library(forecast)
  library(nnfor)
  library(TSrepr)
  library(TSstudio)
  library(tidyverse)
  #library(Ecfun)
  
  library(SmartEDA)
  library(dlookr)
  
  library(tseries)
  library(urca)
  
  library(RSNNS)
  library(NMOF)
  
  library(ids)
  
  library(Metrics)
  
  library(GA)
  
  source("individual_model/ARIMA-Individual-func.R")
  source("individual_model/ARIMAX-Individual-func.R")
  
  source("individual_model/MLP-Individual-func.R")
  source("individual_model/MLPX-Individual-func.R")
  
  source("hybrid_model/ARIMA-MLP-Parallel-func.R")
  source("hybrid_model/ARIMA-MLP-Series-func.R")
  
  source("hybrid_model/ARIMAX-MLPX-Parallel-func.R")
  source("hybrid_model/ARIMAX-MLPX-Series-func.R")
  
  source("hybrid_model/MLP-ARIMA-Series-func.R")
  source("hybrid_model/MLPX-ARIMAX-Series-func.R")
  
  set.seed(72)
}

split_data<-function(data,precentage_test){
  library(TSstudio)
  length_data<-length(index(data))
  n_test<-round(length_data*(precentage_test/100))
  split<-ts_split(data,sample.out = n_test)
  return(split)
}

####################grid search################
#testFun <- function(x) {
#  mlp.model<-mlp(train_test_data$train,hd=c(x[1],x[2]),xreg = cbind(EIDULFITR),reps = 1,lags = x[3])
#  mlp.model$MSE
#}
#res <- gridSearch(fun=testFun, levels = list(1:10,1:10,1:24))
#res$minfun
#res$minlevels

###############################################