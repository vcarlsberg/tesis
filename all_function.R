my_function<- function(x){
  x*2
  
}

add_eidulfitr_regressor<-function(data){
  library(prophet)
  holidays<-prophet::generated_holidays
  
  holidays<-holidays %>% filter(country=="ID" & holiday=="Eid al-Fitr")
  holidays$bulan<-as.integer(format(as.Date(holidays$ds), "%m"))
  holidays$tahun<-as.integer(format(as.Date(holidays$ds), "%Y"))
  holidays<-holidays %>% select(-year)
  names(holidays)<-c("ds","holiday","country","Bulan","Tahun")
  
  add_1994<-data.frame("1994-03-14","Eid al-Fitr","ID",3,1994)
  names(add_1994)<-c("ds","holiday","country","Bulan","Tahun")
  holidays<-rbind(holidays, add_1994)
  
  data<-data %>% left_join(holidays,copy = TRUE) 
  data<-data %>% select(-c(holiday, country))
  data<-data %>% mutate(ds=ifelse(is.na(ds),yes = 0,no=1))
  #colnames(data)[colnames(data) == 'ds'] <- 'eid'
  #data<-as_tibble(data) %>% rename(ds=eid)
  #%>%  %>% 
}

read_data<-function(kota,pecahan){
  library(gsheet)
  library(tidyverse)
  file<-read.csv("Outflow_Refit.csv")
  #url<-"https://drive.google.com/file/d/1WES1QCc0OHqZt9kiFkwRp2qx2RGxOA97/view?usp=sharing"
  #dataset<-gsheet2tbl(url) %>% filter(Kota == kota) %>% select(Kota,Tahun,Bulan,pecahan)
  dataset<-file %>% filter(Kota == kota) %>% select(Kota,Tahun,Bulan,pecahan)
  
  index<-dataset[,4]<=0.0000
  dataset[,4][index]<-NA
  dataset<-dataset %>% na.trim()
  dataset[,4]<-na.approx(dataset[,4])
  dataset<-as.data.frame(dataset)
  dataset<-add_eidulfitr_regressor(dataset)
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
  
  library(SmartEDA)
  library(dlookr)
  
  library(tseries)
  library(urca)
  
  library(RSNNS)
  library(NMOF)
  
  library(ids)
  
  library(Metrics)
  
  library(GA)
  
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