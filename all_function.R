my_function<- function(x){
  x*2
  
}

add_eidulfitr_regressor<-function(data){
  
  holidays<-prophet::generated_holidays
  
  holidays<-holidays %>% filter(country=="ID" & holiday=="Eid al-Fitr")
  holidays$bulan<-as.integer(format(as.Date(holidays$ds), "%m"))
  holidays$tahun<-as.integer(format(as.Date(holidays$ds), "%Y"))
  
  add_1994<-data.frame("1994-03-14","Eid al-Fitr","ID",1994,3,1994)
  names(add_1994)<-c("ds","holiday","country","year","bulan","tahun")
  holidays<-rbind(holidays, add_1994)
  
  data<-data %>% left_join(holidays,copy = TRUE) 
  data<-data %>% select(tahun:ds)
  data<-data %>% mutate(ds=ifelse(is.na(ds),yes = 0,no=1))
  #colnames(data)[colnames(data) == 'ds'] <- 'eid'
  #data<-as_tibble(data) %>% rename(ds=eid)
  #%>%  %>% 
}

read_data<-function(kota,pecahan){
  library(gsheet)
  library(tidyverse)
  url<-"https://docs.google.com/spreadsheets/d/1pYpYd04zw6iUz32mGkGNz_1_-jorwM-QWGxXSKiOzpo/edit?usp=sharing"
  a <- gsheet2text(url, format='csv')
  b <- read.csv(text=a, stringsAsFactors=FALSE)
  c<-b %>% filter(Kota == kota)
  
  Dataset_Surabaya <- c
  data_outflow<-data.frame(tahun=Dataset_Surabaya[["Tahun"]],
                           bulan=Dataset_Surabaya[["Bulan"]],
                           data1=Dataset_Surabaya[pecahan]
  )
  
  index<-data_outflow[pecahan]==0
  data_outflow[pecahan][index]<-NA
  data_outflow<-na.omit(na.approx(data_outflow))
  data_outflow<-as.data.frame(data_outflow)
  data_outflow<-add_eidulfitr_regressor(data_outflow)
  
  
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
  
  set.seed(72)
}

split_data<-function(data,precentage_test){
  library(TSstudio)
  length_data<-dim(flow_data_xts)[1]
  n_test<-round(length_data*(precentage_test/100))
  n_train<-length_data-n_test
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