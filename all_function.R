my_function<- function(x){
  x*2
  
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
}

init_run<-function(){
  library(xts)
  library(forecast)
  library(nnfor)
  library(TSrepr)
  library(TSstudio)
  
  set.seed(72)
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