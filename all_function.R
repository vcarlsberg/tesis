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
