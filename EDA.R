source("all_function.R")
init_run()

set.seed(72)

desc.compile<-data.frame()
norm.compile<-data.frame()
adf.compile<-data.frame()


for(location in c("Jakarta"))
{
  for(denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
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



# plot_normality(flow_data,3)
# adf<-adf.test(flow_data_xts[,1])
# acf<-acf(flow_data_xts[,1],lag.max = 100)
# pacf(flow_data_xts[,1],lag.max = 100)
# 
# ggg<-ur.df(flow_data_xts[,1])
# hhh<-adf.test(flow_data_xts[,1])
