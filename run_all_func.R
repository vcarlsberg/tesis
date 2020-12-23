source("all_function.R")
library(googlesheets4)
init_run()

compiled_result<-data.frame()
nn_gridsearch_result<-data.frame()

lag_info <- read_csv("lag_info.csv") %>% as.data.frame()
lag_info$Lags<-strsplit(lag_info$Lags," ")

sheet_id<-"1Lh34zALwYcbPTRN2KgDa3FOspgiSGUSJyBAcbofjgjA"


for (flow in c("Inflow"))
{
  for (location in c("Cirebon"))
  {
    for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
    {
      for (preprocessing in c(1))
      {
        tryCatch({
          print(paste(flow,location,denomination,preprocessing,Sys.time(),"ARIMA"))

          result<-ARIMA_Individual(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,flow = flow)
          compiled_result<-rbind(compiled_result,result)
        },error=function(e){
          print(e)
        })

        tryCatch({
          print(paste(flow,location,denomination,preprocessing,Sys.time(),"ARIMAX"))

          result<-ARIMAX_Individual(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,flow = flow)
          compiled_result<-rbind(compiled_result,result)
        },error=function(e){
          print(e)
        })

      }
    }
  }
}


for (flow in c("Inflow","Outflow"))
{
  for (location in c("Bandung"))
  {
    for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
    {
      for (preprocessing in c(1))
      {
        for (MLP_layer in c(1,2))
        {
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"MLP_Individual"))

            #findlag
            lagrow<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMA-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-MLP_Individual(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,
                                   MLP_layer=MLP_layer,
                                   flow=flow,
                                   lag=lagrow)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            
            sheet_append(sheet_id,
                         result$modelResult,
                         sheet="oos")
            sheet_append(sheet_id,
                         result$gridsearchNN,
                         sheet="nn")
            
            
          },error=function(e){
            print(e)
          })

          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"MLXP_Individual"))
            
            #findlag
            lagrow<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMAX-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-MLPX_Individual(preprocessing = preprocessing,
                                    location=location,
                                    denomination=denomination,
                                    MLP_layer=MLP_layer,
                                    flow=flow,
                                    lag=lagrow)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            sheet_append(sheet_id,
                         result$modelResult,
                         sheet="oos")
            sheet_append(sheet_id,
                         result$gridsearchNN,
                         sheet="nn")
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMA_MLP_Parallel"))
            
            #findlag
            lagrow<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMA-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-ARIMA_MLP_Parallel(preprocessing = preprocessing,
                                       location=location,
                                       denomination=denomination,
                                       MLP_layer=MLP_layer,
                                       flow=flow,
                                       lag=lagrow)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            sheet_append(sheet_id,
                         result$modelResult,
                         sheet="oos")
            sheet_append(sheet_id,
                         result$gridsearchNN,
                         sheet="nn")
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMAX_MLPX_Parallel"))
            
            #findlag
            lagrow<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMAX-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-ARIMAX_MLPX_Parallel(preprocessing = preprocessing,
                                         location=location,
                                         denomination=denomination,
                                         MLP_layer=MLP_layer,
                                         flow=flow,
                                         lag=lagrow)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            sheet_append(sheet_id,
                         result$modelResult,
                         sheet="oos")
            sheet_append(sheet_id,
                         result$gridsearchNN,
                         sheet="nn")
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMA_MLP_Seri"))
            
            #findlag
            lagrow<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMA-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-ARIMA_MLP_Series(preprocessing = preprocessing,
                                     location=location,
                                     denomination=denomination,
                                     MLP_layer=MLP_layer,
                                     flow=flow,
                                     lag=lagrow)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            sheet_append(sheet_id,
                         result$modelResult,
                         sheet="oos")
            sheet_append(sheet_id,
                         result$gridsearchNN,
                         sheet="nn")
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMAX_MLPX_Series"))
            
            #findlag
            lagrow<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMAX-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-ARIMAX_MLPX_Series(preprocessing = preprocessing,
                                       location=location,
                                       denomination=denomination,
                                       MLP_layer=MLP_layer,
                                       flow=flow,
                                       lag=lagrow)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            sheet_append(sheet_id,
                         result$modelResult,
                         sheet="oos")
            sheet_append(sheet_id,
                         result$gridsearchNN,
                         sheet="nn")
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"MLP_ARIMA_Seri"))
            
            #findlag
            lagrow<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMA-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-MLP_ARIMA_Series(preprocessing = preprocessing,
                                     location=location,
                                     denomination=denomination,
                                     MLP_layer=MLP_layer,
                                     flow=flow,
                                     lag=lagrow)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            sheet_append(sheet_id,
                         result$modelResult,
                         sheet="oos")
            sheet_append(sheet_id,
                         result$gridsearchNN,
                         sheet="nn")
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"MLPX_ARIMAX_Series"))
            
            #findlag
            lagrow<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMAX-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-MLPX_ARIMAX_Series(preprocessing = preprocessing,
                                       location=location,
                                       denomination=denomination,
                                       MLP_layer=MLP_layer,
                                       flow=flow,
                                       lag=lagrow)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            sheet_append(sheet_id,
                         result$modelResult,
                         sheet="oos")
            sheet_append(sheet_id,
                         result$gridsearchNN,
                         sheet="nn")
            
          },error=function(e){
            print(e)
          })
          
          
        }
      }
    }
  }
}
