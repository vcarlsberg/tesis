library(RMySQL)
mydb = dbConnect(RMySQL::MySQL(), 
                 user='admin', 
                 password='sZpckBNuXW4H', 
                 dbname='tesis', 
                 host='51.79.167.101',
                 port=3300)

dbListTables(mydb)

source("all_function.R")
init_run()

compiled_result<-data.frame()
nn_gridsearch_result<-data.frame()

lag_info <- read_csv("lag_info.csv") %>% as.data.frame()
lag_info$Lags<-strsplit(lag_info$Lags," ")


for (flow in c("Inflow","Outflow"))
{
  for (location in c("Semarang"))
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
          
          result$DateExecuted<-result$DateExecuted %>% as.character()
          append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result,row.names = FALSE )
          dbExecute( conn = mydb , statement = append_cmd )
          
        },error=function(e){
          print(e)
        })

        tryCatch({
          print(paste(flow,location,denomination,preprocessing,Sys.time(),"ARIMAX"))

          result<-ARIMAX_Individual(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,flow = flow)
          compiled_result<-rbind(compiled_result,result)
          
          result$DateExecuted<-result$DateExecuted %>% as.character()
          append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result,row.names = FALSE )
          dbExecute( conn = mydb , statement = append_cmd )
          
        },error=function(e){
          print(e)
        })

      }
    }
  }
}


for (flow in c("Inflow","Outflow"))
{
  for (location in c("Semarang"))
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
            lag<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMA-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-MLP_Individual(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,
                                   MLP_layer=MLP_layer,
                                   flow=flow,
                                   lag=lag)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            
            result$modelResult$DateExecuted<-result$modelResult$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result$modelResult,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            result$gridsearchNN$DateExecuted<-result$gridsearchNN$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "nn",values = result$gridsearchNN,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"MLPX_Individual"))
            
            #findlag
            lag<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMAX-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-MLPX_Individual(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,
                                   MLP_layer=MLP_layer,
                                   flow=flow,
                                   lag=lag)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            
            result$modelResult$DateExecuted<-result$modelResult$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result$modelResult,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            result$gridsearchNN$DateExecuted<-result$gridsearchNN$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "nn",values = result$gridsearchNN,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMA_MLP_Parallel"))
            
            #findlag
            lag<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMA-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-ARIMA_MLP_Parallel(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,
                                   MLP_layer=MLP_layer,
                                   flow=flow,
                                   lag=lag)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            
            result$modelResult$DateExecuted<-result$modelResult$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result$modelResult,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            result$gridsearchNN$DateExecuted<-result$gridsearchNN$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "nn",values = result$gridsearchNN,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMAX_MLPX_Parallel"))
            
            #findlag
            lag<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMAX-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-ARIMAX_MLPX_Parallel(preprocessing = preprocessing,
                                       location=location,
                                       denomination=denomination,
                                       MLP_layer=MLP_layer,
                                       flow=flow,
                                       lag=lag)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            
            result$modelResult$DateExecuted<-result$modelResult$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result$modelResult,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            result$gridsearchNN$DateExecuted<-result$gridsearchNN$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "nn",values = result$gridsearchNN,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMA_MLP_Series"))
            
            #findlag
            lag<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMA-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-ARIMA_MLP_Series(preprocessing = preprocessing,
                                         location=location,
                                         denomination=denomination,
                                         MLP_layer=MLP_layer,
                                         flow=flow,
                                         lag=lag)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            
            result$modelResult$DateExecuted<-result$modelResult$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result$modelResult,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            result$gridsearchNN$DateExecuted<-result$gridsearchNN$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "nn",values = result$gridsearchNN,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMAX_MLPX_Series"))
            
            #findlag
            lag<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMAX-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-ARIMAX_MLPX_Series(preprocessing = preprocessing,
                                     location=location,
                                     denomination=denomination,
                                     MLP_layer=MLP_layer,
                                     flow=flow,
                                     lag=lag)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            
            result$modelResult$DateExecuted<-result$modelResult$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result$modelResult,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            result$gridsearchNN$DateExecuted<-result$gridsearchNN$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "nn",values = result$gridsearchNN,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"MLP_ARIMA_Series"))
            
            #findlag
            lag<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMA-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-MLP_ARIMA_Series(preprocessing = preprocessing,
                                     location=location,
                                     denomination=denomination,
                                     MLP_layer=MLP_layer,
                                     flow=flow,
                                     lag=lag)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            
            result$modelResult$DateExecuted<-result$modelResult$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result$modelResult,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            result$gridsearchNN$DateExecuted<-result$gridsearchNN$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "nn",values = result$gridsearchNN,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            
          },error=function(e){
            print(e)
          })
          
          tryCatch({
            print(paste(flow,location,denomination,preprocessing,MLP_layer,Sys.time(),"MLPX_ARIMAX_Series"))
            
            #findlag
            lag<-(lag_info %>% filter(Flow==flow,Location==location,Denomination==denomination,Model=="ARIMAX-Individual") %>% select("Lags") %>% slice(1))$Lags[[1]]%>% as.numeric()
            result<-MLPX_ARIMAX_Series(preprocessing = preprocessing,
                                       location=location,
                                       denomination=denomination,
                                       MLP_layer=MLP_layer,
                                       flow=flow,
                                       lag=lag)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
            
            result$modelResult$DateExecuted<-result$modelResult$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "result",values = result$modelResult,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            result$gridsearchNN$DateExecuted<-result$gridsearchNN$DateExecuted %>% as.character()
            append_cmd<-sqlAppendTable(con = mydb,table = "nn",values = result$gridsearchNN,row.names = FALSE )
            dbExecute( conn = mydb , statement = append_cmd )
            
            
          },error=function(e){
            print(e)
          })
          

        }
      }
    }
  }
}