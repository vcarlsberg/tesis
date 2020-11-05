source("individual_model/ARIMA-Individual-func.R")
source("individual_model/ARIMAX-Individual-func.R")
source("individual_model/MLP-Individual-func.R")
source("individual_model/MLPX-Individual-func.R")

source("hybrid_model/ARIMA-MLP-Parallel-func.R")
source("hybrid_model/ARIMAX-MLPX-Parallel-func.R")

source("hybrid_model/ARIMA-MLP-Series-func.R")
source("hybrid_model/ARIMAX-MLPX-Series-func.R")
source("hybrid_model/MLP-ARIMA-Series-func.R")
source("hybrid_model/MLPX-ARIMAX-Series-func.R")



compiled_result<-data.frame()
nn_gridsearch_result<-data.frame()

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(1,0,-1))
    {
      tryCatch({
        print(paste(location,denomination,preprocessing,Sys.time(),"ARIMA"))
        
        result<-ARIMA_Individual(preprocessing = preprocessing,
                                 location=location,
                                 denomination=denomination)
        compiled_result<-rbind(compiled_result,result$modelResult)
      },error=function(e){
        print(e)
      })
      
      tryCatch({
        print(paste(location,denomination,preprocessing,Sys.time(),"ARIMAX"))
        
        result<-ARIMA_Individual(preprocessing = preprocessing,
                                 location=location,
                                 denomination=denomination)
        compiled_result<-rbind(compiled_result,result$modelResult)
      },error=function(e){
        print(e)
      })

    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(1,0,-1))
    {
      for (MLP_layer in c(1,2))
      {
        tryCatch({
          print(paste(location,denomination,preprocessing,MLP_layer,Sys.time(),"MLP_Individual"))
          
          result<-MLP_Individual(preprocessing = preprocessing,
                                  location=location,
                                  denomination=denomination,MLP_layer=MLP_layer)
          compiled_result<-rbind(compiled_result,result$modelResult)
          nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
        },error=function(e){
          print(e)
        })
        
      }
      
    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(1,0,-1))
    {
      for (MLP_layer in c(1,2))
      {
        tryCatch({
          print(paste(location,denomination,preprocessing,MLP_layer,Sys.time(),"MLPX_Individual"))
          
          result<-MLPX_Individual(preprocessing = preprocessing,
                                  location=location,
                                  denomination=denomination,MLP_layer=MLP_layer)
          compiled_result<-rbind(compiled_result,result$modelResult)
          nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
        },error=function(e){
          print(e)
        })

      }
      
    }
  }
}


for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(1,0,-1))
    {
      for (MLP_layer in c(1,2))
      {

        tryCatch({
            print(paste(location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMA_MLP_Parallel"))
            
            result<-ARIMA_MLP_Parallel(preprocessing = preprocessing,
                                    location=location,
                                    denomination=denomination,MLP_layer=MLP_layer)
            compiled_result<-rbind(compiled_result,result$modelResult)
            nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
          },error=function(e){
            print(e)
          })
      }
    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(1,0,-1))
    {
      for (MLP_layer in c(1,2))
      {
        
        tryCatch({
          print(paste(location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMAX_MLPX_Parallel"))
          
          result<-ARIMAX_MLPX_Parallel(preprocessing = preprocessing,
                                     location=location,
                                     denomination=denomination,MLP_layer=MLP_layer)
          compiled_result<-rbind(compiled_result,result$modelResult)
          nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
        },error=function(e){
          print(e)
        })
      }
    }
  }
}




for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(1,0))
    {
      for (MLP_layer in c(1,2))
      {
        
        tryCatch({
          print(paste(location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMA_MLP_Series"))
          
          result<-ARIMA_MLP_Series(preprocessing = preprocessing,
                                       location=location,
                                       denomination=denomination,MLP_layer=MLP_layer)
          compiled_result<-rbind(compiled_result,result$modelResult)
          nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
        },error=function(e){
          print(e)
        })
      }
    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(1,0))
    {
      for (MLP_layer in c(1,2))
      {
        
        tryCatch({
          print(paste(location,denomination,preprocessing,MLP_layer,Sys.time(),"ARIMAX_MLPX_Series"))
          
          result<-ARIMAX_MLPX_Series(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,MLP_layer=MLP_layer)
          compiled_result<-rbind(compiled_result,result$modelResult)
          nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
        },error=function(e){
          print(e)
        })
      }
    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(1,0))
    {
      for (MLP_layer in c(1,2))
      {
        
        tryCatch({
          print(paste(location,denomination,preprocessing,MLP_layer,Sys.time(),"MLP_ARIMA_Series"))
          
          result<-MLP_ARIMA_Series(preprocessing = preprocessing,
                                     location=location,
                                     denomination=denomination,MLP_layer=MLP_layer)
          compiled_result<-rbind(compiled_result,result$modelResult)
          nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
        },error=function(e){
          print(e)
        })
      }
    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(1,0))
    {
      for (MLP_layer in c(1,2))
      {
        
        tryCatch({
          print(paste(location,denomination,preprocessing,MLP_layer,Sys.time(),"MLPX_ARIMAX_Series"))
          
          result<-MLPX_ARIMAX_Series(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,MLP_layer=MLP_layer)
          compiled_result<-rbind(compiled_result,result$modelResult)
          nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
        },error=function(e){
          print(e)
        })
      }
    }
  }
}


location="Jakarta"
denomination="K5000"
preprocessing=TRUE
MLP_layer=1

result<-MLP_Individual(preprocessing = preprocessing,
                         location=location,
                         denomination=denomination,
                         MLP_layer=MLP_layer)
