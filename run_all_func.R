source("individual_model/ARIMA-Individual-func.R")
source("individual_model/ARIMAX-Individual-func.R")
source("individual_model/MLP-Individual-func.R")
source("individual_model/MLPX-Individual-func.R")

source("hybrid_model/ARIMA-MLP-Parallel-func.R")
source("hybrid_model/ARIMAX-MLPX-Parallel-func.R")

compiled_result<-data.frame()
nn_gridsearch_result<-data.frame()

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(TRUE,FALSE))
    {
      result<-ARIMA_Individual(preprocessing = preprocessing,
                               location=location,
                               denomination=denomination)
      
      compiled_result<-rbind(compiled_result,result)
      
      result<-ARIMAX_Individual(preprocessing = preprocessing,
                                location=location,
                                denomination=denomination)
      
      compiled_result<-rbind(compiled_result,result)
    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(TRUE,FALSE))
    {
      for (MLP_layer in c(1,2))
      {
        print(paste(location,denomination,preprocessing,MLP_layer))
        
        result<-MLP_Individual(preprocessing = preprocessing,
                               location=location,
                               denomination=denomination,MLP_layer=MLP_layer)
        compiled_result<-rbind(compiled_result,result$modelResult)
        nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
      }
      
    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(TRUE,FALSE))
    {
      for (MLP_layer in c(1,2))
      {
        print(paste(location,denomination,preprocessing,MLP_layer))
        
        result<-MLPX_Individual(preprocessing = preprocessing,
                                location=location,
                                denomination=denomination,MLP_layer=MLP_layer)
        compiled_result<-rbind(compiled_result,result$modelResult)
        nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
      }
      
    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(TRUE,FALSE))
    {
      for (MLP_layer in c(1,2))
      {
<<<<<<< HEAD

          print(paste(location,denomination,preprocessing,MLP_layer))
          
          result<-ARIMA_MLP_Parallel(preprocessing = preprocessing,
                                     location=location,
                                     denomination=denomination,
                                     MLP_layer=MLP_layer)
          compiled_result<-rbind(compiled_result,result$modelResult)
          nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
=======
        
        print(paste(location,denomination,preprocessing,MLP_layer))
        
        result<-ARIMA_MLP_Parallel(preprocessing = preprocessing,
                                   location=location,
                                   denomination=denomination,
                                   MLP_layer=MLP_layer)
        compiled_result<-rbind(compiled_result,result$modelResult)
        nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
>>>>>>> 1cfe16fa22cb4f08bd2a652e5cd8807288da254e
        
      }
    }
  }
}

for (location in c("Jakarta"))
{
  for (denomination in c("K100000","K50000","K20000","K10000","K5000","K2000","K1000"))
  {
    for (preprocessing in c(TRUE,FALSE))
    {
      for (MLP_layer in c(1,2))
      {
<<<<<<< HEAD
          print(paste(location,denomination,preprocessing,MLP_layer))
          
          result<-ARIMAX_MLPX_Parallel(preprocessing = preprocessing,
                                     location=location,
                                     denomination=denomination,
                                     MLP_layer=MLP_layer)
          compiled_result<-rbind(compiled_result,result$modelResult)
          nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
=======
        print(paste(location,denomination,preprocessing,MLP_layer))
        
        result<-ARIMAX_MLPX_Parallel(preprocessing = preprocessing,
                                     location=location,
                                     denomination=denomination,
                                     MLP_layer=MLP_layer)
        compiled_result<-rbind(compiled_result,result$modelResult)
        nn_gridsearch_result<-rbind(nn_gridsearch_result,result$gridsearchNN)
>>>>>>> 1cfe16fa22cb4f08bd2a652e5cd8807288da254e
        
      }
    }
  }
}
