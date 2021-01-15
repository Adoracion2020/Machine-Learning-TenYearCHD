# *******************************************
# VALIDACION CRUZADA REPETIDA Y BOXPLOT para
# 
# LOGISTICA
# AVNNET
# *******************************************


# *********************************
# CRUZADA LOGISTICA
# *********************************

cruzadalogistica <- function(data=data,vardep=NULL,
                             listvars=NULL,grupos=4,sinicio=1234,repe=5)
{
  library(dummies)
  library(MASS)
  library(reshape)
  library(caret)
  library(dplyr)
  library(pROC)
  
  data[,vardep]<-as.factor(data[,vardep])
  
  modelo<-paste(listvars,sep="",collapse="+")
  formu<-formula(paste(vardep,"~",modelo,sep=""))
  
  print(formu)
  # Preparo caret   
  
  set.seed(sinicio)
  control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                        savePredictions = "all",classProbs=TRUE) 
  
  
  # Aplico caret y construyo modelo
  
  regresion <- train(formu,data=data,
                     trControl=control,method="glm",family = binomial(link="logit"))                  
  preditest<-regresion$pred
  
  preditest$prueba<-strsplit(preditest$Resample,"[.]")
  preditest$Fold <- sapply(preditest$prueba, "[", 1)
  preditest$Rep <- sapply(preditest$prueba, "[", 2)
  preditest$prueba<-NULL
  
  tasafallos<-function(x,y) {
    confu<-confusionMatrix(x,y)
    tasa<-confu[[3]][1]
    return(tasa)
  }
  
  # Aplicamos funcion sobre cada Repeticion
  
  medias<-preditest %>%
    group_by(Rep) %>%
    summarize(tasa=1-tasafallos(pred,obs))
  
  # CalculamoS AUC  por cada Repeticion de cv 
  # Definimnos funcion
  
  auc<-function(x,y) {
    curvaroc<-roc(response=x,predictor=y)
    auc<-curvaroc$auc
    return(auc)
  }
  
  # Aplicamos funcion sobre cada Repeticion
  
  mediasbis<-preditest %>%
    group_by(Rep) %>%
    summarize(auc=auc(obs,Yes))
  
  # Unimos la info de auc y de tasafallos
  
  medias$auc<-mediasbis$auc
  
  return(medias)
  
}




# *******************
# CRUZADA avNNet
# *******************


cruzadaavnnetbin<-
  function(data=data,vardep="vardep",
           listvars="listvars",grupos=4,sinicio=1234,repe=5,
           size=c(5),decay=c(0.01),repeticiones=5,itera=100)
  { 
    
    data[,vardep]<-as.factor(data[,vardep])
    
    formu<-formula(paste(vardep,"~.",sep=""))
    
    # Preparo caret   
    # Importante classProbs=TRUE para guardar las probabilidades
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    avnnetgrid <-  expand.grid(size=size,decay=decay,bag=FALSE)
    
    avnnet<- train(formu,data=data,
                   method="avNNet",linout = FALSE,maxit=itera,repeats=repeticiones,
                   trControl=control,tuneGrid=avnnetgrid)
    
    print(avnnet$results)
    
    preditest<-avnnet$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos funcion sobre cada Repeticion
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(tasa=1-tasafallos(pred,obs))
    
    # CalculamoS AUC  por cada Repeticion de cv 
    # Definimnos funcion
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos funcion sobre cada Repeticion
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(medias)
    
  }

