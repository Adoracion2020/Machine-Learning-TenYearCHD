# *********************************
# CRUZADAS PARA ENSAMBLADO DEPENDIENTE BINARIA
# *********************************
# VALIDACIÓN CRUZADA REPETIDA Y BOXPLOT para
# 
# LOGISTICA
# AVNNET
# RF
# GBM
# SVM

# *********************************
library(plyr)
detach(package:plyr)
library(dummies)
library(MASS)
library(reshape)
library(caret)
library(dplyr)
library(pROC)


# *********************************
# CRUZADA LOGISTICA
# ********************************* 

cruzadalogisticaEns <- function(data=data,vardep=NULL,
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
  
  return(list(medias,preditest))
  
}




# *********************************
# CRUZADA avNNet
# **************

cruzadaavnnetbinEns <-
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
    
    return(list(medias,preditest))
    
  }


# *********************************
# CRUZADA Random Forest
# ******************************


cruzadarfbinEns <-
  function(data=data,vardep="vardep",
           listvars="listvars",grupos=4,sinicio=1234,repe=5,nodesize=20,
           mtry=2,ntree=50,replace=TRUE)
  { 
    
    data[,vardep]<-as.factor(data[,vardep])
    
    modelo<-paste(listvars,sep="",collapse="+")
    formu<-formula(paste(vardep,"~",modelo,sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    rfgrid <-expand.grid(mtry=mtry)
    
    rf<- train(formu,data=data,
               method="rf",trControl=control,
               tuneGrid=rfgrid,nodesize=nodesize,replace=replace,
               ntree=ntree)
    
    print(rf$results)
    
    preditest<-rf$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(tasa=1-tasafallos(pred,obs))
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(list(medias,preditest))
    
  }

# ***************************************************************
# gbm : parámetros

# 
#     Number of Boosting Iterations (n.trees, numeric)
#     Max Tree Depth (max.depth, numeric)
#     Shrinkage (shrinkage, numeric)
#     Min. Terminal Node Size (n.minobsinnode, numeric)
#    
# ***************************************************************



cruzadagbmbinEns <-
  function(data=data,vardep="vardep",
           listvars="listvars",
           grupos=4,sinicio=1234,repe=5,
           n.minobsinnode=20,shrinkage=0.1,n.trees=100,interaction.depth=2)
  { 
    
    data[,vardep]<-as.factor(data[,vardep])
    
    modelo<-paste(listvars,sep="",collapse="+")
    formu<-formula(paste(vardep,"~",modelo,sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    
    
    gbmgrid <-expand.grid(n.minobsinnode=n.minobsinnode,
                          shrinkage=shrinkage,n.trees=n.trees,
                          interaction.depth=interaction.depth)
    
    gbm<- train(formu,data=data,
                method="gbm",trControl=control,
                tuneGrid=gbmgrid,distribution="bernoulli",verbose=FALSE)
    
    print(gbm$results)
    
    preditest<-gbm$pred
    
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(tasa=1-tasafallos(pred,obs))
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(list(medias,preditest))
    
  }



# ***************************************************************
# svmLinear: parámetros

# Cost (C, numeric)     

# PONER linout = FALSE
# ***************************************************************


cruzadaSVMbinEns <-
  function(data=data,vardep="vardep",
           listvars="listvars",
           grupos=4,sinicio=1234,repe=5,
           C=1,replace=TRUE)
  { 
    
    data[,vardep]<-as.factor(data[,vardep])
    
    modelo<-paste(listvars,sep="",collapse="+")
    formu<-formula(paste(vardep,"~",modelo,sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C)
    
    SVM<- train(formu,data=data,
                method="svmLinear",trControl=control,
                tuneGrid=SVMgrid,replace=replace)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(tasa=1-tasafallos(pred,obs))
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(list(medias,preditest))
    
  }





cruzadaSVMbinPolyEns<-
  function(data=data,vardep="vardep",
           listvars="listvars",
           grupos=4,sinicio=1234,repe=5,
           C=1,degree=2,scale=1)
  { 
    
    data[,vardep]<-as.factor(data[,vardep])
    
    modelo<-paste(listvars,sep="",collapse="+")
    formu<-formula(paste(vardep,"~",modelo,sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C,degree=degree,scale=scale)
    
    SVM<- train(formu,data=data,
                method="svmPoly",trControl=control,
                tuneGrid=SVMgrid,replace=replace)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(tasa=1-tasafallos(pred,obs))
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(list(medias,preditest))
    
  }



# ***************************************************************
# svmRadial: parámetros

# Sigma (sigma, numeric)
# Cost (C, numeric)

# PONER linout = FALSE
# ***************************************************************



cruzadaSVMbinRBFEns <-
  function(data=data,vardep="vardep",
           listvars="listvars",
           grupos=4,sinicio=1234,repe=5,
           C=1,sigma=1)
  { 
    
    
    data[,vardep]<-as.factor(data[,vardep])
    
    modelo<-paste(listvars,sep="",collapse="+")
    formu<-formula(paste(vardep,"~",modelo,sep=""))
    
    # Preparo caret   
    
    set.seed(sinicio)
    control<-trainControl(method = "repeatedcv",number=grupos,repeats=repe,
                          savePredictions = "all",classProbs=TRUE) 
    
    # Aplico caret y construyo modelo
    
    SVMgrid <-expand.grid(C=C,sigma=sigma)
    
    SVM<- train(formu,data=data,
                method="svmRadial",trControl=control,
                tuneGrid=SVMgrid,replace=replace)
    
    print(SVM$results)
    
    preditest<-SVM$pred
    
    preditest$prueba<-strsplit(preditest$Resample,"[.]")
    preditest$Fold <- sapply(preditest$prueba, "[", 1)
    preditest$Rep <- sapply(preditest$prueba, "[", 2)
    preditest$prueba<-NULL
    
    tasafallos<-function(x,y) {
      confu<-confusionMatrix(x,y)
      tasa<-confu[[3]][1]
      return(tasa)
    }
    
    # Aplicamos función sobre cada Repetición
    
    medias<-preditest %>%
      group_by(Rep) %>%
      summarize(tasa=1-tasafallos(pred,obs))
    
    # CalculamoS AUC  por cada Repetición de cv 
    # Definimnos función
    
    auc<-function(x,y) {
      curvaroc<-roc(response=x,predictor=y)
      auc<-curvaroc$auc
      return(auc)
    }
    
    # Aplicamos función sobre cada Repetición
    
    mediasbis<-preditest %>%
      group_by(Rep) %>%
      summarize(auc=auc(obs,Yes))
    
    # Unimos la info de auc y de tasafallos
    
    medias$auc<-mediasbis$auc
    
    return(list(medias,preditest))
    
  }

