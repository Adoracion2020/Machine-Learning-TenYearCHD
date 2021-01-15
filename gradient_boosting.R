
cruzadagbmbin<-
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
    
    return(medias)
    
  }


