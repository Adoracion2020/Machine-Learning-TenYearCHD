

# *********************************
# RANDOMFOREST
# *********************************

# Igual que bagging, misma funcion, pero en bagging el parámetro MTRY coincide
# con el numero de variables independientes, y en random forest, se modifica de manera
# aleatoria.

# TUNEADO DE MTRY CON CARET

library(caret)

set.seed(12345)
rfgrid<-expand.grid(mtry=c(3,4,5,6,7,8,9,10,11)) # AQUÍ se observa el cambio con respecto a BAGGING

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 

rf<- train(factor(chd)~.,data=saheartbis,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=1000,sampsize=200,nodesize=10,replace=TRUE,
           importance=TRUE)

rf

# IMPORTANCIA DE VARIABLES

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla

barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla))

# PARA PLOTEAR EL ERROR OOB A MEDIDA QUE AVANZAN LAS ITERACIONES
# SE USA DIRECTAMENTE EL PAQUETE randomForest

library(randomForest)
set.seed(12345)

rfbis<-randomForest(factor(chd)~.,
                    data=saheartbis,
                    mtry=3,ntree=5000,sampsize=300,nodesize=10,replace=TRUE)

plot(rfbis$err.rate[,1])

# TUNEADO BÁSICO DEL TAMAÑO DE MUESTRA A SORTEAR
# NOTA: ESTE TRUCO SE PUEDE USAR EN CUALQUIER FUNCIÓN, ÚTIL
# PARA TUNEAR PARÁMETROS QUE CARET NO PERMITE EN EL GRID

for (muestra in seq(100,450,50))
{
  # controlamos la semilla pues bagging depende de ella
  set.seed(12345) 
  rfbis<-randomForest(factor(chd)~.,
                      data=saheartbis,
                      mtry=5,ntree=5000,sampsize=muestra,nodesize=10,replace=TRUE)
  
  plot(rfbis$err.rate[,1],main=muestra,ylim=c(0.25,0.5))
  
}

# Ahora se comprueba con validación cruzada con caret

rfgrid<-expand.grid(mtry=c(5))

rf<- train(factor(chd)~.,data=saheartbis,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,ntree=1000,sampsize=100,nodesize=10,replace=TRUE)

rf

# La función cruzadarfbin permite plantear random forest




################################################################
# EJEMPLO DE APUNTES DEL PROFESOR:
################################################################



load ("c:/saheartbis.Rda")
source ("c:/cruzadas avnnet y log binaria.R")
source ("c:/cruzada arbolbin.R")
source ("c:/cruzada rf binaria.R")

medias1<-cruzadalogistica(data=saheartbis,
                          vardep="chd",listvars=c("sbp", "tobacco", "ldl",
                                                   "adiposity",  "obesity", "famhist.Absent"),
                          grupos=4,sinicio=1234,repe=10)

medias1$modelo="Logística"


medias2<-cruzadaavnnetbin(data=saheartbis,
                          vardep="chd",listvars=c("sbp", "tobacco",
                                                   "ldl", "adiposity",  "obesity", "famhist.Absent"),
                          grupos=4,sinicio=1234,repe=10,
                          size=c(5),decay=c(0.1),repeticiones=5,itera=200)

medias2$modelo="avnnet"


medias3<-cruzadaarbolbin(data=saheartbis,
                         vardep="chd",listvars=c("sbp", "tobacco",
                                                  "ldl", "adiposity",  "obesity", "famhist.Absent"),
                         grupos=4,sinicio=1234,repe=10,
                         cp=c(0),minbucket =5)

medias3$modelo="arbol"

medias4<-cruzadarfbin(data=saheartbis, vardep="chd",
                      listvars=c("sbp", "tobacco",
                                  "ldl", "adiposity",  "obesity", "famhist.Absent"),
                      grupos=4,sinicio=1234,repe=10,nodesize=10,
                      mtry=6,ntree=100,replace=TRUE)

medias4$modelo="bagging"

medias5<-cruzadarfbin(data=saheartbis, vardep="chd",
                      listvars=c("sbp", "tobacco",
                                  "ldl", "adiposity",  "obesity", "famhist.Absent"),
                      grupos=4,sinicio=1234,repe=10,nodesize=10,
                      mtry=5,ntree=100,replace=TRUE)

medias5$modelo="rf"

union1<-rbind(medias1,medias2,medias3,medias4,medias5)

par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS",col="pink")
boxplot(data=union1,auc~modelo,main="AUC")

