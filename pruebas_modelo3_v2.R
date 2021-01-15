
############################################################################################
##                                             MODELO 3                                   ##
############################################################################################

library(sas7bdat)

data <- read.sas7bdat("C:\\Users\\dory_\\Desktop\\Machine learning\\Practica2\\Codigo_R\\Codigo_R\\em_save_train.sas7bdat")
names(data)

# La variable dependiente (Objetivo), no puede tener 0 y 1 como valores, da error
# Esto se soluciona cambiando los valores a Yes, No (por ejemplo)
# data$TenYearCHD[data$TenYearCHD==1] <- "Yes"
# data$TenYearCHD[data$TenYearCHD==0] <- "No"

#Esta opcion mejor que la de arriba
data$TenYearCHD<-ifelse(data$TenYearCHD==1,"Yes","No")


# No se puede crear redes con valores Factor con menos de dos niveles, por lo que hacemos drop 
# de la columna X_WARN_ para evitarlo
data <- subset(data, select = -c(X_WARN_))
# Comprobamos la estructura del dataframe para ver que se ha dropeado la columna
str(data)


# SET MODELO 3
modelo3 <- c("Edad","CigsPorDia","PresionSanguinea","Hombre")


#######################################################################
###            Cruzada logistica y AvNNet:                          ###
#######################################################################


# AvNNet:
#-------------------------------------

# Realizamos un primer modelo aplicando vectores obre los parámetros para
# determinar el mejor conjunto de estos

set.seed(12345)


avnnetgrid <-  expand.grid(size=c(4,6,8,10,12),decay=c(0.1,0.01,0.001),bag=FALSE)

control<-trainControl(method = "cv",number=4,
                      savePredictions = "all",classProbs=TRUE) 

avnnet<- train(TenYearCHD~Edad+CigsPorDia+PresionSanguinea+Hombre,data=data,
               method="avNNet",linout = FALSE,maxit=200,repeats=5,
               trControl=control,tuneGrid=avnnetgrid)

avnnet


# Se observa que la mejor combincion es 4 nodos / 0.001 learning rate
# ()


##---------------------------------------------

medias1_m3<-cruzadalogistica(data=data,
                          vardep="TenYearCHD",listvars=modelo3, grupos=4,sinicio=1234,repe=5)
# 
medias1_m3$modelo="Logistica"
# 
medias1_m3$auc
# 
medias2_m3<-cruzadaavnnetbin(data=data,
                          vardep="TenYearCHD",listvars=modelo3,grupos=4,sinicio=1234,repe=5,
                          size=c(12),decay=c(0.1),repeticiones=5,itera=200)
# 
medias2_m3$modelo="avnnet1"
#
# 
# 
medias3_m3<-cruzadaavnnetbin(data=data,
                          vardep="TenYearCHD",listvars=modelo3,grupos=4,sinicio=1234,repe=5,
                          size=c(12),decay=c(0.01),repeticiones=5,itera=200)
# 
medias3_m3$modelo="avnnet2"
# 
# 
# 
medias4_m3<-cruzadaavnnetbin(data=data,
                          vardep="TenYearCHD",listvars=modelo3,grupos=4,sinicio=1234,repe=5,
                          size=c(12),decay=c(0.001),repeticiones=5,itera=200)
# 
medias4_m3$modelo="avnnet3"
# 
# 
# Mejor parametrización obtenida en el estudio con vectores, NO ES REAL, al aplicar validación
# cruzada repetida se observa mejor performance con 12 nodos
medias5_m3<-cruzadaavnnetbin(data=data,
                          vardep="TenYearCHD",listvars=modelo3,grupos=4,sinicio=1234,repe=5,
                          size=c(6),decay=c(0.01),repeticiones=5,itera=200)
# 
medias5_m3$modelo="avnnet4"
# 
union1<-rbind(medias1_m3,medias2_m3,medias3_m3,medias4_m3,medias5_m3)
# mejor: medias4 (AUC), medias5 (tasa de fallos)
par(cex.axis=0.5)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS ~ M2")
boxplot(data=union1,auc~modelo,main="AUC ~ M2")
# 
# 



########################################################################################
# BAGGING, RANDOM FOREST, GRADIENT BOOSTING, SUPPORT VECTOR MACHINES
########################################################################################

# Bagging y Random Forest:
#-------------------------------------

# Realizamos un primer modelo aplicando vectores obre los parámetros para
# determinar el mejor conjunto de estos

set.seed(12345)
rfgrid<-expand.grid(mtry=c(1,2,3))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 


rf<- train(TenYearCHD~Edad+CigsPorDia+PresionSanguinea+Hombre,data=data,
           method="rf",trControl=control,tuneGrid=rfgrid,
           linout = FALSE,replace=TRUE, nodesize=12, ntree=200,
           importance=TRUE)

rf
# Observamos que el mejor mtry es 1, con lo cual será el que usemos para nuestros
# modelos de random forest

library(randomForest)

# IMPORTANCIA DE VARIABLES

final<-rf$finalModel

tabla<-as.data.frame(importance(final))
tabla<-tabla[order(-tabla$MeanDecreaseAccuracy),]
tabla

barplot(tabla$MeanDecreaseAccuracy,names.arg=rownames(tabla))
# ---------------------------------------------------------------------

# Modelos:
# 
medias6_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                      grupos=4,sinicio=1234,repe=5,nodesize=15,
                      mtry=4,ntree=200,replace=TRUE)
# 
medias6_m3$modelo="bagging1"
# 
# mas arboles no mejora practicamente nada, pruebas con ntree = 1000 y 2000
medias7_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                      grupos=4,sinicio=1234,repe=5,nodesize=20,
                      mtry=4,ntree=600,replace=TRUE)
# 
medias7_m3$modelo="bagging2"
# 
medias8_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                      grupos=4,sinicio=1234,repe=5,nodesize=15,
                      mtry=1,ntree=100,replace=TRUE)
# 
medias8_m3$modelo="rf1"
#
# 
medias9_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                      grupos=4,sinicio=1234,repe=5,nodesize=15,
                      mtry=1,ntree=300,replace=TRUE)
# 
medias9_m3$modelo="rf2"
#
# a menos arboles, la varianza desciende mucho, pero el sesgo esta bastante por debajo del resto de modelos también
medias10_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                       grupos=4,sinicio=1234,repe=5,nodesize=20,
                       mtry=1,ntree=600,replace=TRUE)
# 
medias10_m3$modelo="rf3"
# 
# Comparamos bagging y random forest con logistica
union1<-rbind(medias1_m3,medias6_m3,medias7_m3,medias8_m3,medias9_m3,medias10_m3)
# mejor: medias10
par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")
# 
#---------------------------------------------------------------

## Gradient Boosting
# -------------------------

set.seed(12345)
gbmgrid <-expand.grid(n.minobsinnode=c(4,6,8,10,12),
                      shrinkage=c(0.1,0.01,0.001),n.trees=c(500,1000,2000,5000),
                      interaction.depth=c(2,3,4))

control<-trainControl(method = "cv",number=4,savePredictions = "all",
                      classProbs=TRUE) 


gbm<- train(TenYearCHD~Edad+CigsPorDia+PresionSanguinea+Hombre,data=data,
            method="gbm",trControl=control,
            tuneGrid=gbmgrid,distribution="bernoulli",verbose=FALSE)

gbm

# 167 combinaciones diferentes (EL PC CASI MUERE). Las mejores combinación
# shrinkage  interaction.depth  n.minobsinnode  n.trees  Accuracy   Kappa  
# 0.001      3                  10              2000     0.8501648  0.031702879
# 0.010      2                  12               500     0.8501650  0.063435102
# 0.001      4                  12              2000     0.8506367  0.040736282

# TRAS PROBARLAS, MANTENGO LAS ANTERIORES PORQUE DAN MEJORES RESULTADOS:
# shrinkage (learning rate) mas bajo funciona mejor, como en las redes neuronales
# alrededor de 5000 arboles estan los mejores valores, menos baja mucho, y mas es casi igual un poco por debajo mas arboles
# mas interction.depth = peor performance, baja el sesgo y aumenta la vrianza
medias11_m3<-cruzadagbmbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        n.minobsinnode=6,shrinkage=0.001,n.trees=2000,interaction.depth=4)
# 
medias11_m3$modelo="gbm1"
#
# 
medias12_m3<-cruzadagbmbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        n.minobsinnode=10,shrinkage=0.001,n.trees=2000,interaction.depth=3)
# 
medias12_m3$modelo="gbm2"

medias13_m3<-cruzadagbmbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        n.minobsinnode=12,shrinkage=0.001,n.trees=2000,interaction.depth=3)
# 
medias13_m3$modelo="gbm3"
# 
# 

# Comparamos logistica con Graient Boosting
union1<-rbind(medias1_m3,medias11_m3,medias12_m3,medias13_m3)
# mejor: medias12
par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")



###########################################################################
#  SVM LINEAL: SOLO PARÁMETRO C

set.seed(12345)
SVMgrid<-expand.grid(C=c(1,10,100))

control<-trainControl(method = "cv",number=4,savePredictions = "all") 

SVM<- train(TenYearCHD~Edad+CigsPorDia+PresionSanguinea+Hombre,data=data,
            method="svmLinear",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM$results
plot(SVM$results$C,SVM$results$Accuracy)

# Da igual el valor de C que el accuracy siempre es igual: 0.8480415
# MUY raro

#---------------------------------------------


# Aumentando C no e observa gran cambio respecto a la tasa de fallos, que es relativamente baja
# respecto a otros modelos, aunque los valores AUC son realmente malos, llegando al punto de corte en algunos casos
medias14_m3<-cruzadaSVMbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        C=1)
# 
medias14_m3$modelo="SVM_1"
#
medias15_m3<-cruzadaSVMbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        C=10)
# 
medias15_m3$modelo="SVM_2"
#
#
medias16_m3<-cruzadaSVMbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        C=100)
# 
medias16_m3$modelo="SVM_3"
# 


#------------------------------------------------------------------
#  SVM Polinomial: PARÁMETROS C, degree, scale

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10),
                     degree=c(2,3),scale=c(0.1,0.5,1,2,5))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM<- train(TenYearCHD~Edad+CigsPorDia+PresionSanguinea+Hombre,data=data,
            method="svmPoly",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

SVM$results

dat<-as.data.frame(SVM$results)
library(ggplot2)

# PLOT DE DOS VARIABLES CATEGÓRICAS, UNA CONTINUA
ggplot(dat, aes(x=factor(C), y=Accuracy, 
                color=factor(degree),pch=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# SOLO DEGREE=2
dat2<-dat[dat$degree==2,]  

ggplot(dat2, aes(x=factor(C), y=Accuracy, 
                 colour=factor(scale))) +
  geom_point(position=position_dodge(width=0.5),size=3)

# todos los parametros lineales, excepto scale = 5
#------------------------------------------------------------------------

# kernel polinomial sigue presentando valores muy malos
medias17_m3<-cruzadaSVMbinPoly(data=data, vardep="TenYearCHD",listvars=modelo3,
                            grupos=4,sinicio=1234,repe=5,
                            C=0.5,degree=2,scale=5)
# 
medias17_m3$modelo="SVMPoly_1"
# 
# A mayor C mayor varianza y el sesgo no mejora
medias18_m3<-cruzadaSVMbinPoly(data=data, vardep="TenYearCHD",listvars=modelo3,
                            grupos=4,sinicio=1234,repe=5,
                            C=1,degree=2,scale=5)
# 
medias18_m3$modelo="SVMPoly_2"
# Un poco de mejora de sesgo, pero aumenta aun mas la varianza y siguen siendo valores muy malos
medias19_m3<-cruzadaSVMbinPoly(data=data, vardep="TenYearCHD",listvars=modelo3,
                            grupos=4,sinicio=1234,repe=5,
                            C=0.05,degree=1,scale=5)
# 
medias19_m3$modelo="SVMPoly_3"
#

#--------------------------------------------------------------
#  SVM RBF: PARÁMETROS C, sigma

SVMgrid<-expand.grid(C=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30),
                     sigma=c(0.01,0.05,0.1,0.2,0.5,1,2,5,10,30))

control<-trainControl(method = "cv",
                      number=4,savePredictions = "all") 


SVM<- train(TenYearCHD~Edad+CigsPorDia+PresionSanguinea+Hombre,data=data,
            method="svmRadial",trControl=control,
            tuneGrid=SVMgrid,verbose=FALSE)

SVM

dat<-as.data.frame(SVM$results)

ggplot(dat, aes(x=factor(C), y=Accuracy, 
                color=factor(sigma)))+ 
  geom_point(position=position_dodge(width=0.5),size=3)

# C=5 y sigma=0.2 mejor combinacion
#--------------------------------------------------------------


# Valores malos pero mejora la varianza con respecto a los otros 2 kernels
medias20_m3<-cruzadaSVMbinRBF(data=data, vardep="TenYearCHD",listvars=modelo3,
                           grupos=4,sinicio=1234,repe=5,
                           C=30,sigma=0.1)
# 
medias20_m3$modelo="SVMRBF"

medias21_m3<-cruzadaSVMbinRBF(data=data, vardep="TenYearCHD",listvars=modelo3,
                           grupos=4,sinicio=1234,repe=5,
                           C=5,sigma=0.2)
# 
medias21_m3$modelo="SVMRBF_2"
# Aumenta mucho la varianza con respecto a los otros tuneados
medias22_m3<-cruzadaSVMbinRBF(data=data, vardep="TenYearCHD",listvars=modelo3,
                           grupos=4,sinicio=1234,repe=5,
                           C=10,sigma=0.2)
# 
medias22_m3$modelo="SVMRBF_3"
# 
# Comparamos logistica con SVM (Linear, polinomial y Gauss)
union1<-rbind(medias1_m3,medias14_m3,medias15_m3,medias16_m3,medias17_m3,medias18_m3,medias19_m3,medias20_m3,medias21_m3,medias22_m3)
# mejor: medias19 
par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")


# Comparamos mejores valores de cada modelo para todos los algoritmos, redes y logistica
union1<-rbind(medias1_m3,medias3_m3,medias4_m3,medias7_m3,medias9_m3,medias12_m3,medias19_m3)
# Regresion gana. Gradient boosting cercano en tasa de fallos, red avnnet2 mejor auc, pero redavnnet3 y gbm muy cerca
par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")
# 
# 
