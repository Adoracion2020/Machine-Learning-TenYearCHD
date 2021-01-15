
############################################################################################
##                                             MODELO 3                                   ##
############################################################################################

library(sas7bdat)

data <- read.sas7bdat("C:\\Users\\alex_\\Desktop\\DORI\\Practica_2\\em_save_train.sas7bdat")
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


# Cruzada Logistica y AvNNet


# MODELO 3:



medias1_m3<-cruzadalogistica(data=data,
                             vardep="TenYearCHD",listvars=modelo3, grupos=4,sinicio=1234,repe=5)
# 
medias1_m3$modelo="Logistica"
# 
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
# 
medias5_m3<-cruzadaavnnetbin(data=data,
                             vardep="TenYearCHD",listvars=modelo3,grupos=4,sinicio=1234,repe=5,
                             size=c(12),decay=c(0.001),repeticiones=5,itera=300)
# 
medias5_m3$modelo="avnnet4"
# 
#
#
medias6_m3<-cruzadaavnnetbin(data=data,
                             vardep="TenYearCHD",listvars=modelo3,grupos=4,sinicio=1234,repe=6,
                             size=c(12),decay=c(0.01),repeticiones=6,itera=200)
# 
medias6_m3$modelo="avnnet5"


union2<-rbind(medias1_m3,medias2_m3,medias3_m3,medias4_m3,medias5_m3,medias6_m3)
# mejor: medias4_m3 (tasa fallos), medias6_m3 (AUC)
par(cex.axis=0.5)
boxplot(data=union2,tasa~modelo,main="TASA FALLOS ~ M3")
boxplot(data=union2,auc~modelo,main="AUC ~ M3")
# 
# 


########################################################################################
# BAGGING, RANDOM FOREST, GRADIENT BOOSTING, SUPPORT VECTOR MACHINES
########################################################################################


# 
medias6_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                      grupos=4,sinicio=1234,repe=5,nodesize=12,
                      mtry=4,ntree=200,replace=TRUE)
# 
medias6_m3$modelo="bagging1"
# 
# mas arboles no mejora practicamente nada, pruebas con ntree = 1000 y 2000
medias7_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                      grupos=4,sinicio=1234,repe=5,nodesize=12,
                      mtry=4,ntree=600,replace=TRUE)
# 
medias7_m3$modelo="bagging2"
# 
medias8_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                      grupos=4,sinicio=1234,repe=5,nodesize=12,
                      mtry=3,ntree=200,replace=TRUE)
# 
medias8_m3$modelo="rf1"
#
# 
medias9_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                      grupos=4,sinicio=1234,repe=5,nodesize=12,
                      mtry=2,ntree=600,replace=TRUE)
# 
medias9_m3$modelo="rf2"
#
# a menos arboles, la varianza desciende mucho, pero el sesgo esta bastante por debajo del resto de modelos también
medias10_m3<-cruzadarfbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                       grupos=4,sinicio=1234,repe=5,nodesize=12,
                       mtry=2,ntree=50,replace=TRUE)
# 
medias10_m3$modelo="rf3"
# 
# Comparamos bagging y random forest con logistica
union1<-rbind(medias1_m3,medias6_m3,medias7_m3,medias8_m3,medias9_m3,medias10_m3)
# mejor: medias9_m3
par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")
# 
# 

# shrinkage (learning rate) mas bajo funciona mejor, como en las redes neuronales
# alrededor de 5000 arboles estan los mejores valores, menos baja mucho, y mas es casi igual un poco por debajo mas arboles
# mas interction.depth = peor performance, baja el sesgo y aumenta la vrianza
medias11_m3<-cruzadagbmbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        n.minobsinnode=10,shrinkage=0.001,n.trees=5000,interaction.depth=3)
# 
medias11_m3$modelo="gbm1"
#
# 
medias12_m3<-cruzadagbmbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        n.minobsinnode=12,shrinkage=0.001,n.trees=5000,interaction.depth=2)
# 
medias12_m3$modelo="gbm2"
# 

# Comparamos logistica con Graient Boosting
union1<-rbind(medias1_m3,medias11_m3,medias12_m3)
# mejor: medias12_m3
par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")

# Aumentando C no e observa gran cambio respecto a la tasa de fallos, que es relativamente baja
# respecto a otros modelos, aunque los valores AUC son realmente malos, llegando al punto de corte en algunos casos
medias13_m3<-cruzadaSVMbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        C=0.5)
# 
medias13_m3$modelo="SVM_1"
#
medias14_m3<-cruzadaSVMbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        C=0.05)
# 
medias14_m3$modelo="SVM_2"
#
#
medias15_m3<-cruzadaSVMbin(data=data, vardep="TenYearCHD",listvars=modelo3,
                        grupos=4,sinicio=1234,repe=5,
                        C=1)
# 
medias15_m3$modelo="SVM_3"
# 
# kernel polinomial sigue presentando valores muy malos
medias16_m3<-cruzadaSVMbinPoly(data=data, vardep="TenYearCHD",listvars=modelo3,
                            grupos=4,sinicio=1234,repe=5,
                            C=0.5,degree=2,scale=0.1)
# 
medias16_m3$modelo="SVMPoly_1"
# 
# A mayor C mayor varianza y el sesgo no mejora
medias17_m3<-cruzadaSVMbinPoly(data=data, vardep="TenYearCHD",listvars=modelo3,
                            grupos=4,sinicio=1234,repe=5,
                            C=1,degree=2,scale=0.1)
# 
medias17_m3$modelo="SVMPoly_2"
# Un poco de mejora de sesgo, pero aumenta aun mas la varianza y siguen siendo valores muy malos
medias18_m3<-cruzadaSVMbinPoly(data=data, vardep="TenYearCHD",listvars=modelo3,
                            grupos=4,sinicio=1234,repe=5,
                            C=0.05,degree=1,scale=0.1)
# 
medias18_m3$modelo="SVMPoly_3"
#
# Valores malos pero mejora la varianza con respecto a los otros 2 kernels
medias19_m3<-cruzadaSVMbinRBF(data=data, vardep="TenYearCHD",listvars=modelo3,
                           grupos=4,sinicio=1234,repe=5,
                           C=1,sigma=0.1)
# 
medias19_m3$modelo="SVMRBF"

medias20_m3<-cruzadaSVMbinRBF(data=data, vardep="TenYearCHD",listvars=modelo3,
                           grupos=4,sinicio=1234,repe=5,
                           C=0.5,sigma=0.1)
# 
medias20_m3$modelo="SVMRBF_2"
# Aumenta mucho la varianza con respecto a los otros tuneados
medias21_m3<-cruzadaSVMbinRBF(data=data, vardep="TenYearCHD",listvars=modelo3,
                           grupos=4,sinicio=1234,repe=5,
                           C=0.05,sigma=0.01)
# 
medias21_m3$modelo="SVMRBF_3"
# 
# Comparamos logistica con SVM (Linear, polinomial y Gauss)
union1<-rbind(medias1_m3,medias13_m3,medias14_m3,medias15_m3,medias16_m3,medias17_m3,medias18_m3,medias19_m3,medias20_m3,medias21_m3)
# mejor: medias20_m3 
par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")


# Comparamos mejores valores de cada modelo para todos los algoritmos, redes y logistica
union1<-rbind(medias1_m3,medias3_m3,medias4_m3,medias7_m3,medias9_m3,medias12_m3,medias20_m3)
# Regresion gana. Gradient boosting cercano en tasa de fallos, red avnnet2 mejor auc, pero redavnnet3 y gbm muy cerca
par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,main="TASA FALLOS")
boxplot(data=union1,auc~modelo,main="AUC")
# 
# 
# 

##############################################################################
#  ******************   H2O   **************************
# ****************************************************************************
# validación cruzada h2o:
# ****************************************************************************


library(h2o)
h2o.init()

## MODELO 3 ##


modelo3h2o <- c("TenYearCHD","Edad","CigsPorDia","PresionSanguinea","Hombre")

# Reordeno las columnas
dataH2o<-data[,modelo3h2o]

dataH2o$TenYearCHD<-ifelse(data$TenYearCHD=="Yes",1,0)
dataH2o$TenYearCHD<-as.factor(dataH2o$TenYearCHD) ##make categorical

train.hex <- as.h2o(dataH2o, destination_frame = "train.hex")


logis<-h2o.glm(y="TenYearCHD",training_frame = train.hex,
               family="binomial",nfolds=5,seed=12345)

logis

summary(logis) ## Now the model metrics contain AUC for binary classification
plot(h2o.performance(logis))

Rep<-c("Rep1","Rep2","Rep3","Rep4","Rep5")
auc<-c(0.7280702,0.7082036,0.7196101,0.7093883,0.7223332)
modelo<-c("logis","logis","logis","logis","logis")
logisDF<-data.frame(Rep,auc,modelo)
logisDF$modelo="logis"

##------------------------------------------------------

red1<-h2o.deeplearning(y="TenYearCHD",training_frame = train.hex,
                       hidden = c(12),epochs =100,activation = "Tanh",nfolds=5,seed=12345)

red1

summary(red1) ## Now the model metrics contain AUC for binary classification
plot(h2o.performance(red1)) ## display ROC curve

Rep<-c("Rep1","Rep2","Rep3","Rep4","Rep5")
auc<-c(0.7117121,0.68855643,0.7147393,0.70556265,0.71912915)
modelo<-c("red1","red1","red1","red1","red1")
red1DF<-data.frame(Rep,auc,modelo)
red1DF$modelo="red1"


##------------------------------------------------------


red2<-h2o.deeplearning(y="TenYearCHD",training_frame = train.hex,
                       hidden = c(10),epochs =200,activation = "Tanh",nfolds=5,seed=12345)

red2

summary(red2)
plot(h2o.performance(red2)) ## display ROC curve

#auc  0.69557303  0.7588851 0.68087244 0.70413613
Rep<-c("Rep1","Rep2","Rep3","Rep4","Rep5")
auc<-c(0.72053283,0.69628155,0.6986103,0.6871643,0.7171503)
modelo<-c("red2","red2","red2","red2","red2")
red2DF<-data.frame(Rep,auc,modelo)
red2DF$modelo="red2"


##------------------------------------------------------


red3<-h2o.deeplearning(y="TenYearCHD",training_frame = train.hex,
                       hidden = c(12),epochs =200,activation = "Tanh",nfolds=5,seed=4567)

red3

summary(red3)
plot(h2o.performance(red3)) ## display ROC curve

#auc  0.69557303  0.7588851 0.68087244 0.70413613
Rep<-c("Rep1","Rep2","Rep3","Rep4","Rep5")
auc<-c(0.72709703,0.69493765,0.68942964,0.74599266,0.72046185)
modelo<-c("red3","red3","red3","red3","red3")
red3DF<-data.frame(Rep,auc,modelo)
red3DF$modelo="red3"


##------------------------------------------------------


unionH2o<-rbind(logisDF,red1DF,red2DF,red3DF)
par(cex.axis=0.5)
boxplot(data=unionH2o,auc~modelo,main="AUC ~ H2O_M3")


