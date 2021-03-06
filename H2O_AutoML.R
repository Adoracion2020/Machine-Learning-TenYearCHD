

##############################################################################
#  ******************   H2O   **************************
# ****************************************************************************
# validaci�n cruzada h2o:
# ****************************************************************************

library(h2o)
h2o.init()


## MODELO 2 ##


modelo2h2o <- c("TenYearCHD","Edad","CigsPorDia","PresionSanguinea")

# Reordeno las columnas
dataH2o<-data[,modelo2h2o]

dataH2o$TenYearCHD<-ifelse(data$TenYearCHD=="Yes",1,0)
dataH2o$TenYearCHD<-as.factor(dataH2o$TenYearCHD) ##make categorical

train.hex <- as.h2o(dataH2o, destination_frame = "train.hex")


# Se hace uso de la funci�n GLM (modelo lineal generalizado) para la construcci�n del modelo de regresi�n log�stica en H2O.
# Esta funci�n surge como resultado de generalizar y unificar diferentes modelos lineales tradicionales 
# (regresi�n lineal por m�nimos cuadrados, regresi�n log�stica.), bajo un mismo marco.

logis<-h2o.glm(y="TenYearCHD",training_frame = train.hex,
               family="binomial",link="logit",nfolds=5,seed=12345)

logis

summary(logis) ## Mostrar resultados del modelo sin tener que volver a ejecutar
plot(h2o.performance(logis)) # display de la curva ROC

Rep<-c("Rep1","Rep2","Rep3","Rep4","Rep5")
auc<-c(0.7280702,0.7082036,0.7196101,0.7093883,0.7223332)
modelo<-c("logis","logis","logis","logis","logis")
logisDF<-data.frame(Rep,auc,modelo)
logisDF$modelo="logis"

##------------------------------------------------------

red1<-h2o.deeplearning(y="TenYearCHD",training_frame = train.hex,
                       hidden = c(12),epochs =100,activation = "Tanh",nfolds=5,seed=12345)

red1

summary(red1) ## Mostrar resultados del modelo sin tener que volver a ejecutar
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
# regresi�n logistica y redes:

unionH2o<-rbind(logisDF,red1DF,red2DF,red3DF)
par(cex.axis=0.5)
boxplot(data=unionH2o,auc~modelo,main="AUC ~ H2O_M2")


##-----------------------------------------------------
# BAGGING

red4<-h2o.randomForest(y="TenYearCHD",training_frame = train.hex,
                       mtries=3,ntrees=50,nfolds=5,seed=4567)

red4

##------------------------------------------------------
# RANDOM FOREST

red5<-h2o.randomForest(y="TenYearCHD",training_frame = train.hex,
                       mtries=1,ntrees=50,nfolds=5,seed=4567)

red5

##------------------------------------------------------
# GRADIENT BOOSTING

red6<-h2o.gbm(y="TenYearCHD",training_frame = train.hex,
                       nfolds=5,learn_rate=0.001,ntrees=5000,seed=4567)

red6

##------------------------------------------------------
# SUPPORT VECTOR MACHINES

# No se puede pasar validacion cruzada
red7<-h2o.psvm(y="TenYearCHD",training_frame = train.hex,
               hyper_param=0.5,seed=4567,gamma = 0.01,
               rank_ratio = 0.1,disable_training_metrics = FALSE)

perf <- h2o.performance(red7)
print(perf)

###############################################################################
###########################--- AUTO ML ---#####################################
###############################################################################



## autoML es una funci�n de la librer�a H2O que automatiza el proceso de construccion de un 
# gran n�mero de modelos, con el objetivo de encontrar 'el mejor' sin un conocimiento avanzado,
# ya que realiza una gran cantidad de tareas relacionadas con el modelado que normalmente requerir�an
# muchas l�neas de c�digo, y liberan al usuario para enfocarse en otros aspectos de las tareas de
# canalizaci�n de ciencia de datos, como el preprocesamiento de datos,


# AutoML se puede usar para automatizar el flujo de trabajo de aprendizaje autom�tico, que incluye el ajuste
# de muchos modelos dentro de un l�mite de tiempo especificado, ambos par�metros especificados por el usuario por el usuario.
# Adem�s, se generar�n adicionalmente 2 modelos de ensamblado, uno basado en todos los modelos previamente entrenados
# y otro en el mejor modelo de cada familia, para intentar lograr la mayor capacidad predictiva.


## Ejecutar AutoML 

# El argumento `max_models` especifica el numero de modelos individuales que se generar�n (no incluye los 2 modelos de ensamblado
# que se crean automaticamente al final). El argumento `max_runtime_secs` es el segundo argumento requerido que indica el tiempo 
# m�ximo de ejecuci�n antes de entrenar los modelos ensamblados (se encuentra por defecto en 1h).
# AutoML utiliza validaci�n cruzada y pone por defecto el valor a 5 (nfolds = 5)

aml <- h2o.automl(y = "TenYearCHD",
                  training_frame = train.hex,
                  max_models = 20,
                  seed = 1234)



## Leaderboard


# A continuaci�n, veremos la tabla de clasificaci�n de AutoML. Como no especificamos un `leaderboard_frame` en la funci�n,
# la tabla de clasificaci�n de AutoML utiliza m�tricas de validaci�n cruzada para clasificar los modelos.


# Una m�trica de rendimiento predeterminada para cada tarea de aprendizaje autom�tico (clasificaci�n binaria, clasificaci�n multiclase, regresi�n)
# se especifica internamente y la tabla de clasificaci�n se ordenar� por esa m�trica. En el caso de la clasificaci�n binaria,
# la m�trica de clasificaci�n predeterminada es �rea bajo la curva ROC (AUC).

# El modelo l�der se almacena en `aml@leader` y la tabla de clasificaci�n se almacena en` aml@leaderboard`.
lb <- aml@leaderboard


# Para ver el resultado de los mejores modelos usamos `print(lb)`.
# Para ver la tabla de clasificaci�n completa, especifique el argumento `n` de la funci�n` print.H2OFrame () `como el n�mero total de filas:
print(lb, n = nrow(lb))


## Exploraci�n del ensamblado

# Para entender c�mo funciona el ensamblado, echemos un vistazo dentro del modelo "All models";
# este es un conjunto de todos los modelos individuales en la ejecuci�n de AutoML.

# Obtenemos los ids de todos los modelos
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
# extraemos el modelo ensamblado 'All models'
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
# obtenemos el 'metalearner' del modelo ensamblado
metalearner <- h2o.getModel(se@model$metalearner$name)

# Examinamos la importancia de variable del algoritmo metalearner (combinador) en el ensamblado.
# Esto nos muestra cu�nto contribuye cada modelo base al conjunto. Los conjuntos ensamblados de AutoML usan el
# algoritmo metalearner como valor predeterminado (GLM con pesos no negativos), por lo que la importancia de variables
# del metalearner es en realidad las magnitudes de coeficientes estandarizadas de la GLM (modelo lineal generalizado).

h2o.varimp(metalearner)
h2o.varimp_plot(metalearner)


