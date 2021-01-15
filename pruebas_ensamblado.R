
# PRUEBAS DE ENSAMBLADO


# **************************************
# IMPORTANTE: AQUÍ HAY QUE DECIDIR ANTES LOS PARÁMETROS A UTILIZAR
# EN CADA ALGORITMO, NO VALE GRID
# Importante, la dependiente en letras Yes, No
# Preparación de data, variables y CV. 
# Esto se cambia para cada data.
# Necesario haber cambiado la var dep a Yes,No.
# **************************************

# LEER LAS CRUZADAS DE ENSAMBLADO, SON LIGERAMENTE DIFERENTES
# A LAS UTILIZADAS ANTERIORMENTE AUNQUE SE LLAMAN IGUAL

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

# SET MODELO 2
modelo2 <- c("Edad","CigsPorDia","PresionSanguinea")


vardep<-"TenYearCHD"
listvars<-modelo2
grupos<-4
sinicio<-1234
repe<-5


# APLICACIÓN CRUZADAS PARA ENSAMBLAR (cogemos los mejores modelos individuales)

medias1<-cruzadalogisticaEns(data=data,
                          vardep=vardep,listvars=listvars,
                          grupos=grupos,sinicio=sinicio,repe=repe)

medias1bis<-as.data.frame(medias1[1])
medias1bis$modelo<-"Logistica"
predi1<-as.data.frame(medias1[2])
predi1$logi<-predi1$Yes

medias2<-cruzadaavnnetbinEns(data=data,
                          vardep=vardep,listvars=listvars,
                          grupos=grupos,sinicio=sinicio,repe=repe,
                          size=c(12),decay=c(0.01),repeticiones=5,itera=200)

medias2bis<-as.data.frame(medias2[1])
medias2bis$modelo<-"avnnet"
predi2<-as.data.frame(medias2[2])
predi2$avnnet<-predi2$Yes


medias3<-cruzadarfbinEns(data=data, vardep=vardep,listvars=listvars,
                         grupos=grupos,sinicio=sinicio,repe=repe,nodesize=12,
                         mtry=2,ntree=600,replace=TRUE)


medias3bis<-as.data.frame(medias3[1])
medias3bis$modelo<-"rf"
predi3<-as.data.frame(medias3[2])
predi3$rf<-predi3$Yes

medias4<-cruzadagbmbinEns(data=data,
                       vardep=vardep,listvars=listvars,
                       grupos=grupos,sinicio=sinicio,repe=repe,
                       n.minobsinnode=12,shrinkage=0.001,n.trees=5000,interaction.depth=2)

medias4bis<-as.data.frame(medias4[1])
medias4bis$modelo<-"gbm"
predi4<-as.data.frame(medias4[2])
predi4$gbm<-predi4$Yes

medias5<-cruzadarfbinEns(data=data, vardep=vardep,listvars=listvars,
                         grupos=grupos,sinicio=sinicio,repe=repe,nodesize=12,
                         mtry=3,ntree=600,replace=TRUE)


medias5bis<-as.data.frame(medias5[1])
medias5bis$modelo<-"bagging"
predi5<-as.data.frame(medias5[2])
predi5$bagging<-predi5$Yes


medias6<-cruzadaSVMbinEns(data=data,
                       vardep=vardep,listvars=listvars,
                       grupos=grupos,
                       sinicio=sinicio,repe=repe,C=0.05)

medias6bis<-as.data.frame(medias6[1])
medias6bis$modelo<-"svmLinear"
predi6<-as.data.frame(medias6[2])
predi6$svmLinear<-predi6$Yes


medias7<-cruzadaSVMbinPolyEns(data=data,
                           vardep=vardep,listvars=listvars,
                           grupos=grupos,sinicio=sinicio,repe=repe,
                           C=1,degree=2,scale=0.1)

medias7bis<-as.data.frame(medias7[1])
medias7bis$modelo<-"svmPoly"
predi7<-as.data.frame(medias7[2])
predi7$svmPoly<-predi7$Yes

medias8<-cruzadaSVMbinRBFEns(data=data,
                          vardep=vardep,listvars=listvars,
                          grupos=grupos,
                          sinicio=sinicio,repe=repe,
                          C=1,sigma=0.1)

medias8bis<-as.data.frame(medias8[1])
medias8bis$modelo<-"svmRadial"
predi8<-as.data.frame(medias8[2])
predi8$svmRadial<-predi8$Yes

union1<-rbind(medias1bis,medias2bis,
              medias3bis,medias4bis,medias5bis,medias6bis,
              medias7bis,medias8bis)

par(cex.axis=0.8)
boxplot(data=union1,tasa~modelo,col="green",main='TASA FALLOS')
boxplot(data=union1,auc~modelo,col="pink",main='AUC')

# CONSTRUCCIÓN DE TODOS LOS ENSAMBLADOS
# SE UTILIZARÁN LOS dataS SURGIDOS DE LAS FUNCIONES LLAMADOS predi1,...

unipredi<-cbind(predi1,predi2,predi3,predi4,predi5,predi6,predi7,predi8)

# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]

# Construccion de ensamblados
unipredi$predi9<-(unipredi$logi+unipredi$avnnet)/2
unipredi$predi10<-(unipredi$logi+unipredi$rf)/2
unipredi$predi11<-(unipredi$logi+unipredi$gbm)/2
unipredi$predi12<-(unipredi$logi+unipredi$bagging)/2
unipredi$predi13<-(unipredi$logi+unipredi$svmLinear)/2
unipredi$predi14<-(unipredi$logi+unipredi$svmPoly)/2
unipredi$predi15<-(unipredi$logi+unipredi$svmRadial)/2
unipredi$predi16<-(unipredi$avnnet+unipredi$rf)/2
unipredi$predi17<-(unipredi$avnnet+unipredi$gbm)/2
unipredi$predi18<-(unipredi$avnnet+unipredi$bagging)/2
unipredi$predi19<-(unipredi$avnnet+unipredi$svmLinear)/2
unipredi$predi20<-(unipredi$avnnet+unipredi$svmPoly)/2
unipredi$predi21<-(unipredi$avnnet+unipredi$svmRadial)/2
unipredi$predi22<-(unipredi$rf+unipredi$gbm)/2
unipredi$predi23<-(unipredi$rf+unipredi$bagging)/2
unipredi$predi24<-(unipredi$rf+unipredi$svmLinear)/2
unipredi$predi25<-(unipredi$rf+unipredi$svmPoly)/2
unipredi$predi26<-(unipredi$rf+unipredi$svmRadial)/2
unipredi$predi27<-(unipredi$gbm+unipredi$bagging)/2
unipredi$predi28<-(unipredi$gbm+unipredi$svmLinear)/2
unipredi$predi29<-(unipredi$gbm+unipredi$svmPoly)/2
unipredi$predi30<-(unipredi$gbm+unipredi$svmRadial)/2

unipredi$predi31<-(unipredi$logi+unipredi$avnnet+unipredi$rf)/3

#la union de los 3 mejores modelos:
unipredi$predi32<-(unipredi$logi+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi33<-(unipredi$logi+unipredi$avnnet+unipredi$bagging)/3
unipredi$predi34<-(unipredi$logi+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi35<-(unipredi$logi+unipredi$avnnet+unipredi$svmPoly)/3
unipredi$predi36<-(unipredi$logi+unipredi$avnnet+unipredi$svmRadial)/3
unipredi$predi37<-(unipredi$logi+unipredi$rf+unipredi$gbm)/3
unipredi$predi38<-(unipredi$logi+unipredi$rf+unipredi$bagging)/3
unipredi$predi39<-(unipredi$logi+unipredi$rf+unipredi$svmLinear)/3
unipredi$predi40<-(unipredi$logi+unipredi$rf+unipredi$svmPoly)/3
unipredi$predi41<-(unipredi$logi+unipredi$rf+unipredi$svmRadial)/3
unipredi$predi42<-(unipredi$logi+unipredi$gbm+unipredi$bagging)/3
unipredi$predi43<-(unipredi$logi+unipredi$gbm+unipredi$bagging)/3
unipredi$predi44<-(unipredi$logi+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi45<-(unipredi$logi+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi46<-(unipredi$logi+unipredi$gbm+unipredi$svmRadial)/3
unipredi$predi47<-(unipredi$logi+unipredi$bagging+unipredi$svmLinear)/3
unipredi$predi48<-(unipredi$logi+unipredi$bagging+unipredi$svmPoly)/3
unipredi$predi49<-(unipredi$logi+unipredi$bagging+unipredi$svmRadial)/3

unipredi$predi50<-(unipredi$rf+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi51<-(unipredi$rf+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi52<-(unipredi$rf+unipredi$gbm+unipredi$svmRadial)/3

unipredi$predi53<-(unipredi$rf+unipredi$bagging+unipredi$svmLinear)/3
unipredi$predi54<-(unipredi$rf+unipredi$bagging+unipredi$svmPoly)/3
unipredi$predi55<-(unipredi$rf+unipredi$bagging+unipredi$svmRadial)/3

unipredi$predi56<-(unipredi$rf+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi57<-(unipredi$rf+unipredi$avnnet+unipredi$bagging)/3
unipredi$predi58<-(unipredi$rf+unipredi$avnnet+unipredi$svmLinear)/3
unipredi$predi59<-(unipredi$rf+unipredi$avnnet+unipredi$svmPoly)/3
unipredi$predi60<-(unipredi$rf+unipredi$avnnet+unipredi$svmRadial)/3

unipredi$predi61<-(unipredi$avnnet+unipredi$gbm+unipredi$svmLinear)/3
unipredi$predi62<-(unipredi$avnnet+unipredi$gbm+unipredi$svmPoly)/3
unipredi$predi63<-(unipredi$avnnet+unipredi$gbm+unipredi$svmRadial)/3

unipredi$predi64<-(unipredi$logi+unipredi$rf+unipredi$gbm+unipredi$avnnet)/4
unipredi$predi65<-(unipredi$logi+unipredi$rf+unipredi$bagging+unipredi$avnnet)/4
unipredi$predi66<-(unipredi$logi+unipredi$rf+unipredi$bagging+unipredi$avnnet)/4

unipredi$predi67<-(unipredi$logi+unipredi$rf+unipredi$bagging+unipredi$avnnet+unipredi$svmLinear)/5
unipredi$predi68<-(unipredi$logi+unipredi$rf+unipredi$bagging+unipredi$avnnet+unipredi$svmPoly)/5
unipredi$predi69<-(unipredi$logi+unipredi$rf+unipredi$bagging+unipredi$avnnet+unipredi$svmRadial)/5


# Listado de modelos a considerar, cambiar al gusto

dput(names(unipredi))

listado<-c("logi", "avnnet", 
           "rf","gbm",  "bagging", "svmLinear",  "svmPoly", 
           "svmRadial","predi9", "predi10", "predi11", "predi12", 
           "predi13", "predi14", "predi15", "predi16", "predi17", "predi18", 
           "predi19", "predi20", "predi21", "predi22", "predi23", "predi24", 
           "predi25", "predi26", "predi27", "predi28", "predi29", "predi30", 
           "predi31", "predi32", "predi33", "predi34", "predi35", "predi36", 
           "predi37", "predi38", "predi39", "predi40", "predi41", "predi42", 
           "predi43", "predi44", "predi45", "predi46", "predi47", "predi48", 
           "predi49", "predi50", "predi51", "predi52", "predi53", "predi54", 
           "predi55", "predi56", "predi57", "predi58", "predi59", "predi60", 
           "predi61", "predi62", "predi63", "predi64", "predi65", "predi66", 
           "predi67", "predi68", "predi69")

# Cambio a Yes, No, todas las predicciones

# Defino funcion tasafallos

tasafallos<-function(x,y) {
  confu<-confusionMatrix(x,y)
  tasa<-confu[[3]][1]
  return(tasa)
}

auc<-function(x,y) {
  curvaroc<-roc(response=x,predictor=y)
  auc<-curvaroc$auc
  return(auc)
}

# Se obtiene el numero de repeticiones CV y se calculan las medias por repe en
# el data frame medias0

repeticiones<-nlevels(factor(unipredi$Rep))
unipredi$Rep<-as.factor(unipredi$Rep)
unipredi$Rep<-as.numeric(unipredi$Rep)


medias0<-data.frame(c())
for (prediccion in listado)
{
  unipredi$proba<-unipredi[,prediccion]
  unipredi[,prediccion]<-ifelse(unipredi[,prediccion]>0.5,"Yes","No")
  for (repe in 1:repeticiones)
  {
    paso <- unipredi[(unipredi$Rep==repe),]
    pre<-factor(paso[,prediccion])
    archi<-paso[,c("proba","obs")]
    archi<-archi[order(archi$proba),]
    obs<-paso[,c("obs")]
    tasa=1-tasafallos(pre,obs)
    t<-as.data.frame(tasa)
    t$modelo<-prediccion
    auc<-auc(archi$obs,archi$proba)
    t$auc<-auc
    medias0<-rbind(medias0,t)
  }
}

# Finalmente boxplot (DESORDENADO, MEJOR SACAR SOLO LOS DE ABAJO)

par(cex.axis=0.5,las=2)
boxplot(data=medias0,tasa~modelo,col="green",main="TASA FALLOS")

# Para AUC se utiliza la variable auc del data medias0

boxplot(data=medias0,auc~modelo,col="pink",main="AUC")

# PRESENTACION TABLA MEDIAS

tablamedias<-medias0 %>%
  group_by(modelo) %>%
  summarize(tasa=mean(tasa))     

tablamedias<-tablamedias[order(tablamedias$tasa),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN TASA
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,tasa, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,tasa~modelo,col="green", main='TASA FALLOS')

# ************************************
# PARA AUC
# ************************************

# PRESENTACION TABLA MEDIAS

tablamedias2<-medias0 %>%
  group_by(modelo) %>%
  summarize(auc=mean(auc))     

tablamedias2<-tablamedias2[order(-tablamedias2$auc),]


# ORDENACIÓN DEL FACTOR MODELO POR LAS MEDIAS EN AUC
# PARA EL GRAFICO

medias0$modelo <- with(medias0,
                       reorder(modelo,auc, mean))
par(cex.axis=0.7,las=2)
boxplot(data=medias0,auc~modelo,col="pink", main='AUC')



# Se pueden escoger listas pero el factor hay que pasarlo a character
# para que no salgan en el boxplot todos los niveles del factor

listadobis<-c("logi", "avnnet", 
              "rf","gbm",  "bagging", "svmLinear",  "svmPoly", 
              "svmRadial","predi32", "predi9", "predi36", "predi11") 

medias0$modelo<-as.character(medias0$modelo)

mediasverT<-medias0[medias0$modelo %in% listadobis,]
mediasver<-medias0[medias0$modelo %in% listadobis,]

mediasverT$modelo <- with(mediasver,
                         reorder(modelo,tasa, median))
mediasver$modelo <- with(mediasver,
                         reorder(modelo,auc, median))

par(cex.axis=0.9,las=2)

boxplot(data=mediasverT,tasa~modelo,col="green",main='TASA FALLOS')
boxplot(data=mediasver,auc~modelo,col="pink",main='AUC')



# GRÁFICOS DE APOYO PARA OBSERVAR COMPORTAMIENTO DE LOS MODELOS

unipredi<-cbind(predi1,predi2,predi3,predi4,predi5,predi6,predi7,predi8)
# Esto es para eliminar columnas duplicadas
unipredi<- unipredi[, !duplicated(colnames(unipredi))]
# Añadir ensamblados
unipredi$predi32<-(unipredi$logi+unipredi$avnnet+unipredi$gbm)/3
unipredi$predi11<-(unipredi$logi+unipredi$gbm)/2
# unipredi$predi45<-(unipredi$logi+unipredi$gbm+unipredi$svmPoly)/3
# unipredi$predi46<-(unipredi$logi+unipredi$gbm+unipredi$svmRadial)/3

# Me quedo con la primera repetición de validación cruzada para los análisis
unigraf<-unipredi[unipredi$Rep=="Rep1",]
# Correlaciones entre predicciones de cada algoritmo individual
solos<-c("logi", "avnnet",
         "rf","gbm",  "bagging", "svmLinear",  "svmPoly",
         "svmRadial")
mat<-unigraf[,solos]
matrizcorr<-cor(mat)
matrizcorr

# install.packages("corrplot")
library(corrplot)
corrplot(matrizcorr, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45,is.corr=FALSE) # cl.lim=c(0.7,1),

# #library(ggplot2)
# 
# #qplot(svmRadial,logi,data=unigraf,colour=obs)+
#   geom_hline(yintercept=0.5, color="black", size=1)+
#   geom_vline(xintercept=0.5, color="black", size=1)
# 
# qplot(predi32,logi,data=unigraf,colour=obs)+
#   geom_hline(yintercept=0.5, color="black", size=1)+
#   geom_vline(xintercept=0.5, color="black", size=1)
# 
# qplot(predi11,gbm,data=unigraf,colour=obs)+
#   geom_hline(yintercept=0.5, color="black", size=1)+
#   geom_vline(xintercept=0.5, color="black", size=1)



