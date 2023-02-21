#**************************************************************************************#
#                                    TALLER 1 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: GEIH DANE                                           #
#**************************************************************************************#

# Limpiar el espacio
rm(list = ls(all.names = TRUE))

###  cargamos las bases 
  #1. Train
library(pacman)
p_load("readr","tidyverse", "dplyr", "arsenal")

train_personas_original <- read_csv("stores/train_personas.zip")
train_hogares_original <- read_csv("stores/train_hogares.zip")

  #2. Test
test_personas_original <- read_csv("stores/test_personas.zip")
test_hogares_original <- read_csv("stores/test_hogares.zip")

### ¿Qué variables faltan? 
  #hogares train vs test: para conocer qué variables faltan en test
comparedf(train_hogares_original, test_hogares_original)
hogarescompar <- summary(comparedf(train_hogares_original, test_hogares_original))
print(hogarescompar)

  #personas train vs test: para conocer qué variables faltan en test y si todas las observaciones de test están en el train set
comparedf(test_personas_original, train_personas_original)
personascompar <- summary(comparedf(test_personas_original, train_personas_original))  
print(personascompar)

### CREANDO LAS BASES CON LAS QUE TRABAJAREMOS 
library(skimr)

## Dejar las de hogares con las variables que queremos 
train_hogares <- subset(train_hogares_original, select = c(id, Clase, P5000, P5010, P5090, P5130, P5140, Nper, Npersug, Li, Lp, Depto, Ingtotug, Pobre, Npobres))
test_hogares <- subset(test_hogares_original, select = c(id, Clase, P5000, P5010, P5090, P5130, P5140, Nper, Npersug, Li, Lp, Depto))

## Dejar las de personas con las variables que queremos
train_personas <- subset (train_personas_original, select = c(id, Orden, Clase, Ingtot, P6210,P6430,P6240, P6585s1, P6585s3, P6920, P7505, P7510s3, P6100, Des, Oc))
test_personas <- subset(test_personas_original, select = c(id, Orden, Clase, P6210,P6430,P6240, P6585s1, P6585s3, P6920, P7505, P7510s3, P6100, Des, Oc))

#PARA TRAIN --------------------------------------------------------------------------------------
#1. Creando variables

#1.1 turn 2 en 0
ifelse(train_personas$P6585s1 == 1, 1, 0)
ifelse(train_personas$P6585s3 == 1, 1, 0)
ifelse(train_personas$P7510s3 == 1, 1, 0)
ifelse(train_personas$P7505   == 1, 1, 0)
ifelse(train_personas$P6920   == 1, 1, 0)
ifelse(train_personas$Des     == 1, 1, 0)
ifelse(train_personas$Oc      == 1, 1, 0)
#1.2 con más de 2 categorías 





      #vamos a agrupar las familias por sumas 
sumP6585s1<-train_personas %>% group_by(id) %>% reframe(P6585s1h=sum(P6585s1,na.rm = TRUE))
sumP6585s3<-train_personas %>% group_by(id) %>% reframe(P6585s3h=sum(P6585s3,na.rm = TRUE))
sumP7510s3<-train_personas %>% group_by(id) %>% reframe(P7510s3h=sum(P7510s3,na.rm = TRUE))
sumP7505  <-train_personas %>% group_by(id) %>% reframe(P7505h  =sum(P6585s1,na.rm = TRUE))
sumP6920  <-train_personas %>% group_by(id) %>% reframe(P6920h  =sum(P6585s1,na.rm = TRUE))
sumDes    <-train_personas %>% group_by(id) %>% reframe(Desh    =sum(Des,na.rm = TRUE))
sumOc     <-train_personas %>% group_by(id) %>% reframe(Och     =sum(Oc,na.rm = TRUE))
orden     <-train_personas %>% group_by(id) %>% reframe(Orden   =sum(Orden,na.rm = TRUE))

#y las pegamos a la base de hogares 
library(plyr)
train_hogares<-join_all(list(train_personas, orden, sumP6585s3, sumP6585s1, sumP7510s3, sumP7505, sumP6920, sumDes, sumOc), by= 'id', type= 'left')

#dividimos por personas en la casa para tener la proporción 
train_hogares$prop_P6585s1h <- train_hogares$P6585s1h / train_hogares$Orden
train_hogares$prop_P6585s3h <- train_hogares$P6585s3h / train_hogares$Orden
train_hogares$prop_P7510s3h <- train_hogares$P7510s3h / train_hogares$Orden
train_hogares$prop_P7505h   <- train_hogares$P7505h   / train_hogares$Orden
train_hogares$prop_P6920h   <- train_hogares$P6920h   / train_hogares$Orden
train_hogares$prop_Desh     <- train_hogares$Desh     / train_hogares$Orden
train_hogares$prop_Och      <- train_hogares$Och      / train_hogares$Orden

colnames(train_hogares)

#PARA TEST------------------------------------------------------------------------------
#1. Creando variables

#1.1 turn 2 en 0
ifelse(test_personas$P6585s1 == 1, 1, 0)
ifelse(test_personas$P6585s3 == 1, 1, 0)
ifelse(test_personas$P7510s3 == 1, 1, 0)
ifelse(test_personas$P7505   == 1, 1, 0)
ifelse(test_personas$P6920   == 1, 1, 0)
ifelse(test_personas$Des     == 1, 1, 0)
ifelse(test_personas$Oc      == 1, 1, 0)
#1.2 con más de 2 categorías 





        #vamos a agrupar las familias por sumas 
sumP6585s1<-test_personas %>% group_by(id) %>% reframe(P6585s1h=sum(P6585s1,na.rm = TRUE))
sumP6585s3<-test_personas %>% group_by(id) %>% reframe(P6585s3h=sum(P6585s3,na.rm = TRUE))
sumP7510s3<-test_personas %>% group_by(id) %>% reframe(P7510s3h=sum(P7510s3,na.rm = TRUE))
sumP7505  <-test_personas %>% group_by(id) %>% reframe(P7505h  =sum(P6585s1,na.rm = TRUE))
sumP6920  <-test_personas %>% group_by(id) %>% reframe(P6920h  =sum(P6585s1,na.rm = TRUE))
sumDes    <-test_personas %>% group_by(id) %>% reframe(Desh    =sum(Des,na.rm = TRUE))
sumOc     <-test_personas %>% group_by(id) %>% reframe(Och     =sum(Oc,na.rm = TRUE))

        #y las pegamos a la base de hogares 
test_hogares<-join_all(list(test_hogares,sumP6585s3, sumP6585s1, sumP7510s3, sumP7505, sumP6920, sumDes, sumOc), by= 'id', type= 'left')
colnames(test_hogares)

#dividimos por personas en la casa para tener la proporción 
