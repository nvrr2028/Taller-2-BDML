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

train_personas_original <- read_csv("data/train_personas.zip")
train_hogares_original <- read_csv("data/train_hogares.zip")

  #2. Test
test_personas_original <- read_csv("data/test_personas.zip")
test_hogares_original <- read_csv("data/test_hogares.zip")

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
test_personas <- subset(test_personas_original, select = c(id, Orden, Clase, P6210,P6430,P6240, P6585s1, P6585s3, P6920, P7505, P7510s3, P6100, Pet, Des, Oc))
colnames(train_personas)
#1. Creando variables

#1.1 turn 2 en 0
ifelse(train_personas$P6585s1 == 1, 1, 0)
ifelse(train_personas$P6585s3 == 1, 1, 0)
ifelse(train_personas$P7510s3 == 1, 1, 0)
ifelse(train_personas$P7505   == 1, 1, 0)
ifelse(train_personas$P6920   == 1, 1, 0)

      #vamos a agrupar las familias por sumas 
sumP6585s1<-train_personas %>% group_by(id) %>% summarize(P6585s1h=sum(P6585s1,na.rm = TRUE))
sumP6585s3<-train_personas %>% group_by(id) %>% summarize(P6585s3h=sum(P6585s3,na.rm = TRUE))
sumP7510s3<-train_personas %>% group_by(id) %>% summarize(P7510s3h=sum(P7510s3,na.rm = TRUE))
sumP7505  <-train_personas %>% group_by(id) %>% summarize(P7505h  =sum(P6585s1,na.rm = TRUE))
sumP6920  <-train_personas %>% group_by(id) %>% summarize(P6920h  =sum(P6585s1,na.rm = TRUE))

      #dividimos por personas en la casa para tener la proporción 



#1.2 con más de 2 categorías 


