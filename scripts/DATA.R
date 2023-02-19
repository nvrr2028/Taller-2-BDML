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

train_personas <- read_csv("data/train_personas.zip")
train_hogares <- read_csv("data/train_hogares.zip")

  #2. Test
test_personas <- read_csv("data/test_personas.zip")
test_hogares <- read_csv("data/test_hogares.zip")

### ¿Qué variables faltan? 
  #hogares train vs test: para conocer qué variables faltan en test
comparedf(train_hogares, test_hogares)
hogarescompar <- summary(comparedf(train_hogares, test_hogares))
print(hogarescompar)

  #personas train vs test: para conocer qué variables faltan en test y si todas las observaciones de test están en el train set
comparedf(test_personas, train_personas)
personascompar <- summary(comparedf(test_personas, train_personas))  
print(personascompar)

dim(test_personas) #sí están todas las observaciones de test en train
dim(test_hogares) #sí están todas las observaciones de test en train

test_personas$test = 1 #binaria para reconocer qué observaciones pertenecen al test set
test_hogares$test = 1
train_personas$test = 0 #binaria para reconocer qué observaciones pertenecen al test set
train_hogares$test = 0

# Obtenemos las variables que faltan para los sets de test tanto para personas como hogares
test_personas1 <- left_join(test_personas, train_personas) # obtenemos todas las variables para las personas del test set
dim(test_personas1)
dim(test_personas)

test_hogares1 <- left_join(test_hogares, train_hogares) # obtenemos todas las variables para los hogares del test set
dim(test_hogares1)
dim(test_hogares)

#Creamos las nuevas col pero estas no tienen info
library(skimr)
skim(test_personas1)
skim(test_personas)


#### UNIR BASES SEGÚN ID 
colnames(train_personas)
colnames(train_hogares)

## TRAIN

#1. Agrupar la variable que queremos pasar de una base a otra 
sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE))  # It returns one row for each combination of grouping variables, borramos NA
summary(sum_ingresos)

#2. Agregar la variable que agrupamos previamente
train_hogares<-left_join(train_hogares,sum_ingresos)
colnames(train_hogares)

skim(train_hogares)  #ingtothogar e ingtotug son similares más no iguales 

##TEST
colnames(test_personas)
colnames(test_hogares)

#mo tenemos variables de ingreso... 

