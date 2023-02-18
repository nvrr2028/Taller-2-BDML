#DATA 

###  cargamos las bases 
  #1. Train
library(pacman)


p_load("readr","tidyverse", "dplyr", "arsenal")

train_personas <- read_csv("stores/train_personas.zip")
train_hogares <- read_csv("stores/train_hogares.zip")

  #2. Test
test_personas <- read_csv("stores/test_personas.zip")
test_hogares <- read_csv("stores/test_hogares.zip")

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



