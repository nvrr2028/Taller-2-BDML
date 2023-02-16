#DATA 

###  cargamos las bases 
  #1. Train
library(pacman)
p_load("readr","tidyverse", "dplyr", "arsenal")

train_personas <- read_csv("data/train_personas.zip")
train_hogares <- read_csv("data/train_hogares.zip")

  #2. Test
test_personas <- read_csv("data/test_personas.zip")
test_hogares <- read_csv("data/test_hogares.zip")

### qué variables faltan? 
  #personas vs hogares
comparedf(train_hogares, train_personas)                         #Cuántas faltan
summary(comparedf(train_hogares, train_personas))                #Cuáles faltan
  #hogares train vs test
comparedf(train_hogares, test_hogares)
summary(comparedf(train_hogares, test_hogares))
  #personas train vs test
comparedf(test_personas, train_personas)
summary(comparedf(test_personas, train_personas))  

      
