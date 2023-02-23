#**************************************************************************************#
#                                    TALLER 2 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: GEIH DANE                                           #
#**************************************************************************************#

# Limpiar el espacio
rm(list = ls(all.names = TRUE))

# Directorio de trabajo (cambiar según computador)
setwd("C:/Users/nicol/Documents/GitHub/Repositorios/Taller-2-BDML")

# ------------------------------------------------------------------------------------ #
# Cargar librerias.
# ------------------------------------------------------------------------------------ #

list.of.packages = c("pacman", "readr","tidyverse", "dplyr", "arsenal", "fastDummies", 
                     "caret", "glmnet", "MLmetrics", "skimr", "plyr", "stargazer", "jtools", 
                     "Metrics", "writexl", "yardstick")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# ------------------------------------------------------------------------------------ #
# 1. Descripción del problema
# ------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------ #
# 2. Data
# ------------------------------------------------------------------------------------ #

###  2.1 Cargamos las bases 
#1. Train
train_personas_original <- read_csv("./data/train_personas.csv")
train_hogares_original <- read_csv("./data/train_hogares.csv")

#2. Test
test_personas_original <- read_csv("./data/test_personas.csv")
test_hogares_original <- read_csv("./data/test_hogares.csv")

### 2.2 ¿Qué variables faltan? 
#hogares train vs test: para conocer qué variables faltan en test
comparedf(train_hogares_original, test_hogares_original)
hogarescompar <- summary(comparedf(train_hogares_original, test_hogares_original))
print(hogarescompar)

#personas train vs test: para conocer qué variables faltan en test y si todas las observaciones de test están en el train set
comparedf(test_personas_original, train_personas_original)
personascompar <- summary(comparedf(test_personas_original, train_personas_original))  
print(personascompar)

### 2.3 CREANDO LAS BASES CON LAS QUE TRABAJAREMOS 

## Dejar las de hogares con las variables que queremos 
train_hogares <- subset(train_hogares_original, select = c(id, Clase, P5000, P5010, P5090, P5130, P5140, Nper, Npersug, Li, Lp, Depto, Ingtotug, Pobre, Npobres))
test_hogares <- subset(test_hogares_original, select = c(id, Clase, P5000, P5010, P5090, P5130, P5140, Nper, Npersug, Li, Lp, Depto))

## Dejar las de personas con las variables que queremos
train_personas <- subset (train_personas_original, select = c(id, Orden, Clase, Ingtot, P6210,P6430,P6240, P6585s1, P6585s3, P6920, P7505, P7510s3, P6100, Des, Oc))
test_personas <- subset(test_personas_original, select = c(id, Orden, Clase, P6210,P6430,P6240, P6585s1, P6585s3, P6920, P7505, P7510s3, P6100, Des, Oc))

# PARA TRAIN --------------------------------------------------------------------------------------
#1. Creando variables

#1.1 convertimos los valores de 2 en 0 para las variables binarias
train_personas$P6585s1 <- ifelse(train_personas$P6585s1 == 1, 1, 0) # ¿el mes pasado recibió a. Auxilio o subsidio de alimentación?
train_personas$P6585s3 <- ifelse(train_personas$P6585s3 == 1, 1, 0) # ¿el mes pasado recibió c. Subsidio familiar?
train_personas$P7510s3 <- ifelse(train_personas$P7510s3 == 1, 1, 0) # Durante los últimos 12 meses, ¿recibió c. ayudas en dinero de instituciones del país?
train_personas$P7505 <- ifelse(train_personas$P7505   == 1, 1, 0) # Durante los últimos doce meses, ¿recibió dinero de otros hogares, personas o instituciones no gubernamentales; dinero por intereses, dividendos, utilidades o por cesantias?
train_personas$P6920 <- ifelse(train_personas$P6920   == 1, 1, 0) # ¿está... Cotizando actualmente a un fondo de pensiones?
train_personas$Des <- ifelse(train_personas$Des     == 1, 1, 0) # Desocupado 1: sí
train_personas$Oc <- ifelse(train_personas$Oc      == 1, 1, 0) # Ocupado 1: sí

#1.2 con más de 2 categorías 
##P6100 ¿A cual de los siguientes regímenes de seguridad social en salud está afiliado:
train_personas$subsidiado   <- ifelse(train_personas$P6100 == 3, 1, 0)
train_personas$contributivo <- ifelse(train_personas$P6100 == 1, 1, 0)
train_personas$especial     <- ifelse(train_personas$P6100 == 2, 1, 0)
#se excluye no informa

##P6210 ¿Cuál es el nivel educativo más alto alcanzado por .... y el último año o grado aprobado en este nivel? 
train_personas$ningunoeduc      <- ifelse(train_personas$P6210 == 1, 1, 0)
train_personas$preescolar <- ifelse(train_personas$P6210 == 2, 1, 0)
train_personas$basicaprimaria         <- ifelse(train_personas$P6210 == 3, 1, 0)
train_personas$basicasecundaria <- ifelse(train_personas$P6210 == 4, 1, 0)
train_personas$media            <- ifelse(train_personas$P6210 == 5, 1, 0)
train_personas$superior         <- ifelse(train_personas$P6210 == 6, 1, 0)
#se excluye no informa

##P6240 ¿En que actividad ocupó...... la mayor parte del tiempo la semana pasada?
train_personas$mayoriatiempotrabajo               <- ifelse(train_personas$P6240 == 1, 1, 0)
train_personas$mayoriatiempobuscandotrabajo       <- ifelse(train_personas$P6240 == 2, 1, 0)
train_personas$mayoriatiempoestudiando            <- ifelse(train_personas$P6240 == 3, 1, 0)
train_personas$mayoriatiempooficiohogar           <- ifelse(train_personas$P6240 == 4, 1, 0)
train_personas$mayoriatiempoincapacitado          <- ifelse(train_personas$P6240 == 4, 1, 0)
#se excluye otra actividad

##P6430 En este trabajo es …. (posición ocupacional primera actividad)
train_personas$obreroemplempresa         <- ifelse(train_personas$P6430 == 1, 1, 0)
train_personas$obreroemplgobierno        <- ifelse(train_personas$P6430 == 2, 1, 0)
train_personas$empldomestico             <- ifelse(train_personas$P6430 == 3, 1, 0)
train_personas$trabajadorcuentapropia    <- ifelse(train_personas$P6430 == 4, 1, 0)
train_personas$patronempleador           <- ifelse(train_personas$P6430 == 5, 1, 0)
train_personas$trabajadorsinremunfamilia <- ifelse(train_personas$P6430 == 6, 1, 0)
train_personas$trabajadorsinremunempresa <- ifelse(train_personas$P6430 == 7, 1, 0)
#se excluye otro

#1.3 Vamos a agrupar las familias por sumas 
sumP6585s1<-train_personas %>% group_by(id) %>% dplyr::summarise(P6585s1h=sum(P6585s1,na.rm = TRUE))
sumP6585s3<-train_personas %>% group_by(id) %>% dplyr::summarise(P6585s3h=sum(P6585s3,na.rm = TRUE))
sumP7510s3<-train_personas %>% group_by(id) %>% dplyr::summarise(P7510s3h=sum(P7510s3,na.rm = TRUE))
sumP7505  <-train_personas %>% group_by(id) %>% dplyr::summarise(P7505h  =sum(P6585s1,na.rm = TRUE))
sumP6920  <-train_personas %>% group_by(id) %>% dplyr::summarise(P6920h  =sum(P6585s1,na.rm = TRUE))
sumDes    <-train_personas %>% group_by(id) %>% dplyr::summarise(Desh    =sum(Des,na.rm = TRUE))
sumOc     <-train_personas %>% group_by(id) %>% dplyr::summarise(Och     =sum(Oc,na.rm = TRUE))
orden     <-train_personas %>% group_by(id) %>% dplyr::summarise(Orden   =sum(Orden,na.rm = TRUE))
sumsubsidiado                   <-train_personas %>% group_by(id) %>% dplyr::summarise(subsidiado=sum(subsidiado,na.rm = TRUE))
sumcontributivo                 <-train_personas %>% group_by(id) %>% dplyr::summarise(contributivo=sum(contributivo,na.rm = TRUE))
sumespecial                     <-train_personas %>% group_by(id) %>% dplyr::summarise(especial=sum(especial,na.rm = TRUE))
sumningunoeduc                  <-train_personas %>% group_by(id) %>% dplyr::summarise(ningunoeduc =sum(ningunoeduc ,na.rm = TRUE))
sumpreescolar                   <-train_personas %>% group_by(id) %>% dplyr::summarise(preescolar=sum(preescolar,na.rm = TRUE))
sumbasicaprimaria               <-train_personas %>% group_by(id) %>% dplyr::summarise(basicaprimaria=sum(basicaprimaria,na.rm = TRUE))
sumbasicasecundaria             <-train_personas %>% group_by(id) %>% dplyr::summarise(basicasecundaria=sum(basicasecundaria,na.rm = TRUE))
summedia                        <-train_personas %>% group_by(id) %>% dplyr::summarise(media=sum(media,na.rm = TRUE))
sumsuperior                     <-train_personas %>% group_by(id) %>% dplyr::summarise(superior=sum(superior,na.rm = TRUE))
summayoriatiempotrabajo         <-train_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempotrabajo=sum(mayoriatiempotrabajo,na.rm = TRUE))
summayoriatiempobuscandotrabajo <-train_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempobuscandotrabajo=sum(mayoriatiempobuscandotrabajo,na.rm = TRUE))
summayoriatiempoestudiando      <-train_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempoestudiando=sum(mayoriatiempoestudiando,na.rm = TRUE))
summayoriatiempooficiohogar     <-train_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempooficiohogar=sum(mayoriatiempooficiohogar,na.rm = TRUE))
summayoriatiempoincapacitado    <-train_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempoincapacitado=sum(mayoriatiempoincapacitado,na.rm = TRUE))
sumobreroemplempresa            <-train_personas %>% group_by(id) %>% dplyr::summarise(obreroemplempresa=sum(obreroemplempresa,na.rm = TRUE))
sumobreroemplgobierno           <-train_personas %>% group_by(id) %>% dplyr::summarise(obreroemplgobierno=sum(obreroemplgobierno,na.rm = TRUE))
sumempldomestico                <-train_personas %>% group_by(id) %>% dplyr::summarise(empldomestico=sum(empldomestico,na.rm = TRUE))
sumtrabajadorcuentapropia       <-train_personas %>% group_by(id) %>% dplyr::summarise(trabajadorcuentapropia=sum(trabajadorcuentapropia,na.rm = TRUE))
sumpatronempleador              <-train_personas %>% group_by(id) %>% dplyr::summarise(patronempleador=sum(patronempleador,na.rm = TRUE))
sumtrabajadorsinremunfamilia    <-train_personas %>% group_by(id) %>% dplyr::summarise(trabajadorsinremunfamilia=sum(trabajadorsinremunfamilia,na.rm = TRUE))
sumtrabajadorsinremunempresa    <-train_personas %>% group_by(id) %>% dplyr::summarise(trabajadorsinremunempresa=sum(trabajadorsinremunempresa,na.rm = TRUE))

#Y las pegamos a la base de hogares 
train_hogares<-join_all(list(train_hogares, orden, sumP6585s3, sumP6585s1, sumP7510s3, sumP7505, sumP6920, sumDes, sumOc, sumsubsidiado, sumcontributivo, sumespecial, sumningunoeduc, sumpreescolar, sumbasicaprimaria, sumbasicasecundaria, summedia, sumsuperior, summayoriatiempoincapacitado, summayoriatiempooficiohogar, summayoriatiempoestudiando, summayoriatiempobuscandotrabajo, summayoriatiempotrabajo, sumobreroemplgobierno, sumobreroemplempresa, sumempldomestico, sumtrabajadorcuentapropia, sumpatronempleador, sumtrabajadorsinremunempresa, sumtrabajadorsinremunfamilia), by= 'id', type= 'left')

#dividimos por personas en la casa para tener la proporción 
train_hogares$prop_P6585s1h <- train_hogares$P6585s1h / train_hogares$Orden
train_hogares$prop_P6585s3h <- train_hogares$P6585s3h / train_hogares$Orden
train_hogares$prop_P7510s3h <- train_hogares$P7510s3h / train_hogares$Orden
train_hogares$prop_P7505h   <- train_hogares$P7505h   / train_hogares$Orden
train_hogares$prop_P6920h   <- train_hogares$P6920h   / train_hogares$Orden
train_hogares$prop_Desh     <- train_hogares$Desh     / train_hogares$Orden
train_hogares$prop_Och      <- train_hogares$Och      / train_hogares$Orden
train_hogares$prop_subsidiado                   <- train_hogares$subsidiado / train_hogares$Orden
train_hogares$prop_contributivo                 <- train_hogares$contributivo / train_hogares$Orden
train_hogares$prop_especial                     <- train_hogares$especial / train_hogares$Orden
train_hogares$prop_ningunoeduc                  <- train_hogares$ningunoeduc   / train_hogares$Orden
train_hogares$prop_preescolar                   <- train_hogares$preescolar   / train_hogares$Orden
train_hogares$prop_basicaprimaria               <- train_hogares$basicaprimaria     / train_hogares$Orden
train_hogares$prop_basicasecundaria             <- train_hogares$basicasecundaria      / train_hogares$Orden
train_hogares$prop_media                        <- train_hogares$media   / train_hogares$Orden
train_hogares$prop_superior                     <- train_hogares$superior   / train_hogares$Orden
train_hogares$prop_mayoriatiempotrabajo         <- train_hogares$mayoriatiempotrabajo   / train_hogares$Orden
train_hogares$prop_mayoriatiempobuscandotrabajo <- train_hogares$mayoriatiempobuscandotrabajo / train_hogares$Orden
train_hogares$prop_mayoriatiempoestudiando      <- train_hogares$mayoriatiempoestudiando / train_hogares$Orden
train_hogares$prop_mayoriatiempooficiohogar     <- train_hogares$mayoriatiempooficiohogar / train_hogares$Orden
train_hogares$prop_mayoriatiempoincapacitado    <- train_hogares$mayoriatiempoincapacitado / train_hogares$Orden
train_hogares$prop_obreroemplempresa            <- train_hogares$obreroemplempresa / train_hogares$Orden
train_hogares$prop_obreroemplgobierno           <- train_hogares$obreroemplgobierno / train_hogares$Orden
train_hogares$prop_empldomestico                <- train_hogares$empldomestico / train_hogares$Orden
train_hogares$prop_trabajadorcuentapropia       <- train_hogares$trabajadorcuentapropia / train_hogares$Orden
train_hogares$prop_patronempleador              <- train_hogares$patronempleador / train_hogares$Orden
train_hogares$prop_trabajadorsinremunfamilia    <- train_hogares$trabajadorsinremunfamilia / train_hogares$Orden
train_hogares$prop_trabajadorsinremunempresa    <- train_hogares$trabajadorsinremunempresa / train_hogares$Orden

colnames(train_hogares)

#1.3 Modificaciones adicionales 
train_hogares$P5010[train_hogares$P5010>=10] <- 10
train_hogares$Pobre <- as.factor(train_hogares$Pobre) # Pobre como factor
train_hogares$Depto <- as.factor(train_hogares$Depto) # Departamento como factor
train_hogares$P5000 <- as.factor(train_hogares$P5000) # Número de cuartos como factor
train_hogares$P5010 <- as.factor(train_hogares$P5010) # Número de dormitorios como factor
train_hogares$P5090 <- as.factor(train_hogares$P5090) # Tipo de tenencia como factor

#PARA TEST------------------------------------------------------------------------------

#1. Creando variables

#1.1 convertimos los valores de 2 en 0 para las variables binarias
test_personas$P6585s1 <- ifelse(test_personas$P6585s1 == 1, 1, 0)
test_personas$P6585s3 <- ifelse(test_personas$P6585s3 == 1, 1, 0)
test_personas$P7510s3 <- ifelse(test_personas$P7510s3 == 1, 1, 0)
test_personas$P7505 <- ifelse(test_personas$P7505   == 1, 1, 0)
test_personas$P6920 <- ifelse(test_personas$P6920   == 1, 1, 0)
test_personas$Des <- ifelse(test_personas$Des     == 1, 1, 0)
test_personas$Oc <- ifelse(test_personas$Oc      == 1, 1, 0)

#1.2 con más de 2 categorías 
##P6100 ¿A cual de los siguientes regímenes de seguridad social en salud está afiliado:
test_personas$subsidiado    <- ifelse(test_personas$P6100 == 3, 1, 0)
test_personas$contributivo  <- ifelse(test_personas$P6100 == 1, 1, 0)
test_personas$especial      <- ifelse(test_personas$P6100 == 2, 1, 0)
#se excluye no informa

##P6210 ¿Cuál es el nivel educativo más alto alcanzado por .... y el último año o grado aprobado en este nivel? 
test_personas$ningunoeduc       <- ifelse(test_personas$P6210 == 1, 1, 0)
test_personas$preescolar  <- ifelse(test_personas$P6210 == 2, 1, 0)
test_personas$basicaprimaria          <- ifelse(test_personas$P6210 == 3, 1, 0)
test_personas$basicasecundaria  <- ifelse(test_personas$P6210 == 4, 1, 0)
test_personas$media             <- ifelse(test_personas$P6210 == 5, 1, 0)
test_personas$superior          <- ifelse(test_personas$P6210 == 6, 1, 0)
#se excluye no informa

##P6240 ¿En que actividad ocupó...... la mayor parte del tiempo la semana pasada?
test_personas$mayoriatiempotrabajo                 <- ifelse(test_personas$P6240 == 1, 1, 0)
test_personas$mayoriatiempobuscandotrabajo         <- ifelse(test_personas$P6240 == 2, 1, 0)
test_personas$mayoriatiempoestudiando              <- ifelse(test_personas$P6240 == 3, 1, 0)
test_personas$mayoriatiempooficiohogar             <- ifelse(test_personas$P6240 == 4, 1, 0)
test_personas$mayoriatiempoincapacitado            <- ifelse(test_personas$P6240 == 5, 1, 0)
#se excluye otra actividad

##P6430 En este trabajo es …. (posición ocupacional primera actividad)
test_personas$obreroemplempresa          <- ifelse(test_personas$P6430 == 1, 1, 0)
test_personas$obreroemplgobierno         <- ifelse(test_personas$P6430 == 2, 1, 0)
test_personas$empldomestico              <- ifelse(test_personas$P6430 == 3, 1, 0)
test_personas$trabajadorcuentapropia     <- ifelse(test_personas$P6430 == 4, 1, 0)
test_personas$patronempleador            <- ifelse(test_personas$P6430 == 5, 1, 0)
test_personas$trabajadorsinremunfamilia  <- ifelse(test_personas$P6430 == 6, 1, 0)
test_personas$trabajadorsinremunempresa  <- ifelse(test_personas$P6430 == 7, 1, 0)
#se excluye otro

#1.3 Vamos a agrupar las familias por sumas de acuerdo con las variables individuales
sumP6585s1<-test_personas %>% group_by(id) %>% dplyr::summarise(P6585s1h=sum(P6585s1,na.rm = TRUE))
sumP6585s3<-test_personas %>% group_by(id) %>% dplyr::summarise(P6585s3h=sum(P6585s3,na.rm = TRUE))
sumP7510s3<-test_personas %>% group_by(id) %>% dplyr::summarise(P7510s3h=sum(P7510s3,na.rm = TRUE))
sumP7505  <-test_personas %>% group_by(id) %>% dplyr::summarise(P7505h  =sum(P6585s1,na.rm = TRUE))
sumP6920  <-test_personas %>% group_by(id) %>% dplyr::summarise(P6920h  =sum(P6585s1,na.rm = TRUE))
sumDes    <-test_personas %>% group_by(id) %>% dplyr::summarise(Desh    =sum(Des,na.rm = TRUE))
sumOc     <-test_personas %>% group_by(id) %>% dplyr::summarise(Och     =sum(Oc,na.rm = TRUE))
orden     <-test_personas %>% group_by(id) %>% dplyr::summarise(Orden   =sum(Orden,na.rm = TRUE))
sumsubsidiado                   <-test_personas %>% group_by(id) %>% dplyr::summarise(subsidiado=sum(subsidiado,na.rm = TRUE))
sumcontributivo                 <-test_personas %>% group_by(id) %>% dplyr::summarise(contributivo=sum(contributivo,na.rm = TRUE))
sumespecial                     <-test_personas %>% group_by(id) %>% dplyr::summarise(especial=sum(especial,na.rm = TRUE))
sumningunoeduc                  <-test_personas %>% group_by(id) %>% dplyr::summarise(ningunoeduc =sum(ningunoeduc ,na.rm = TRUE))
sumpreescolar                   <-test_personas %>% group_by(id) %>% dplyr::summarise(preescolar=sum(preescolar,na.rm = TRUE))
sumbasicaprimaria               <-test_personas %>% group_by(id) %>% dplyr::summarise(basicaprimaria=sum(basicaprimaria,na.rm = TRUE))
sumbasicasecundaria             <-test_personas %>% group_by(id) %>% dplyr::summarise(basicasecundaria=sum(basicasecundaria,na.rm = TRUE))
summedia                        <-test_personas %>% group_by(id) %>% dplyr::summarise(media=sum(media,na.rm = TRUE))
sumsuperior                     <-test_personas %>% group_by(id) %>% dplyr::summarise(superior=sum(superior,na.rm = TRUE))
summayoriatiempotrabajo         <-test_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempotrabajo=sum(mayoriatiempotrabajo,na.rm = TRUE))
summayoriatiempobuscandotrabajo <-test_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempobuscandotrabajo=sum(mayoriatiempobuscandotrabajo,na.rm = TRUE))
summayoriatiempoestudiando      <-test_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempoestudiando=sum(mayoriatiempoestudiando,na.rm = TRUE))
summayoriatiempooficiohogar     <-test_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempooficiohogar=sum(mayoriatiempooficiohogar,na.rm = TRUE))
summayoriatiempoincapacitado    <-test_personas %>% group_by(id) %>% dplyr::summarise(mayoriatiempoincapacitado=sum(mayoriatiempoincapacitado,na.rm = TRUE))
sumobreroemplempresa            <-test_personas %>% group_by(id) %>% dplyr::summarise(obreroemplempresa=sum(obreroemplempresa,na.rm = TRUE))
sumobreroemplgobierno           <-test_personas %>% group_by(id) %>% dplyr::summarise(obreroemplgobierno=sum(obreroemplgobierno,na.rm = TRUE))
sumempldomestico                <-test_personas %>% group_by(id) %>% dplyr::summarise(empldomestico=sum(empldomestico,na.rm = TRUE))
sumtrabajadorcuentapropia       <-test_personas %>% group_by(id) %>% dplyr::summarise(trabajadorcuentapropia=sum(trabajadorcuentapropia,na.rm = TRUE))
sumpatronempleador              <-test_personas %>% group_by(id) %>% dplyr::summarise(patronempleador=sum(patronempleador,na.rm = TRUE))
sumtrabajadorsinremunfamilia    <-test_personas %>% group_by(id) %>% dplyr::summarise(trabajadorsinremunfamilia=sum(trabajadorsinremunfamilia,na.rm = TRUE))
sumtrabajadorsinremunempresa    <-test_personas %>% group_by(id) %>% dplyr::summarise(trabajadorsinremunempresa=sum(trabajadorsinremunempresa,na.rm = TRUE))

#Y las pegamos a la base de hogares 
test_hogares<-join_all(list(test_hogares, orden, sumP6585s3, sumP6585s1, sumP7510s3, sumP7505, sumP6920, sumDes, sumOc, sumsubsidiado, sumcontributivo, sumespecial, sumningunoeduc, sumpreescolar, sumbasicaprimaria, sumbasicasecundaria, summedia, sumsuperior, summayoriatiempoincapacitado, summayoriatiempooficiohogar, summayoriatiempoestudiando, summayoriatiempobuscandotrabajo, summayoriatiempotrabajo, sumobreroemplgobierno, sumobreroemplempresa, sumempldomestico, sumtrabajadorcuentapropia, sumpatronempleador, sumtrabajadorsinremunempresa, sumtrabajadorsinremunfamilia), by= 'id', type= 'left')
colnames(test_hogares)

#Dividimos por personas en la casa para tener la proporción 
test_hogares$prop_P6585s1h <- test_hogares$P6585s1h / test_hogares$Orden
test_hogares$prop_P6585s3h <- test_hogares$P6585s3h / test_hogares$Orden
test_hogares$prop_P7510s3h <- test_hogares$P7510s3h / test_hogares$Orden
test_hogares$prop_P7505h   <- test_hogares$P7505h   / test_hogares$Orden
test_hogares$prop_P6920h   <- test_hogares$P6920h   / test_hogares$Orden
test_hogares$prop_Desh     <- test_hogares$Desh     / test_hogares$Orden
test_hogares$prop_Och      <- test_hogares$Och      / test_hogares$Orden
test_hogares$prop_subsidiado                   <- test_hogares$subsidiado / test_hogares$Orden
test_hogares$prop_contributivo                 <- test_hogares$contributivo / test_hogares$Orden
test_hogares$prop_especial                     <- test_hogares$especial / test_hogares$Orden
test_hogares$prop_ningunoeduc                  <- test_hogares$ningunoeduc   / test_hogares$Orden
test_hogares$prop_preescolar                   <- test_hogares$preescolar   / test_hogares$Orden
test_hogares$prop_basicaprimaria               <- test_hogares$basicaprimaria     / test_hogares$Orden
test_hogares$prop_basicasecundaria             <- test_hogares$basicasecundaria      / test_hogares$Orden
test_hogares$prop_media                        <- test_hogares$media   / test_hogares$Orden
test_hogares$prop_superior                     <- test_hogares$superior   / test_hogares$Orden
test_hogares$prop_mayoriatiempotrabajo         <- test_hogares$mayoriatiempotrabajo   / test_hogares$Orden
test_hogares$prop_mayoriatiempobuscandotrabajo <- test_hogares$mayoriatiempobuscandotrabajo / test_hogares$Orden
test_hogares$prop_mayoriatiempoestudiando      <- test_hogares$mayoriatiempoestudiando / test_hogares$Orden
test_hogares$prop_mayoriatiempooficiohogar     <- test_hogares$mayoriatiempooficiohogar / test_hogares$Orden
test_hogares$prop_mayoriatiempoincapacitado    <- test_hogares$mayoriatiempoincapacitado / test_hogares$Orden
test_hogares$prop_obreroemplempresa            <- test_hogares$obreroemplempresa / test_hogares$Orden
test_hogares$prop_obreroemplgobierno           <- test_hogares$obreroemplgobierno / test_hogares$Orden
test_hogares$prop_empldomestico                <- test_hogares$empldomestico / test_hogares$Orden
test_hogares$prop_trabajadorcuentapropia       <- test_hogares$trabajadorcuentapropia / test_hogares$Orden
test_hogares$prop_patronempleador              <- test_hogares$patronempleador / test_hogares$Orden
test_hogares$prop_trabajadorsinremunfamilia    <- test_hogares$trabajadorsinremunfamilia / test_hogares$Orden
test_hogares$prop_trabajadorsinremunempresa    <- test_hogares$trabajadorsinremunempresa / test_hogares$Orden

#1.3 Modificaciones adicionales 
test_hogares$P5010[test_hogares$P5010>=10] <- 10
test_hogares$Depto <- as.factor(test_hogares$Depto) # Departamento como factor
test_hogares$P5000 <- as.factor(test_hogares$P5000) # Número de cuartos como factor
test_hogares$P5010 <- as.factor(test_hogares$P5010) # Número de dormitorios como factor
test_hogares$P5090 <- as.factor(test_hogares$P5090) # Tipo de tenencia como factor

# ------------------------------------------------------------------------------------ #
# 3. Modelos de regresión
# ------------------------------------------------------------------------------------ #

# En el caso de los modelos de regresión, nuestro interés para a ser la predicción del 
# ingreso de los hogares, para luego evaluar si ese ingreso se encuentra por encima o 
# por debajo de la línea de pobreza. 
# La medida de desempeño es el RMSE.

# Variable explicada Y 
#      Ingtotug 
#      Pobre (discreta) - Pobre=1 No pobre=0

# Lista de posibles variables explicativas: 
#      P5000 (discreta) - Incluyendo sala-comedor ¿de cuántos cuartos en total dispone este hogar?
#      P5010 (discreta) - ¿En cuántos de esos cuartos duermen las personas de este hogar?
#      P5090 (discreta) - La vivienda ocupada por este hogar es
#      P5130 (continua) - Si tuviera que pagar arriendo por esta vivienda, ¿cuánto estima que tendría que pagar mensualmente?
#      P5140 (continua) - ¿Cuánto pagan mensualmente por arriendo?
#      Nper (continua) - Personas en el hogar
#      Npersug (continua) - Número de personas en la unidad de gasto
#      Depto (discreta) - Departamento
#      prop_P6585s1h - Proporción de personas que recibieron un auxilio de alimentación en el hogar
#      prop_P6585s3h - Proporción de personas que recibieron un auxilio familiar en el hogar
#      prop_P7510s3h - Proporción de personas que recibieron dinero de instituciones en el hogar.
#      prop_P7505h - Proporción de personas que recibieron dinero de otros hogares en el hogar.
#      prop_P6920h - Proporción de personas en el hogar que estarían cotizando a pensión.
#      prop_Desh - Proporción de personas en el hogar desempleadas.
#      prop_Och - Proporción de personas en el hogar empleadas.  
#      Npobres (continua) - Número de pobres
#      prop_subsidiado - Proporción de personas en el hogar cotizantes al regimen subsidiado.                
#      prop_contributivo - Proporción de personas en el hogar cotizantes al regimen contributivo.    
#      prop_especial - Proporción de personas en el hogar cotizantes al regimen especial.              
#      prop_ningunoeduc - Proporción de personas en el hogar con ningún nivel de educación.                 
#      prop_preescolar - Proporción de personas en el hogar con nivel de educación preescolar.                 
#      prop_basicaprimaria - Proporción de personas en el hogar con nivel de educación básica primaria.            
#      prop_basicasecundaria - Proporción de personas en el hogar con nivel de educación básica secundaria.           
#      prop_media - Proporción de personas en el hogar con nivel de educación media.                 
#      prop_superior - Proporción de personas en el hogar con nivel de educación superior.            
#      prop_mayoriatiempotrabajo - Proporción de personas en el hogar que pasaron la mayor parte del tiempo trabajando.       
#      prop_mayoriatiempobuscandotrabajo - Proporción de personas en el hogar que pasaron la mayor parte del tiempo buscando trabajo.  
#      prop_mayoriatiempoestudiando - Proporción de personas en el hogar que pasaron la mayor parte del tiempo estudiando.
#      prop_mayoriatiempooficiohogar - Proporción de personas en el hogar que pasaron la mayor parte del tiempo haciendo oficios del hogar.   
#      prop_mayoriatiempoincapacitado - Proporción de personas en el hogar que pasaron la mayor parte del tiempo incapacitado.
#      prop_obreroemplempresa - Proporción de personas en el hogar que son empleados de una empresa privada.     
#      prop_obreroemplgobierno - Proporción de personas en el hogar que son empleados del gobierno.              
#      prop_empldomestico - Proporción de personas en el hogar que son empleados domésticos.               
#      prop_trabajadorcuentapropia - Proporción de personas en el hogar que son cuenta propia. 
#      prop_patronempleador - Proporción de personas en el hogar que son patrón.         
#      prop_trabajadorsinremunfamilia - Proporción de personas en el hogar que son trabajadores de la familia sin remuneración.
#      prop_trabajadorsinremunempresa - Proporción de personas en el hogar que son trabajadores de una empresa sin remuneración.

set.seed(11111)

# Partición de la base de datos train, con el objetivo de evaluar el performance de los modelos.
inTrain <- createDataPartition(
  y = train_hogares$Ingtotug,## Nuestra  
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

hog_training <- train_hogares[ inTrain,] # Set de datos de entrenamiento
hog_testing  <- train_hogares[-inTrain,] # Set de datos de evaluación
nrow(hog_training) # El conjunto de entrenamiento contiene el 70% de la base original (115473/164960*100)

# Cross-validation
ctrl <- trainControl(
  method = "cv", 
  number = 7) # número de folds

# Fórmula de los modelos
fmla <- formula(Ingtotug~P5000+P5010+P5090+Nper+Npersug+Depto+prop_P6585s1h+prop_P6585s3h+prop_P7510s3h+
                  prop_P7505h+prop_P6920h+prop_Desh+prop_subsidiado+prop_contributivo+prop_especial+
                  prop_ningunoeduc+prop_preescolar+prop_basicaprimaria+prop_basicasecundaria+prop_media+prop_superior+
                  prop_mayoriatiempotrabajo+prop_mayoriatiempobuscandotrabajo+prop_mayoriatiempoestudiando+
                  prop_mayoriatiempooficiohogar+prop_mayoriatiempoincapacitado+prop_obreroemplempresa+
                  prop_obreroemplgobierno+prop_empldomestico+prop_trabajadorcuentapropia+prop_patronempleador+
                  prop_trabajadorsinremunfamilia+prop_trabajadorsinremunempresa)

### 3.1 Modelo benchmark: regresión lineal ------------------------------------------------------------

## Ajuste: Estimación de modelo de regresión lineal con datos de entrenamiento
ModeloRL <- train(fmla, 
                  data = hog_training, method = 'lm',
                  trControl= ctrl,
                  preProcess = c("center", "scale"))

summary(ModeloRL) # Resumen del modelo
ggplot(varImp(ModeloRL)) # Gráfico de importancia de las variables

## Predicción 1: Predicciones con hog_testing
pred_test1_ModeloRL <- predict(ModeloRL, newdata = hog_testing) # Predicción
eva_ModeloRL <- data.frame(obs=hog_testing$Ingtotug, pred=pred_test1_ModeloRL) # Data frame con observados y predicciones
metrics_ModeloRL <- metrics(eva_ModeloRL, obs, pred); metrics_ModeloRL # Cálculo del medidas de precisión

# Identificación de pobres y no pobres en hog_testing
pob1_ModeloRL <- ifelse(pred_test1_ModeloRL<hog_testing$Lp, 1, 0)

# Evaluación de clasificación
eva_ModeloRL <- data.frame(obs=as.factor(hog_testing$Pobre), pred=as.factor(pob1_ModeloRL)) # Data frame con observados y predicciones
confmatrix_ModeloRL <- confusionMatrix(data = as.factor(pob1_ModeloRL), reference = as.factor(hog_testing$Pobre)) ; confmatrix_ModeloRL # Matriz de confusión

## Predicción 2: Predicciones con test_hogares
pred_test2_ModeloRL <- predict(ModeloRL, newdata = test_hogares)

# Identificación de pobres y no pobres en test_hogares
pob2_ModeloRL <- ifelse(pred_test2_ModeloRL<test_hogares$Lp, 1, 0)

# Exportar para prueba en Kaggle
Kaggle_ModeloRL <- data.frame(id=test_hogares$id, pobre=pob2_ModeloRL)
write.csv(Kaggle_ModeloRL,"./stores/Kaggle_ModeloRL.csv", row.names = FALSE)
# Accuracy: 0.75349

### 3.2 Lasso -----------------------------------------------------------------------------------------



### 3.3 Elastic net -----------------------------------------------------------------------------------
ModeloEN<-caret::train(fmla,
          data=hog_training,
          method = 'glmnet', 
          trControl = ctrl,
          tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #Lasso
                                 lambda = seq(0.001,0.1,by = 0.001)),
          preProcess = c("center", "scale")
) 

summary(ModeloEN) # Resumen del modelo
ggplot(varImp(ModeloEN)) # Gráfico de importancia de las variables
ModeloEN$bestTune

## Gráfico de los coeficientes 
#Put coefficients in a data frame, except the intercept
coefs_EN<-data.frame(t(as.matrix(coef(ModeloEN)))) %>% select(-X.Intercept.)
#add the lambda grid to to data frame
coefs_EN<- coefs_EN %>% mutate(lambda=grid)              

#ggplot friendly format
coefs_EN<- coefs_EN %>% pivot_longer(cols=!lambda,
                                           names_to="variables",
                                           values_to="coefficients")



ggplot(data=coefs_EN, aes(x = lambda, y = coefficients, color = variables)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes Elastic Net", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")

coef_EN<-coef(EN$finalModel, EN$bestTune$lambda)
coef_EN

## Predicción 1: Predicciones con hog_testing
pred_test1_ModeloEN <- predict(ModeloEN, newdata = hog_testing) # Predicción
eva_ModeloEN <- data.frame(obs=hog_testing$Ingtotug, pred=pred_test1_ModeloEN) # Data frame con observados y predicciones
metrics_ModeloEN <- metrics(eva_ModeloEN, obs, pred); metrics_ModeloEN # Cálculo del medidas de precisión

# Identificación de pobres y no pobres en hog_testing
pob1_ModeloEN <- ifelse(pred_test1_ModeloEN<hog_testing$Lp, 1, 0)

# Evaluación de clasificación
eva_ModeloEN <- data.frame(obs=as.factor(hog_testing$Pobre), pred=as.factor(pob1_ModeloEN)) # Data frame con observados y predicciones
confmatrix_ModeloEN <- confusionMatrix(data = as.factor(pob1_ModeloEN), reference = as.factor(hog_testing$Pobre)) ; confmatrix_ModeloRL # Matriz de confusión

## Predicción 2: Predicciones con test_hogares
pred_test2_ModeloEN <- predict(ModeloEN, newdata = test_hogares)

# Identificación de pobres y no pobres en test_hogares
pob2_ModeloEN <- ifelse(pred_test2_ModeloEN<test_hogares$Lp, 1, 0)

# Exportar para prueba en Kaggle
Kaggle_ModeloEN <- data.frame(id=test_hogares$id, pobre=pob2_ModeloEN)
write.csv(Kaggle_ModeloEN,"./stores/Kaggle_ModeloEN.csv", row.names = FALSE)
# Accuracy: 0.75462

### 3.4 Random Forest ---------------------------------------------------------------------------------



### 3.5 AdaBoosting -----------------------------------------------------------------------------------





