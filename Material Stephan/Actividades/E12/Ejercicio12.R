
# Actividad: practicando regresión logística (múltiple)

library(ggplot2)
library(lattice)
library(caret)
library(dplyr)
library(leaps)
library(pROC)   
library(tidyr)

library(ggpubr)

# Indicar directorio
dir <- "/Users/Downloads"

basename <- "Phishing.csv"
file <- file.path(dir, basename)
datos <- read.csv(file = file, sep = ",")

datos <- datos %>% mutate_all(as.factor)

# Usando como semilla el año y mes de nacimiento de un/a miembro del equipo
set.seed(200003)

# Se selecciona una muestra de 500 casos de los datos
tamaño.muestra <- 500

# Fijamos el tamaño de la poblacion en estudio, contando la cantidad de columnas
tamaño.poblacion <- nrow(datos)

# Fijamos la muestra
muestra <- datos[sample(1:tamaño.poblacion,tamaño.muestra),]

# Para convertir la muestra a largo
muestra.long <- muestra %>% pivot_longer(!Result,
                                         names_to = "variable",
                                         values_to = "valor")
#1
g1 <- ggbarplot(muestra.long,
                x = "Result",
                y = "variable")
print(g1)

g2 <- g1 + facet_wrap(~ variable)
print(g2)

g3 <- g2 + facet_wrap(~ valor)
print(g3)




#2
g1 <- ggbarplot(muestra.long,
                x = "valor",
                y = "variable")
print(g1)

g2 <- g1 + facet_wrap(~ variable)
print(g2)


#3
g1 <- ggbarplot(muestra.long,
                x = "valor",
                y = "Result")
print(g1)

g2 <- g2 + facet_wrap(~ variable)
print(g2)




#________________________________________

set.seed ( 1313 )

# Cargar los datos.
datos <- mtcars
datos $ am <- factor ( datos $ am )

# Separar conjuntos de entrenamiento y prueba .
n <- nrow ( datos )
n_entrenamiento <- floor ( 0.8 * n )
muestra <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]

# Ajustar modelo .
modelo <- glm ( am ~ wt ,
                family = binomial ( link = " logit " ) ,
                data = entrenamiento )

print ( summary ( modelo ) )

# Evaluar el modelo con el conjunto de entrenamiento.
cat ( "Evaluación del modelo a partir del conjunto de entrenamiento :\ n")
probs_e <- predict ( modelo , entrenamiento , type = " response " )

umbral <- 0.5
preds_e <- sapply ( probs_e , function ( p ) ifelse ( p >= umbral , "1" , "0" ) )
preds_e <- factor ( preds_e , levels = levels ( datos [[ "am" ]]) )

ROC_e <- roc ( entrenamiento [[ "am" ]] , probs_e )
plot ( ROC_e )

matriz_e <- confusionMatrix ( preds_e , entrenamiento [[ "am" ]])
print ( matriz_e )

# Evaluar el modelo con el conjunto de prueba .
cat ( " Evaluación del modelo a partir del conjunto de prueba :\ n " )
probs_p <- predict ( modelo , prueba , type = " response " )

preds_p <- sapply ( probs_p , function ( p ) ifelse ( p >= umbral , "1" , "0" ) )
preds_p <- factor ( preds_p , levels = levels ( datos [[ "am" ]]) )

ROC_p <- roc ( entrenamiento [[ "am" ]] , probs_e )
plot ( ROC_p )

matriz_p <- confusionMatrix ( preds_p , prueba [[ "am" ]])
print ( matriz_p )




#____________________________________________________________


# Validación cruzada


set.seed ( 1313 )

# Cargar los datos .
datos <- mtcars
datos $ am <- factor ( datos $ am )

# Ajustar modelo usando validación cruzada de 5 pliegues .
modelo <- train ( am ~ wt , data = entrenamiento , method = " glm " ,
                  family = binomial ( link = " logit " ) ,
                  trControl = trainControl ( method = " cv " , number = 5 ,
                                             savePredictions = TRUE ) )

print ( summary ( modelo ) )

# Evaluar el modelo
cat ( " Evaluación del modelo basada en validación cruzada :\ n " )
matriz <- confusionMatrix ( modelo $ pred $ pred , modelo $ pred $ obs )
print ( matriz )

#______________________________________________________________



library ( car )

# Elegimos la variable "having_IP_Address" porque es dictómica.
having_IP_Address <- factor(muestra.long$having_IP_Address)
muestra.long$having_IP_Address <- NULL
muestra.long <- cbind (having_IP_Address, muestra.long)

# Separar conjuntos de entrenamiento y prueba.
n <- nrow ( muestra.long )
n_entrenamiento <- floor ( 0.8 * n )
sample1 <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ sample1 , ]
prueba <- muestra.long [ - sample1 , ]

# Ajustar modelo nulo .
nulo <- glm ( having_IP_Address ~ 1 ,
              family = binomial ( link = "logit" ) ,
              data = entrenamiento )


# Ajustar modelo completo.
cat ( "\n \n " )
completo <- glm ( having_IP_Address ~ . ,
                  family = binomial ( link = "logit" ) ,
                  data = entrenamiento )

# Ajustar modelo con regresión escalonada.
cat ( " Modelo con regresión escalonada \n" )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\ n " )
mejor <- step ( nulo ,
                scope = list ( lower = nulo , upper = completo ) ,
                direction = " both " ,
                trace = 0 )

print(summary(mejor))

# Verificación de multicolinealidad .
cat ( " Verificación de colinealidad \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\ n " )
cat ( "\ nVIF :\ n " )
vifs <- vif ( mejor )
print ( vifs )
cat ( "\ nPromedio VIF : ")
print ( mean ( vifs ) )

# Ajustar modelo con el peso como predictor .
cat ( " Modelo con el peso como predictor \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\ n " )
modelo_peso <- glm ( am ~ wt ,
                     family = binomial ( link = " logit " ) ,
                     data = entrenamiento )

print ( summary ( modelo_peso ) )

# Ajustar modelo con la potencia como predictor .
cat ( " Modelo con la potencia como predictor \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\ n " )

modelo_potencia <- glm ( am ~ hp ,
                         family = binomial ( link = " logit " ) ,
                         data = entrenamiento )

print ( summary ( modelo_potencia ) )

# Comparar los modelos con el peso y la potencia como predictores .
cat ( "\ n \ n " )
cat ( " Likelihood Ratio Test para los modelos \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\ n " )
print ( anova ( modelo_peso , modelo_potencia , test = " LRT " ) )

# A modo de ejercicio , comparar el modelo obtenido mediante
# regresi ón escalonada con el que solo tiene el peso como predictor .
cat ( "\ n \ n " )
cat ( " Likelihood Ratio Test para los modelos \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\ n " )
print ( anova ( modelo_peso , mejor , test = " LRT " ))

# Independencia de los residuos .
cat ( " Verificaci ó n de independencia de los residuos \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\ n " )
print ( durbinWatsonTest ( modelo_peso , max.lag = 5 ) )

# Detectar posibles valores atípicos .
cat ( " Identificaci ó n de posibles valores at í picos \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\ n " )
plot ( mejor )

# Obtener los residuos y las estadísticas .


output <- data.frame ( predicted.probabilities = fitted ( modelo_peso ) )
output [[ " standardized . residuals " ]] <- rstandard ( modelo_peso )
output [[ " studentized . residuals " ]] <- rstudent ( modelo_peso )
output [[ " cooks . distance " ]] <- cooks.distance ( modelo_peso )
output [[ " dfbeta " ]] <- dfbeta ( modelo_peso )
output [[ " dffit " ]] <- dffits ( modelo_peso )
output [[ " leverage " ]] <- hatvalues ( modelo_peso )

# Evaluar residuos estandarizados que escapen a la normalidad .
# 9 5 % de los residuos estandarizados deber í an estar entre
# -1.9 6 y 1.9 6, y 9 9 % entre -2.5 8 y 2.5 8.
sospechosos1 <- which ( abs ( output [[ " standardized.residuals " ]]) > 1.96)
sospechosos1 <- sort ( sospechosos1 )
cat ( " \n \ n " )
cat ( " Residuos estandarizados fuera del 9 5 % esperado \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\ n " )
print ( rownames ( entrenamiento [ sospechosos1 , ]) )

# Revisar casos con distancia de Cook mayor a uno .
sospechosos2 <- which ( output [[ " cooks.distance " ]] > 1 )
sospechosos2 <- sort ( sospechosos2 )
cat ( " \n \ n " )
cat ( " Residuales con una distancia de Cook alta \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n " )
print ( rownames ( entrenamiento [ sospechosos2 , ]) )

# Revisar casos cuyo apalancamiento sea m á s del doble
# o triple del apalancamiento promedio .
leverage.promedio <- ncol ( entrenamiento ) / nrow ( datos )
sospechosos3 <- which ( output [[ " leverage " ]] > leverage.promedio )
sospechosos3 <- sort ( sospechosos3 )
cat ( "\ n \ n " )
cat ( " Residuales con levarage fuera de rango ( > " )
cat ( round ( leverage.promedio , 3 ) , " ) " , " \n " , sep = " " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\ n " )
print ( rownames ( entrenamiento [ sospechosos3 , ]) )

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which ( apply ( output [[ " dfbeta " ]] >= 1 , 1 , any ) )
sospechosos4 <- sort ( sospechosos4 )
names ( sospechosos4 ) <- NULL
cat ( "\ n \ n " )
cat ( " Residuales con DFBeta sobre 1 \ n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - -\ n " )
print ( rownames ( entrenamiento [ sospechosos4 , ]) )

# Detalle de las observaciones posiblemente atípicas .
sospechosos <- c ( sospechosos1 , sospechosos2 , sospechosos3 , sospechosos4)
sospechosos <- sort ( unique ( sospechosos ) )
cat ( "\ n \ n " )
cat ( " Casos sospechosos \ n" )
cat ( " - - - - - - - - - - -- - - - - -\ n " )
print ( entrenamiento [ sospechosos , ])
cat ( "\ n \ n " )
print ( output [ sospechosos , ])