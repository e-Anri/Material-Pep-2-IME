
# Actividad: practicando regresión logística (múltiple)

library(caret)
library(car)
library(dplyr)
library(corrplot)
library(pROC) 

# Indicar directorio
#dir <- "/Users/.../Downloads"
dir <- "/Users/Downloads"

basename <- "Phishing.csv"
file <- file.path(dir, basename)
datos <- read.csv(file = file)

# Usando como semilla el año y mes de nacimiento de un/a miembro del equipo
set.seed(200003)

# Se selecciona una muestra de 500 casos de los datos
tamaño.muestra <- 500

# Fijamos el tamaño de la poblacion en estudio, contando la cantidad de columnas
tamaño.poblacion <- nrow(datos)

# Se crea matriz de correlación para seleccionar predictores
correlacion <- round( cor(datos), 1)

corrplot(correlacion, 
         method ="number", 
         type = "lower",
         title = "Matriz de correlacion")

# Al observar la matriz de correlacion y enfocar nuestra atención en la variable Result,
# podemos notar que está mayormente relacionada con las variables SSLfinal_State y 
# URL_of_Anchor con un valor igual a 0.7, como también con las variables Prefix_Suffix,
# having_Sub_Domain, Request_URL y web_traffic con un valor igual a 0.3.

# De esta manera decidimos elegir como mejores predictores a las siguientes 5 variables: 
# - SSLfinal_State
# - URL_of_Anchor 
# - Prefix_Suffix
# - having_Sub_Domain
# - web_traffic

datos <- datos %>% mutate_all(as.factor)

Result <- datos$Result
datos$Result <- NULL
datos <- cbind (Result, datos)

# Separar conjuntos de entrenamiento y prueba .
n_entrenamiento <- floor ( 0.8 * tamaño.muestra )
muestra <- sample.int ( n = tamaño.muestra , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]

# Ajustar modelo nulo .
nulo <- glm ( Result ~ 1 ,
              family = binomial ( link = "logit" ) ,
              data = entrenamiento )


# Ajustar modelo completo .
cat ( "\n \n " )
completo <- glm ( Result ~ . ,
                  family = binomial ( link = "logit" ) ,
                  data = entrenamiento )

# Ajustar modelo con regresión escalonada.
cat ( " Modelo con regresión escalonada \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
mejor <- step ( nulo ,
                scope = list ( lower = nulo , upper = completo ) ,
                direction = "both" ,
                trace = 0 )

print(summary(mejor))

# Con esto podemos corroborar nuevamente que las variables que elegimos para la 
# construcción del modelo son adecuadas.


# Ajustar modelo con predictores previamente seleccionados.
cat ( " Modelo con con los 6 predictores seleccionados \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
modelo <- glm ( Result ~ SSLfinal_State + URL_of_Anchor + Prefix_Suffix + 
                  having_Sub_Domain + web_traffic, 
                family = binomial(link = "logit"),
                data = entrenamiento)

print(summary(modelo))

# Evaluar el modelo con el conjunto de entrenamiento.
cat ( "Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict ( modelo , entrenamiento , type = "response" )

umbral <- 0.5
preds_e <- sapply ( probs_e , function(p) ifelse ( p >= umbral , "1" , "-1" ) )
preds_e <- factor ( preds_e , levels = levels ( datos[["Result"]]) )

ROC_e <- roc (entrenamiento[["Result"]], probs_e )
plot ( ROC_e )

matriz_e <- confusionMatrix( preds_e , entrenamiento[["Result"]])
print ( matriz_e )

# Evaluar el modelo con el conjunto de prueba .
cat ( " Evaluación del modelo a partir del conjunto de prueba :\n " )
probs_p <- predict ( modelo , prueba , type = "response" )

preds_p <- sapply ( probs_p , function(p) ifelse ( p >= umbral , "1" , "-1" ) )
preds_p <- factor ( preds_p , levels = levels ( datos [[ "Result" ]]) )

ROC_p <- roc ( entrenamiento [[ "Result" ]] , probs_e )
plot ( ROC_p )

matriz_p <- confusionMatrix ( preds_p , prueba [["Result"]])
print ( matriz_p )

# Verificación de multicolinealidad .
cat ( " Verificación de colinealidad \n" )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
cat ( "\ nVIF :\n " )
vifs <- vif ( modelo )
print ( vifs )
cat ( "\ nPromedio VIF : ")
print ( mean ( vifs ) )

# Analisis de la confiabilidad del modelo
cat ( "\n \n " )
cat ( " Likelihood Ratio Test para el modelo \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
print ( anova ( modelo, test = "LRT" ) )


# Independencia de los residuos .
cat ( " Verificación de independencia de los residuos \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
print ( durbinWatsonTest ( modelo , max.lag = 5 ) )

# Detectar posibles valores atípicos .
cat ( " Identificación de posibles valores atípicos \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
plot ( modelo )

# Obtener los residuos y las estadísticas .
output <- data.frame ( predicted.probabilities = fitted ( modelo ) )
output [[ "standardized.residuals" ]] <- rstandard ( modelo )
output [[ "studentized.residuals" ]] <- rstudent ( modelo )
output [[ "cooks.distance" ]] <- cooks.distance ( modelo )
output [[ "dfbeta" ]] <- dfbeta ( modelo )
output [[ "dffit" ]] <- dffits ( modelo )
output [[ "leverage" ]] <- hatvalues ( modelo )

# Evaluar residuos estandarizados que escapen a la normalidad .
# 95% de los residuos estandarizados deberían estar entre -1.96 y 1.96, 
# y 99 % entre -2.58 y 2.58.
sospechosos1 <- which ( abs ( output [[ "standardized.residuals" ]]) > 1.96)
sospechosos1 <- sort ( sospechosos1 )
cat ( " \n \n " )
cat ( " Residuos estandarizados fuera del 95% esperado \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\n " )
print ( rownames ( entrenamiento [ sospechosos1 , ]) )

# Revisar casos con distancia de Cook mayor a uno .
sospechosos2 <- which ( output [[ " cooks.distance " ]] > 1 )
sospechosos2 <- sort ( sospechosos2 )
cat ( " \n \n " )
cat ( " Residuales con una distancia de Cook alta \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n " )
print ( rownames ( entrenamiento [ sospechosos2 , ]) )

# Revisar casos cuyo apalancamiento sea más del doble o triple del 
# apalancamiento promedio.
leverage.promedio <- ncol ( entrenamiento ) / nrow ( datos )
sospechosos3 <- which ( output [[ "leverage" ]] > leverage.promedio )
sospechosos3 <- sort ( sospechosos3 )
cat ( "\n \n " )
cat ( " Residuales con levarage fuera de rango ( > " )
cat ( round ( leverage.promedio , 3 ) , " ) " , " \n " , sep = " " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
print ( rownames ( entrenamiento [ sospechosos3 , ]) )

# Revisar casos con DFBeta >= 1.
sospechosos4 <- which ( apply ( output [[ "dfbeta" ]] >= 1 , 1 , any ) )
sospechosos4 <- sort ( sospechosos4 )
names ( sospechosos4 ) <- NULL
cat ( "\n \n " )
cat ( " Residuales con DFBeta sobre 1 \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - -\n " )
print ( rownames ( entrenamiento [ sospechosos4 , ]) )

# Detalle de las observaciones posiblemente atípicas .
sospechosos <- c ( sospechosos1 , sospechosos2 , sospechosos3 , sospechosos4)
sospechosos <- sort ( unique ( sospechosos ) )
cat ( "\n \n " )
cat ( " Casos sospechosos \n" )
cat ( " - - - - - - - - - - -- - - - - -\n " )
print ( entrenamiento [ sospechosos , ])
cat ( "\n \n " )
print ( output [ sospechosos , ])


# Validación cruzada

# Ajustar modelo usando validación cruzada de 5 pliegues .
modelo <- train ( Result ~ SSLfinal_State + URL_of_Anchor + Prefix_Suffix + 
                    having_Sub_Domain + web_traffic,
                  data = entrenamiento , 
                  method = "glm" ,
                  family = binomial ( link = "logit" ) ,
                  trControl = trainControl ( method = "cv" , number = 5 ,
                                             savePredictions = TRUE ) )
print(summary(modelo))

# Evaluar el modelo
cat ( " Evaluación del modelo basada en validación cruzada :\n " )
matriz <- confusionMatrix ( modelo$pred$pred , modelo$pred$obs )
print ( matriz )

# Conclusión:
# De acuerdo al ajuste realizado podemos determinar que nuestro modelo es confiable,
# ya que hemos elegido las variables adecuadas para predecir el resultado de un modelo
# que prediga los mejores métodos para dignosticar phishing.


