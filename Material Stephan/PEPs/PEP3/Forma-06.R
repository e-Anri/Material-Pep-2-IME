# PEP 3 IME
# Forma 06

# Pregunta 1:
# Utilizando los datos provistos, construya un modelo de regresión logística para 
# identificar la situación final de los estudiantes (respuesta), que tenga entre 3 y 8 
# variables predictoras y que consiga una exactitud (accuracy), en datos de prueba, 
# superior a 0,70. Considere la semilla 287 para determinar los conjuntos de entrenamiento 
# (80% de las observaciones) y prueba (20% de las observaciones). Verifique que el
# modelo no presenta multicolinealidad severa (usando el factor de inflación de la varianza).

library(caret)
library(car)
library(dplyr)
library(corrplot)
library(pROC) 

# Indicar directorio
#dir <- "/Users/.../Downloads"
dir <- "/Users/Downloads"

basename <- "Forma 06.csv"
file <- file.path(dir, basename)
datos <- read.csv(file = file)

# Semilla solicitada
set.seed(287)

# Se deja la variable final al principio ya que es la variable que nuestro modelo 
# debe predecir.
final <- datos$final
datos$final <- NULL
datos <- cbind (final, datos)

# Configuración de datos
datos$final <- factor(datos$final)
datos$colegio <- factor(datos$colegio)
datos$sexo <- factor(datos$sexo)
datos$residencia <- factor(datos$residencia)
datos$tam.fam <- factor(datos$tam.fam)
datos$estado.padres <- factor(datos$estado.padres)
datos$edu.madre <- factor(datos$edu.madre)
datos$edu.padre <- factor(datos$edu.padre)
datos$trab.madre <- factor(datos$trab.madre)
datos$trab.padre <- factor(datos$trab.padre)
datos$motivo <- factor(datos$motivo)
datos$apoderado <- factor(datos$apoderado)
datos$tpo.viaje <- factor(datos$tpo.viaje)
datos$estudio <- factor(datos$estudio)
datos$apoyo.alumno <- factor(datos$apoyo.alumno)
datos$apoyo.familia <- factor(datos$apoyo.familia)
datos$particular <- factor(datos$particular)
datos$actividades <- factor(datos$actividades)
datos$preescolar <- factor(datos$preescolar)
datos$superior <- factor(datos$superior)
datos$internet <- factor(datos$internet)
datos$relacion <- factor(datos$relacion)
datos$relacion.familiar <- factor(datos$relacion.familiar)
datos$tiempo.libre <- factor(datos$tiempo.libre)
datos$salidas <- factor(datos$salidas)
datos$alcohol.s <- factor(datos$alcohol.s)
datos$alcohol.f <- factor(datos$alcohol.f)
datos$salud <- factor(datos$salud)

# Separar conjuntos de entrenamiento y prueba.
n <- nrow ( datos )
n_entrenamiento <- floor ( 0.8 * n )
muestra <- sample.int ( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos [ muestra , ]
prueba <- datos [ - muestra , ]  


# Ajustar modelo nulo.
nulo <- glm ( final ~ 1 ,
              family = binomial(link = "logit"),
              data = entrenamiento )


# Ajustar modelo completo.
completo <- glm ( final ~ . ,
                  family = binomial ( link = "logit" ) ,
                  data = entrenamiento )

# Ajustar modelo con regresión escalonada.
cat ( " Modelo con regresión escalonada \n " )
mejor <- step ( nulo ,
                scope = list ( lower = nulo , upper = completo ) ,
                direction = "both" ,
                trace = 0 )

print(summary(mejor))

# Según la función step, basados en un modelo nulo obtenemos este modelo como 
# postulante a ser el mejor. De esta manera obtenemos que las variables predictoras
# que elegimos para la construcción del modelo son las siguientes:
# - reprobaciones
# - superior
# - residencia 
# - inasistencias
# - alcohol.f 
# - apoyo.alumno 
# - colegio 


# Ajustar modelo con predictores previamente seleccionados.
cat ( " Modelo con con los 7 predictores seleccionados \n " )
modelo <- glm ( final ~ reprobaciones + superior + residencia + 
                  inasistencias + alcohol.f + apoyo.alumno + colegio, 
                family = binomial(link = "logit"),
                data = entrenamiento)

print(summary(modelo))

# Evaluar el modelo con el conjunto de entrenamiento.
cat ( "Evaluación del modelo a partir del conjunto de entrenamiento :\n")
probs_e <- predict ( modelo , entrenamiento , type = "response" )

umbral <- 0.5
preds_e <- sapply ( probs_e , function(p) ifelse ( p >= umbral , "R" , "A" ) )
preds_e <- factor ( preds_e , levels = levels ( datos[["final"]]) )

ROC_e <- roc (entrenamiento[["final"]], probs_e )
plot ( ROC_e )

matriz_e <- confusionMatrix( preds_e , entrenamiento[["final"]])
print ( matriz_e )

# Evaluar el modelo con el conjunto de prueba .
cat ( " Evaluación del modelo a partir del conjunto de prueba :\n " )
probs_p <- predict ( modelo , prueba , type = "response" )

preds_p <- sapply ( probs_p , function(p) ifelse ( p >= umbral , "R" , "A" ) )
preds_p <- factor ( preds_p , levels = levels ( datos [[ "final" ]]) )

ROC_p <- roc ( entrenamiento [[ "final" ]] , probs_e )
plot ( ROC_p )

matriz_p <- confusionMatrix ( preds_p , prueba [["final"]])
print ( matriz_p )

# Aquí se puede apreciar que nuetro modelo consigue una exactitud (accuracy), en datos 
# de prueba superior a 0,70, para ser específicos Accuracy : 0.7273.

# Ahora, para revisar la confiabilidad del modelo:

# Verificación de multicolinealidad .
cat ( " Verificación de colinealidad \n" )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
cat ( "\ nVIF :\n " )
vifs <- vif ( modelo )
print ( vifs )
cat ( "\ nPromedio VIF : ")
print ( mean ( vifs ) )

# Dados los resultados obtenidos podemos verificar que el modelo no presenta 
# multicolinealidad severa a través del uso del factor de inflación de la varianza 
# (VIF) debido a que todos tienen un valor al rededor de 1, con una media de 1.2,
# lo que es mucho menor a 10, por lo que no hay de qué preocuparse.


# A pesar de no ser solicitado en el enunciado se continua evaluando la confiabilidad
# del modelo.

# Analisis de la confiabilidad del modelo.
cat ( "\n \n " )
cat ( " Likelihood Ratio Test para el modelo \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
print ( anova ( modelo, test = "LRT" ) )

# Independencia de los residuos.
cat ( " Verificación de independencia de los residuos \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
print ( durbinWatsonTest ( modelo , max.lag = 5 ) )

# Detectar posibles valores atípicos.
cat ( " Identificación de posibles valores atípicos \n " )
cat ( " - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - -- - - - - - -\n " )
plot ( modelo )

# Obtener los residuos y las estadísticas.
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

# Revisar casos con distancia de Cook mayor a uno.
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

# Detalle de las observaciones posiblemente atípicas.
sospechosos <- c ( sospechosos1 , sospechosos2 , sospechosos3 , sospechosos4)
sospechosos <- sort ( unique ( sospechosos ) )
cat ( "\n \n " )
cat ( " Casos sospechosos \n" )
cat ( " - - - - - - - - - - -- - - - - -\n " )
print ( entrenamiento [ sospechosos , ])
cat ( "\n \n " )
print ( output [ sospechosos , ])

# A modo de conclusión, se cumple con lo solicitado en el enunciado, se hace el 
# modelo con las variables más apropiadas, se consigue una exactitud (accuracy)
# en datos de prueba superior a 0,70 y el modelo no presenta multicolinealidad severa. 

# Pregunta 2: 
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas 
# dadas) en donde dos variables relacionadas con las eliminatorias para el campeonato 
# mundial de fútbol Catar 2022 presenten una correlación con un coeficiente de Pearson 
# de aproximadamente -0,7. Dé valores posibles, que podrían ser reales, y escriba código
# R para mostrar en un gráfico cómo se ve esta relación.

# Se propone como ejemplo una relación entre el sueldo de los jugadores y la cantidad 
# de pases que relizan en las eliminatorias de Catar 2022.

library(ggpubr)

sueldoJugadores <- c(21, 22.8, 18.7, 24.4, 23.4) # En miles de euros
cantidadPases <- c( 110, 93, 175, 123, 62)

datos2 <- data.frame(sueldoJugadores, cantidadPases)

modelo2 <- lm(cantidadGoles ~ sueldoJugadores, datos2)
summary(modelo2)

p2 <- ggscatter(datos2,
                x = "sueldoJugadores",
                y = "cantidadPases",
                color = "blue",
                fill = "blue",
                #shape = 21,
                #size = 3,
                add = "reg.line",
                add.params = list(color = "red", fill = "pink"),
                conf.int = TRUE,
                cor.coef = TRUE,
                cor.coeff.args = list(method = "pearson", label.sep = ", "))
print(p2)

# Se cumple con una correlacion y con un coef. de Pearson igual a -0.7
# Además, es significativo dado que se obtiene p = 0.18.

# Pregunta 3:
# Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas 
# dadas) de un problema de regresión lineal simple que podría aparecer en relación con 
# las calificaciones finales de estudiantes de primer año de Ingeniería durante la 
# pandemia de Covid-19. Justifique que es plausible obtener los datos para resolver el 
# problema que plantea

# Las calificaciones finales de estudiantes de primer año de Ingeniería durante la 
# pandemia de Covid-19 dependen de la cantidad de personas que habitan en la casa del 
# estudiante.

# Sería plausible obtener los datos para resolver este problema debido a que la ficha 
# para entrar a la universidad "Ficha socioeconómica", contiene la informacion 
# actualizada sobre cuantas personas habitan en la vivienda de tal estudiante.

# Esta informacion la posee la universidad por lo que se podría hacer un modelo 
# facilmemte.

