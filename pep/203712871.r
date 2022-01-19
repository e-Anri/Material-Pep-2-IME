# Nombre: Estefanía Alvarez


# Importamos las librerías
library(ggpubr)
library(dplyr)


# Importamos los datos
datos <- read.csv("C:\\Users\\estef\\Desktop\\datos.csv", encoding = "UTF-8", sep = ";")

# Para este problema se puede apreciar que nos encontramos frente a dos muestras pareadas, 
# pues las instancias tratan de las mismas observaciones en dos ocasiones distintas
# (antes y después de entrenar)

# Ya que no se conoce la desviación estándar, se realizará una prueba t de student para dos muestras pareadas
# verificando primero si se cumplen las condiciones:

# 1.- al provenir de una muestra aleatoria de 40 reclutas,
# se puede decir que las observaciones son independientes entre sí


# //// verificación de normalidad ////

# se define la semilla
set.seed(523)

datos2 <- sample_n(datos, 40)

antes <- datos2 %>% select(imc_1)
despues <- datos2 %>% select(imc_2)

diferencia <- antes - despues

alfa <- 0.05


# se decide probar si la muestra sigue una distribución normal mediante shapiro
normalidad <- shapiro.test(diferencia)


# Las hipótesis a plantear son:
# H0: El índice de masa muscular se ha reducido en promedio 2,5 [Kg/m2] tras el entrenamiento
# H1: El índice de masa muscular no se ha reducido tras el entrenamiento

valor_nulo <- 2.5

# se realiza la prueba t para dos muestras pareadas
prueba <- t.test(x = antes, 
                 y = despues, 
                 paired = TRUE, 
                 alternative = "two.sided", 
                 mu = valor_nulo,
                 conf.level = 1 - alfa)

# Debido a por falta de tiempo no se pudo llegar más lejos en la implementación del programa, 
# de todas maneras a simple vista al comparar ambas muestras se puede ver que de hecho el imc
# en los reclutas efectivamente disminuyó, sin embaergo, debido a que no funcionaron los cálculos 
# planteados, no se puede decir con certeza si efectivamente disminuyó 2.5 Kg/m2, la tendencia 
# en los datos muestra que no, por lo tanto, se rechaza la Hipótesis nula (H0) en favor de la
# hipótesis alternativa


