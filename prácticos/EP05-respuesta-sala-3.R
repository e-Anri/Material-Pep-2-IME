#Nombres:
#Estefanía Alvarez 20.371.287-1
#Felipe Cornejo 20.427.782-6
#David Morales 19.881.480-6
#Claudio Muñoz 20.003.395-7

library(tidyr)
library (ggpubr)
library(ggplot2)
library (dplyr)
library(pwr)
library (tidyverse)

# ////////////////////////// Enunciado //////////////////////////

#Se sabe que el proceso de fabricación de barras de acero para concreto reforzado 
#producen barras con medidas de dureza que siguen una distribución normal con 
#desviación estándar de 10 kilogramos de fuerza por milímetro cuadrado. Usando 
#una muestra aleatoria de tamaño 50, un ingeniero quiere averiguar si una línea 
#de producción está generando barras con dureza media de 170 [kgf mm-2] 

#Desviación Estándar
s <- 10

#Tamaño de la muestra
n <- 50

#Error Estándar
SE = s / sqrt (n)

# ////////////////////////// PREGUNTA 1 //////////////////////////

#Si el ingeniero está seguro que la verdadera dureza media no puede ser menor a 
#los 170 [kgf mm-2] y piensa rechazar la hipótesis nula cuando la muestra 
#presente una media mayor a 173 [kgf mm^-2], ¿cuál es la probabilidad de que 
#cometa un error de tipo 1? 

#Hipotesis
#H0: m = 170
#H1: m > 170

# se utiliza pnorm para calcular la probabilidad de que la variable tome valores
#menores o iguales que un valor dado, a partir de las probabilidades.


cola_superior <- pnorm(mean = 170, sd = SE, q = 173, lower.tail = FALSE)

#Como se trata de una prueba unilateral, el nivel de significacion es igual la cola superior
alfa = cola_superior

print(alfa)

#R: El nivel de significación corresponde a 0,01694 aproximadamente, lo
#que equivale a un 1,694% de probabilidad de cometer un error de tipo 1.

#Gráfico área error tipo 1
x <- seq(170 - s*SE,170 + s*SE,0.01)
y <- dnorm(x, mean = 170 , sd = SE)
g <- ggplot(data = data.frame(x,y), aes(x))
g <- g + stat_function(
  fun = dnorm ,
  n = 100,
  args = list(mean = 170 , sd = SE),
  colour = "black", size = 1)
g <- g + ylab("Densidad")
g <-g + xlab("Dureza")
g <-g + labs(title = "Área Error tipo 1")
g <- g + theme_pubr()
g <- g + geom_area(data = subset(data.frame(x,y), x > 173), 
                     aes(y = y),
                     colour = "red",
                     fill = "red",
                     alpha = 0.5)
print(g)

# ////////////////////////// PREGUNTA 2 //////////////////////////

#Si la verdadera dureza media de la línea de producción fuera 172 [kgf mm^-2], 
#¿cuál sería la probabilidad de que el ingeniero, que obviamente no conoce este 
#dato, cometa un error de tipo 2?

media <- 172

poder_estadistico <- power.t.test(n = 50,
                               sd = s,
                               sig.level = alfa,
                               delta = media - 170,
                               type = "one.sample",
                               alternative = "one.sided")
print(poder_estadistico)

power = poder_estadistico$power

beta <- 1 - power

print(beta)

#R: Por lo tanto, la probabilidad de cometer un error de tipo 2 en este caso es de aproximadamente
#77.0179%

#Gráfico área error tipo 2
y2 <- dnorm(x, mean = 172 , sd = SE)
g1<- ggplot(data = data.frame(x,y), aes(x))
g1 <- g1 + stat_function(
  fun = dnorm ,
  n = 100,
  args = list(mean = 170 , sd = SE),
  colour = "black", size = 1)
g2 <- ggplot(data = data.frame(x,y2), aes(x))
g2 <- g1 + stat_function(
  fun = dnorm ,
  n = 100,
  args = list(mean = 172 , sd = SE),
  colour = "blue", size = 1)
g2 <- g2 + ylab("Densidad")
g2 <-g2 + xlab("Dureza")
g2 <-g2 + labs(title = "Área Error tipo 2")
g2 <- g2 + theme_pubr()
g2 <- g2 + geom_area(data = subset(data.frame(x,y2), x < 173), 
                   aes(y = y2),
                   colour = "blue",
                   fill = "blue",
                   alpha = 0.5)
print(g2)


# ////////////////////////// PREGUNTA 3 //////////////////////////
#Como  no se conoce  la verdadera dureza  media, genere un gráfico del poder 
#estadístico con las condiciones anteriores, pero suponiendo que las verdaderas 
#durezas medias podrían variar de 170 a 178 [kgf mm^-2].

# Valores conocidos
n <- 50
desv_estandar <- 10

medias <- seq(170, 178, 0.01)
media_nula <- 174 # (178 + 170) / 2


# Cálculo del efecto
efecto <- (medias - media_nula) / desv_estandar

# Cálculo del poder
resultado <- power.t.test(n = n, delta = efecto, sd = desv_estandar, sig.level = alfa
                          , type = "one.sample", alternative = "two.sided")$power


# Se define el gráfico
datos <- data.frame(efecto, resultado)
datos <- datos %>% pivot_longer(!"efecto", names_to = "fuente", values_to = "poder")
niveles <- c("resultado")
etiquetas <- c("resultado")
datos[["fuente"]] <- factor(datos[["fuente"]], levels = niveles, labels = etiquetas)

g <- ggplot (datos, aes(efecto, poder, colour = factor (fuente)))
g <- g + geom_line()
g <- g + labs ( colour = "")
g <- g + ylab ("Poder estadístico")
g <- g + xlab ("Tamaño del efecto")

# Título para el gráfico
g <- g + theme_pubr ()
g <- g + ggtitle ("Poder v/s tamaño del efecto durezas")
g <- g + geom_vline ( xintercept = 0, linetype = "dashed")

print(g)


# Luego, como se puede apreciar en el gráfico generado, mientras mayor sea el tamaño del efecto,
# mayor será entonces el poder estadístico sobre la muestra.



# ////////////////////////// PREGUNTA 4 //////////////////////////
#¿Cuántas barras deberían revisarse para conseguir un poder estadístico de 0,90 y un nivel de significación
#de 0,05?

#Fijar valores conocidos
diferencia <- 2 # 172 - 170
desv_estandar <- 10
alfa <- 0.05
poder <- 0.9


#Cálculo del número de barras
resultado <- power.t.test(n = NULL, delta = diferencia, sd = desv_estandar,
                          sig.level = alfa, power = poder, type = "one.sample", alternative = "two.sided")

n <- ceiling(resultado[["n"]])

print(n)


# Luego, deberían revisarse un total de 265 barras para conseguir un poder estadístico de 0.90 y un nivel
# de significación de 0.05


# ////////////////////////// PREGUNTA 5 //////////////////////////
#¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error de tipo 1 a un 1% solamente?

#Fijar valores conocidos
diferencia <- 2 # 172 - 170
desv_estandar <- 10
poder <- 0.9

#Se cambia el valor de alfa a 0.01
alfa <- 0.01


#Cálculo del número de barras, con las nuevas condiciones
resultado <- power.t.test(n = NULL, delta = diferencia, sd = desv_estandar,
                          sig.level = alfa, power = poder, type = "one.sample", alternative = "two.sided")

n <- ceiling(resultado[["n"]])

print(n)


# Luego, deberían revisarse un total de 376 barras para bajar la probabilidad de cometer un error de tipo 1
# a un 1% solamente



# Finalmente, en los ejercicios 4 y 5 se puede concluir que, a medida que aumenta la necesidad de disminuir la probabilidad de cometer errores de 
# tipo 1, se puede apreciar que se vuelve necesario aumentar en 111 la cantidad de barras, esto con el fin de lograr un 
# 1% de probabilidad de cometer este tipo de error 1








