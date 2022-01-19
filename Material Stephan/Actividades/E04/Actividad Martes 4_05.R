
# Contexto
# Se sabe que el proceso de fabricacion de barras de acero para concreto
# reforzado producen barras con medidas de dureza que siguen una distribucion
# normal con desviacion estandar de 10 kilogramos de fuerza por milimetro
# cuadrado. Usando una muestra aleatoria de tamano 25, un ingeniero quiere 
# averiguar si una linea de produccion estagenerando barras con dureza media
# de 170 [kgf mm-2].

# Preguntas C
# 1. Si el ingeniero piensa rechazar la hipotesis nula si la muestra presenta
# una media menor a 168 [kgf mm-2] o mayor a 172 [kgf mm-2],pero la verdadera
# dureza media de la linea de produccion fuera 173 [kgf mm-2], ¿Cual seria 
# la probabilidad de que el ingeniero, que obviamente no conoce este dato, 
# tome la decision correcta de rechazar la hipotesis nula en favor de la
# hipotesis alternativa?


#Importacion de librerias
library(ggplot2)
library(ggpubr)

# Parametros conocidos.
sigma <- 10
alfa <- 0.05
n <- 25

# Calculo del error estandar.
SE <- sigma / sqrt(n)

# Grafico de la distribucion de la diferencia de medias si la difererencia
# de medias si la frecuencia fuera nula, con regiones de rechazo de la 
# hipotesis nula.
media_nula <- 173
Z_critico <- qnorm(alfa/2, mean = media_nula, sd = SE, lower.tail = FALSE) 
q_critico_inferior<-qnorm(alfa,mean=media_nula, sd=SE,lower.tail=TRUE)
q_critico_superior<-qnorm(alfa,mean=media_nula, sd=SE,lower.tail=FALSE)


x <- seq(media_nula - (5 * SE), media_nula + (5 * SE), 0.01)
y <- dnorm(x, mean = media_nula, sd = SE) 
df <- data.frame(x, y)

g <- ggplot(data = df, aes(x))

g <- g + stat_function(fun = dnorm ,
                       args = list(mean = media_nula, sd = SE),
                       colour = "red", 
                       size = 1)

g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name = "kilogramos de fuerza por milimetro cuadrado",
                            breaks = seq(media_nula - 5, media_nula + 5, 2))

g <- g + theme_pubr()

g <- g + geom_area(data = subset(df, x < q_critico_inferior), 
                   aes(y = y),
                   colour = "red", 
                   fill = "red", 
                   alpha = 0.5)

g <- g + geom_area(data = subset(df, x > q_critico_superior), 
                   aes(y = y),
                   colour = "red", 
                   fill = "red", 
                   alpha = 0.5)

print(g)


# Grafico de la distribucion de la diferencia de medias
media_efecto <- 168
x1<-seq(media_efecto - (5 * SE), media_efecto + (5 * SE), 0.01)
y1 <- dnorm(x, mean = media_efecto, sd = SE) 
df1 <- data.frame(x, y=y1)
df2<-data.frame(x,y)

g <- g + stat_function(fun = dnorm ,
                       args = list(mean = media_efecto, sd = SE), 
                       colour = "blue", 
                       size = 1)
g <- g + ylab("")

g <- g + geom_area(data = subset(df1, x < q_critico_inferior), 
                   aes(y=y),
                   colour = "blue", 
                   fill = "blue", 
                   alpha = 0.5)

g <- g + geom_area(data = subset(df2, x > q_critico_superior), 
                   aes(y=y),
                   colour = "blue", 
                   fill = "blue", 
                   alpha = 0.5)

print(g)


# Calculo del poder de acuerdo al analisis teorico.

poder <- pnorm(q_critico_inferior, 
               mean = media_efecto ,
               sd = SE,
               lower.tail = TRUE) 
+ pnorm(q_critico_superior, 
        mean = media_efecto, 
        sd = SE,
        lower.tail = FALSE)

print(poder)

# Respuesta C-1
# De acuerdo al poder que obtuvimos, la probabilidad que tome la decision 
# correcta de rechazar la hipotesis nula en favor de la hipotesis alternativa 
# sera de p = 0.8037649.


#2.¿A cuanto cambiaria esta probabilidad si se pudiera tomar
#una muestra de 64 barras?

sigma <- 10
alfa <- 0.05
n <- 64

# Calculo del error estandar.
SE <- sigma / sqrt(n)

# Grafico de la distribucion de la diferencia de medias si la difererencia
# de medias si la frecuencia fuera nula, con regiones de rechazo de la 
# hipotesis nula.
media_nula <- 173
Z_critico <- qnorm(alfa/2, mean = media_nula, sd = SE, lower.tail = FALSE) 
q_critico_inferior<-qnorm(alfa,mean=media_nula, sd=SE,lower.tail=TRUE)
q_critico_superior<-qnorm(alfa,mean=media_nula, sd=SE,lower.tail=FALSE)


x <- seq(media_nula - (5 * SE), media_nula + (5 * SE), 0.01)
y <- dnorm(x, mean = media_nula, sd = SE) 
df <- data.frame(x, y)

g <- ggplot(data = df, aes(x))
g <- g + stat_function(fun = dnorm ,
                       args = list(mean = media_nula, sd = SE),
                       colour = "red", 
                       size = 1)

g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name = "kilogramos de fuerza por milimetro cuadrado",
                            breaks = seq(media_nula - 5, media_nula + 5, 2))

g <- g + theme_pubr()

g <- g + geom_area(data = subset(df, x < q_critico_inferior), 
                   aes(y = y),
                   colour = "red", 
                   fill = "red", 
                   alpha = 0.5)

g <- g + geom_area(data = subset(df, x > q_critico_superior), 
                   aes(y = y),
                   colour = "red", 
                   fill = "red", 
                   alpha = 0.5)

print(g)


# Grafico de la distribucion de la diferencia de medias
media_efecto <- 168
x1<-seq(media_efecto - (5 * SE), media_efecto + (5 * SE), 0.01)
y1 <- dnorm(x, mean = media_efecto, sd = SE) 
df1 <- data.frame(x, y=y1)
df2<-data.frame(x,y)

g <- g + stat_function(fun = dnorm ,
                       args = list(mean = media_efecto, sd = SE), 
                       colour = "blue", 
                       size = 1)
g <- g + ylab("")

g <- g + geom_area(data = subset(df1, x < q_critico_inferior), 
                   aes(y=y),
                   colour = "blue", 
                   fill = "blue", 
                   alpha = 0.5)

g <- g + geom_area(data = subset(df2, x > q_critico_superior), 
                   aes(y=y),
                   colour = "blue", 
                   fill = "blue", 
                   alpha = 0.5)

print(g)


# Calculo del poder de acuerdo al analisis teorico.

poder <- pnorm(q_critico_inferior, 
               mean = media_efecto ,
               sd = SE,
               lower.tail = TRUE) 
+ pnorm(q_critico_superior, 
        mean = media_efecto, 
        sd = SE,
        lower.tail = FALSE)

print(poder)

# Respuesta C-2
# En este caso de acuerdo al poder que obtuvimos, la probabilidad que tome la 
# decision correcta de rechazar la hipotesis nula en favor de la hipotesis 
# alternativa cambiara a p = 0.9907423.


# 3. ¿Cuantas barras deberian revisarse para conseguir un
# poder estadistico de 0,90 y un nivel de significacion de 0,05?

# Calculo del poder usando la funcion power.t.test().
poder_3 <- power.t.test(n = NULL, 
                        delta=16,
                        sd=10,
                        power = 0.90,
                        sig.level = 0.05, 
                        type="paired",
                        alternative = "one.sided")

print(poder_3)

#Respuesta C-3
# Para conseguir un poder estadistico de 0,90 y un nivel de significacion 
# de 0,05, se deben revisar 5 barras.


# 4. ¿Y si quisiera ser bien exigente y bajar la probabilidad 
# de cometer un error de tipo 1 a un 1% solamente?

# Calculo del poder usando la funcion pwr.t.test().
library(pwr)

n2 <- 5
SE2 <- 0.01

d_cohen <- (173/sigma)*((n-2)/(n-1.25))

poder_4 <- pwr.t.test(n = NULL,
                      power=1,
                      sig.level = alfa,
                      type = "paired", 
                      alternative = "two.sided", 
                      d = d_cohen)

print(poder_4)

# Respuesta C-4
# Se intentó llegar a obtener n de acuerdo a bajar la probabilidad 
# de cometer un error de tipo 1 a un 1% solamente, pero no se pudo 
# lograr por falta de tiempo.