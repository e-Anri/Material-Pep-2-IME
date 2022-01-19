#Nombres:
#Estefanía Alvarez 20.371.287-1
#Felipe Cornejo 20.427.782-6
#David Morales 19.881.480-6
#Claudio Muñoz 20.003.395-7

# Instalación de paquetes
library(tidyr)
library (TeachingDemos)
library (ggpubr)
library(ggplot2)
library (dplyr)
if (!require(nortest)){
  install.packages("nortest", dependencies = TRUE )
  require (nortest)
}



# ////////////////////////// PREGUNTA 1 //////////////////////////

#¿Proporciona esta información evidencia para concluir que la compañía no está llenando sus frascos como
#lo anuncia?

texto<- "4.62 4.43 5.18 4.89 4.89 5.41 4.87 5.07 5.30 4.98 4.54 5.21 4.60 4.71 4.58
4.99 5.05 4.70 4.63 4.95 4.85 4.19 5.25 4.69 5.03 4.74 4.67 4.85 4.45 4.93
4.42 4.40 5.59 4.69 5.42 5.19 4.99 4.88 4.03 5.51 4.90 4.43 4.93 4.84 4.73
4.89 4.53 4.97 5.10 5.95 4.95 4.18 4.91 4.87 5.38 5.49 4.96 4.76 4.76 4.63
5.10 4.84 4.87 4.39 4.99 5.03 4.31 5.05 4.71 4.78 4.90 5.02 4.84 5.18 4.79
4.99 4.55 4.70 4.74 4.60 4.94 5.25 5.01 4.95 4.19 5.27 5.00 5.15 5.12 4.34
4.27 4.92 4.98 4.91 5.05 5.28 4.29 5.58 5.55 4.60"
file <- textConnection(texto) 
datos <- scan(file)
peso <- data.frame("peso" = datos, stringsAsFactors = TRUE)
media <- mean(datos)
desv_est <- sd(datos)
#Hipotesis
#Hipotesis Nula: 
# m = 5
#Hipotesis Alternativa
# m <> 5

#Como se tienen mas de 30 observaciones en la muestra se decide comprobar si es posible utilizar 
#la prueba Z

#Pruebas de  normalidad
#Primero se comprueba si es factible realizar la prueba verificando si la muestra 
#sigue aproximadamente una distribución normal
# Gráfico Q-Q para la variable peso
g <- ggqqplot(peso, x = "peso", color = "red")
print(g)
hist(datos)
alfa = 0.01
Z <- (media - 5) / desv_est
cat("Z =", Z, "\n")
p <-2 * pnorm(Z, lower.tail = FALSE)

#Prueba de Shapiro-Wilk
shapiro.test(datos)

#Prueba de Kolmogorov-Smirnov
lillie.test(datos)

# Realizar la prueba t para la muestra.
t.test(datos, mu = 5, alternative = "two.sided", conf.level = 1 - alfa)

# Se puede ver que el valor-p es mucho menor que el nivel de significancia, (0.000513 < 0.01), por lo cual se rechaza la hipotesis
# nula a favor de la alternativa. Considerando tambien que la media de los datos, se encuentra dentro del intervalo de confianza
# [4.784641, 4.966559].
# Se concluye entonces que el laboratorio no está llenando los frascos como lo anuncia. 

# ////////////////////////// PREGUNTA 2 //////////////////////////

# ¿Sugieren los datos que el contenido total de minerales en los huesos del cuerpo durante el posdestete
# excede el de la etapa de lactancia por más de 40 g?


#Paso 1: Se cargan los datos del enunciado
antes<- "2825 1843 1928 2549 1924 2621 2114 2175 2541 1628"
despues<- "2895 2006 2126 2885 1942 2626 2164 2184 2627 1750"
file1 <- textConnection(antes) 
file2 <- textConnection(despues) 
lactancia <- scan(file1)
posdestete<- scan(file2)
datos.pre.post = data.frame(
  Sujeto = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
  Lactancia = lactancia,
  Posdestete = posdestete
)

#Paso 2: Pruebas de Normalidad
shapiro.test(lactancia)
shapiro.test(posdestete)
hist(lactancia)
hist(posdestete)
datos1 <- data.frame("sujeto" = lactancia, stringsAsFactors = TRUE)
ggqqplot(datos1, x = "sujeto", color = "blue", title = "Lactancia")
datos2 <- data.frame("sujeto" = posdestete, stringsAsFactors = TRUE)
ggqqplot(datos2, x = "sujeto", color = "red", title = "Posdestete")
#Como los valores p en la pruebas de Shapiro-Wilk son mayores a un nivel de significación de 0.05 no se rechazan las 
#hipótesis nulas donde H0:La variable tiene una distribución normal, por lo que se 
#asume que estas poseen una distribución normal.

#Paso 3: Hipótesis
#Hipótesis Nula
#La media de las diferencias es menor o igual a 40
#H0: m2 - m1 <= 40
#Hipótesis Alternativa
#La media de las diferencias es mayor a 40
#H1: m2 - m1 > 40
diferencia <- datos.pre.post$Posdestete - datos.pre.post$Lactancia
datos.pre.post<- cbind(datos.pre.post, diferencia)
head(datos.pre.post,4)

#Paso 4: Se realiza la prueba T
t.test(datos.pre.post$Posdestete,datos.pre.post$Lactancia, paired = TRUE, mu = 40,
       conf.level = 1 - 0.05, alternative = "greater")

#Conclusión
#Con un nivel de significancia de 0,05, como el valor p=0.0382 es menor a éste, 
#y además la media de las diferencias cae dentro del intervalo de confianza, se 
#puede rechazar la hipótesis nula a favor de la hipótesis alternativa, por lo 
#que se concluye que los datos sugieren que, en promedio el contenido total de 
#minerales en los huesos del cuerpo durante el posdestete excede el de la etapa 
#de lactancia por más de 40g




# ////////////////////////// PREGUNTA 3 //////////////////////////

# La avicultura de carne es un negocio muy lucrativo, y cualquier método que ayude al rápido crecimiento de
# los pollitos es beneficioso, tanto para las avícolas como para los consumidores. En el paquete datasets
# de R están los datos (chickwts) de un experimento hecho para medir la efectividad de varios
# suplementos alimenticios en la tasa de crecimiento de las aves. Pollitos recién nacidos se separaron
# aleatoriamente en 6 grupos, y a cada grupo se le dio un suplemento distinto. Para productores de la 6ta
# región, es especialmente importante saber si existe diferencia en la efectividad entre el suplemento
# basado en harina animal (meatmeal) y el basado en soya (soybean).


#Llamado de librerías
library(ggpubr)
library(ggplot2)
library (dplyr)

#Importación de datos
datos <- chickwts


#Grafico de cajas que compara los pesos de los pollitos por tipo de alimento suministrado
g <- boxplot(weight ~ feed,
             data = datos,
             border = "red",
             col = "pink",
             ylab = "Pesos de los Pollitos [g]",
             xlab = "Tipo de Alimento Suministrado")

print (g)


# Se grafican todas las muestras con cajas como primera instancia para responder la pregunta planteada, viendo que 
# la mayoría de los datos de meatmeal superan a soybean.


# Parte 1: Verificación 

# Para poder utilizar la prueba T en 2 muestras independientes, requerimos demostrar que cumplen los requisitos:
#1. Cada muestra cumple las condiciones para usar la distribución t.
#2. Las muestras son independientes entre sí.

# Para la 1, las condiciones de la distribución t es que cada muestra cuente con una distribución normal y que 
# las muestras sean independientes entre sí. Por lo tanto, verificaremos estas condiciones para cada muestra (soybean y meatmeal)


# Se separan las muestras meatmeal y soybean
datos.meatmeal <- datos %>% filter(feed == "meatmeal")
datos.soybean <- datos %>% filter(feed == "soybean")



# Verificar si las distribuciones se acercan a la normal 
g.meatmeal <- ggqqplot (data = datos.meatmeal,
                        x = "weight",
                        color = "steelblue",
                        title = "Gráfico Q-Q meatmeal v/s distr.normal")
print (g.meatmeal)


g.soybean <- ggqqplot (data = datos.soybean,
                       x = "weight",
                       color = "red",
                       title = "Gráfico Q-Q soybean v/s distr.normal")
print (g.soybean)


# Se puede ver a partir de los gráficos Q-Q generados, que las muestras efectivamente poseen distribuciones normales

# Para complementar las verificaciones, se realizan las pruebas de Shapiro-Wilk, considerando un alfa = 0.01
shapiro.test(datos.meatmeal[["weight"]])
shapiro.test(datos.soybean[["weight"]])

# Como los resultados dieron un p mayor a 0.01, se concluye finalmente que sí corresponden a una distribución normal ambas muestras

# De manera adicional, como son muestras tomadas al azar y una muestra no depende de la otra, se concluye que son independientes entre sí, 
# cumpliéndose todos los requisitos para realizar una prueba T con 2 muestras independientes (meatmeal y soybean)


# Parte 2: Definición de las Hipótesis

# Hipótesis nula:
# H0: No existe una diferencia en efectividad entre las muestras 

# Hipótesis alternativa
# Ha: Existe diferencia, en promedio, entre las efectividades de las muestras

# Vamos a considerar que la efectividad se puede medir como el promedio de los pesos de los pollitos
# que consuman tal suplemento

media.meatmeal <- mean(datos.meatmeal[["weight"]])
media.soybean <- mean(datos.soybean[["weight"]])

# H0:  media.meatmeal - media.soybean = 0
# Ha: media.meatmeal - media.soybean <> 0



# Parte 3
# Se parte realizando las pruebas t para 2 muestras independientes 

# Mantendremos el nivel de significación igual a 0.01
alfa <- 0.01

prueba <- t.test(x = datos.meatmeal[["weight"]], 
                 y = datos.soybean[["weight"]],
                 paired = FALSE, 
                 alternative = "two.sided",
                 mu = 0,
                 conf.level = 1 - alfa)

print(prueba)



diferencia <- media.meatmeal - media.soybean
cat ("diferencia de las medias = ", diferencia, "[g]")

print(diferencia)


# Conclusión
# A partir de la prueba T realizada, se rechaza la Hipótesis alternativa (Ha) a favor
# de la Hipótesis nula (H0), ya que el valor p resultante es mucho mayor que el valor de significancia (0.01). También obtuvimos que la diferencia
# entre las medias es de 30 gramos, y que el promedio está dentro del intervalo de confianza [-38.96506, 99.9261]. 

# En consecuencia, podemos concluir con un 99% de confianza que los suplementos no presentan gran diferencia en la efectividad.

# Por otra parte, el gráfico de caja muestra que una gran cantidad de datos de la muestra meatmeal están muy por encima de las caja de datos de la muestra soybean (casi el 50%),
# entonces, se podría recomendar tomar diferentes muestras para volver a realizar este estudio o utilizar muestras más grandes.


