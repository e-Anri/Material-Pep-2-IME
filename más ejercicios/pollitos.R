#Llamado de librer�as
library(ggpubr)
library(ggplot2)
library (dplyr)

#Importaci�n de datos
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
# la mayor�a de los datos de meatmeal superan a soybean.


# Parte 1: Verificaci�n 

# Para poder utilizar la prueba T en 2 muestras independientes, requerimos demostrar que cumplen los requisitos:
#1. Cada muestra cumple las condiciones para usar la distribuci�n t.
#2. Las muestras son independientes entre s�.

# Para la 1, las condiciones de la distribuci�n t es que cada muestra cuente con una distribuci�n normal y que 
# las muestras sean independientes entre s�. Por lo tanto, verificaremos estas condiciones para cada muestra (soybean y meatmeal)


# Se separan las muestras meatmeal y soybean
datos.meatmeal <- datos %>% filter(feed == "meatmeal")
datos.soybean <- datos %>% filter(feed == "soybean")



# Verificar si las distribuciones se acercan a la normal 
g.meatmeal <- ggqqplot (data = datos.meatmeal,
                   x = "weight",
                   color = "steelblue",
                   title = "Gr�fico Q-Q meatmeal v/s distr.normal")
print (g.meatmeal)


g.soybean <- ggqqplot (data = datos.soybean,
               x = "weight",
               color = "red",
               title = "Gr�fico Q-Q soybean v/s distr.normal")
print (g.soybean)


# Se puede ver a partir de los gr�ficos Q-Q generados, que las muestras efectivamente poseen distribuciones normales

# Para complementar las verificaciones, se realizan las pruebas de Shapiro-Wilk, considerando un alfa = 0.01
shapiro.test(datos.meatmeal[["weight"]])
shapiro.test(datos.soybean[["weight"]])

# Como los resultados dieron un p mayor a 0.01, se concluye finalmente que s� corresponden a una distribuci�n normal ambas muestras

# De manera adicional, como son muestras tomadas al azar y una muestra no depende de la otra, se concluye que son independientes entre s�, 
# cumpli�ndose todos los requisitos para realizar una prueba T con 2 muestras independientes (meatmeal y soybean)


# Parte 2: Definici�n de las Hip�tesis

# Hip�tesis nula:
# H0: No existe una diferencia en efectividad entre las muestras 

# Hip�tesis alternativa
# Ha: Existe diferencia, en promedio, entre las efectividades de las muestras

# Vamos a considerar que la efectividad se puede medir como el promedio de los pesos de los pollitos
# que consuman tal suplemento

media.meatmeal <- mean(datos.meatmeal[["weight"]])
media.soybean <- mean(datos.soybean[["weight"]])

# H0:  media.meatmeal - media.soybean = 0
# Ha: media.meatmeal - media.soybean <> 0



# Parte 3
# Se parte realizando las pruebas t para 2 muestras independientes 

# Mantendremos el nivel de significaci�n igual a 0.01
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


# Conclusi�n
# A partir de la prueba T realizada, se rechaza la Hip�tesis alternativa (Ha) a favor
# de la Hip�tesis nula (H0), ya que el valor p resultante es mucho mayor que el valor de significancia (0.01). Tambi�n obtuvimos que la diferencia
# entre las medias es de 30 gramos, y que el est� dentro del intervalo de confianza [-38.96506, 99.9261]. 

# En consecuencia, podemos concluir con un 99% de confianza que los suplementos no presentan gran diferencia en la efectividad.

# Por otra parte, el gr�fico de caja muestra que una gran cantidad de datos de la muestra meatmeal est�n muy por encima de los datos de la muestra soybean (casi el 50%),
# entonces, se podr�a recomendar tomar diferentes muestras para volver a realizar este estudio o utilizar muestras m�s grandes.


