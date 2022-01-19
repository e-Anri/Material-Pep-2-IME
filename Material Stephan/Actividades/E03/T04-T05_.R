
# Se importan librerias
library(ggpubr)
library(ggplot2)

#Actividad: contraste de hipótesis con una muestra

#Problema C

# Un laboratorio que fabrica aspirina en Chile llena los frascos por peso en 
# lugar de por conteo. Cada frasco contiene 30 tabletas si es que se cumple el 
# supuesto de que el peso promedio de las tableta es de 5 gramos. Se obtuvo la 
# siguiente muestra de 100 tabletas.

# 4.62 4.43 5.18 4.89 4.89 5.41 4.87 5.07 5.30 4.98 4.54 5.21 4.60
# 4.71 4.58 4.99 5.05 4.70 4.63 4.95 4.85 4.19 5.25 4.69 5.03 4.74
# 4.67 4.85 4.45 4.93 4.42 4.40 5.59 4.69 5.42 5.19 4.99 4.88 4.03
# 5.51 4.90 4.43 4.93 4.84 4.73 4.89 4.53 4.97 5.10 5.95 4.95 4.18
# 4.91 4.87 5.38 5.49 4.96 4.76 4.76 4.63 5.10 4.84 4.87 4.39 4.99
# 5.03 4.31 5.05 4.71 4.78 4.90 5.02 4.84 5.18 4.79 4.99 4.55 4.70
# 4.74 4.60 4.94 5.25 5.01 4.95 4.19 5.27 5.00 5.15 5.12 4.34 4.27
# 4.92 4.98 4.91 5.05 5.28 4.29 5.58 5.55 4.60

# ¿Proporciona esta información una fuerte evidencia para concluir que la 
# compañía no está llenando sus frascos como lo anuncia?

# Se propone o una hipótesis nula y una hipótesis alternativa 
# H0: media = 5
# H1: media < 5

# Se cargan los conjuntos de datos en variables 
texto <- "4.62 4.43 5.18 4.89 4.89 5.41 4.87 5.07 5.30 4.98 4.54 5.21 4.60
          4.71 4.58 4.99 5.05 4.70 4.63 4.95 4.85 4.19 5.25 4.69 5.03 4.74
          4.67 4.85 4.45 4.93 4.42 4.40 5.59 4.69 5.42 5.19 4.99 4.88 4.03
          5.51 4.90 4.43 4.93 4.84 4.73 4.89 4.53 4.97 5.10 5.95 4.95 4.18
          4.91 4.87 5.38 5.49 4.96 4.76 4.76 4.63 5.10 4.84 4.87 4.39 4.99
          5.03 4.31 5.05 4.71 4.78 4.90 5.02 4.84 5.18 4.79 4.99 4.55 4.70
          4.74 4.60 4.94 5.25 5.01 4.95 4.19 5.27 5.00 5.15 5.12 4.34 4.27
          4.92 4.98 4.91 5.05 5.28 4.29 5.58 5.55 4.60"

archivo <- textConnection(texto)

datos <- scan(archivo)

dataframe <- data.frame(datos)

# Se verifica que la distribución se acerca a la normal
ggqqplot(data = dataframe, 
         x = "datos", 
         color ="red", 
         xlab = "Teorico",
         ylab = "Muestra",
         title = "Grafico Q-Q v/s distribucion normal")

# Se calculan estimadores puntuales y estadisticos utiles
mu_0 = 5
alfa <- 0.05

#Se decide aplicar la prueba t Student
t.test(datos, alternative="less", mu = mu_0, conf.level = 1-alfa)

#Conclusión:
# Al aplicar la prueba, se observa que el valor p obtenido es muy pequeño 
# 0.0002565, menor a 0.05, por lo que es poco probable que la hipotesis nula
# sea cierta. Finalmente se rechaza H0 en favor de HA. Se puede afirmar con un
# 95% de confianza, que el peso promedio de las tableta es menor que 5 gramos y
# la compñia no está llenando sus frascos como lo anuncia.


#_______________________________________________________________________________


#Actividad: contraste de hipótesis con dos muestras

# Problema A

# Se sabe que la lactancia estimula una pérdida de masa ósea para proporcionar 
# cantidades de calcio adecuadas para la producción de leche. Un estudio intentó 
# determinar si madres adolescentes podían recuperar niveles más normales a pesar 
# de no consumir suplementos (Amer. J. Clinical Nutr., 2004; 1322-1326). El estudio
# obtuvo las siguientes medidas del contenido total de minerales en los huesos del
# cuerpo (en gramos) para una muestra de madres adolescentes tanto durante la 
# lactancia (6-24 semanas postparto) y posterior a ella (12-30 semana postparto)

# ¿Sugieren los datos que el contenido total de minerales en los huesos del 
# cuerpo durante el posdestete excede el de la etapa de lactancia por más de 
# 25 g? 

# Se formulan las hipotesis
# H0: El contenido total de minerales en los huesos del cuerpo durante el 
#     posdestete no alcanza al de la etapa de lactancia igual a 25 g.
# H1: El contenido total de minerales en los huesos del cuerpo durante el 
#     posdestete excede el de la etapa de lactancia por más de 25 g.

# Se propone o una hipótesis nula y una hipótesis alternativa 
# H0: media_posdestete - media_lactancia = 25
# H1: media_posdestete - media_lactancia > 25


# Cargar los datos y guardarlos como data frame.
instancia <- seq(1, 10, 1)

lactancia <- c(1928,2549,2825,1924,1628,2175,2114,2621,1843,2541)

posdestete <- c(2126,2885,2895,1942,1750,2184,2164,2626,2006,2627)

diferencia <- posdestete - lactancia

dataframe <- data.frame(instancia, lactancia, posdestete, diferencia)

# Se verifica que la distribucion de las diferencias se acerca a la normal.
ggqqplot(data = dataframe,
         x = "diferencia",
         color = "deeppink3",
         xlab = "Teorico",
         ylab = "Muestra",
         title = "Grafico Q-Q v/s distribucion normal")

# Calcular estimadores puntuales y estadisticos utiles.
mu_0 = 25
alfa <- 0.05

# Aplicar la prueba t de Student a la diferencia de medias.
t.test(diferencia ,
       alternative = "two.sided",
       mu = mu_0,
       conf.level = 1 - alfa)

# Otra alternativa puede ser aplicar la prueba t de Student para dos muestras 
# pareadas.
t.test(x = posdestete,
       y = lactancia,
       paired = TRUE ,
       alternative = "two.sided",
       mu = mu_0,
       conf.level = 1 - alfa)


#Conclusión:
# Al aplicar la prueba, se observa que el valor p obtenido es muy pequeño 
# 0.03631, menor a 0.05, por lo que es poco probable que la hipotesis nula
# sea cierta. Además, el intervalo de confianza es [31.41375-179.98625] por lo
# que la media 105.7 se encuentra en el intervalo de confianza. Finalmente se 
# rechaza H0 en favor de HA. Se puede afirmar con un 95% de confianza, que el 
# contenido total de minerales en los huesos del cuerpo durante el posdestete 
# excede el de la etapa de lactancia por más de 25 g.
  