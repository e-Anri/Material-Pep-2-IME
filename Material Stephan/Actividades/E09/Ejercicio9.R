
# EJERCICIO PRÁCTICO 9: TRANSFORMACIÓN DE DATOS Y MÉTODOS NO PARAMÉTRICOS

# Problema A
# En trabajo de título de un estudiante del DIINF, se reportan los siguientes 
# tiempos de ejecución (en milisegundos) medidos para dos versiones de un 
# algoritmo genético para resolver instancias del problema del vendedor viajero
# disponibles en repositorios públicos. 
# ¿Es uno de los algoritmos más rápido que el otro?

# H0: No hay diferencias significativas en los tiempos de ejecución de las dos 
#     versiones de un algoritmo genético.
# HA: Existen diferencias en los tiempos de ejecución de las dos versiones de 
#     un algoritmo genético.

# En resumen:

# H0: Tpo_A6 - Tpo_B12 = 0 
# HA: Tpo_A6 - Tpo_B12 != 0 

library (ggpubr)

texto <-("
Instancia 'Tiempo A6 (ms)' Instancia 'Tiempo B12 (ms)'
'fl1400' 337977 'd1291' 335566
'pcb1173' 303634 'd657' 52696
'rat575' 33349 'fl1577' 3192222
'rl1323' 243679 'nrw1379' 393213
'u1060' 3453176 'pr1002' 162808
'u1432' 398653 'pr2392' 8765321
'u1817' 876432 'rat783' 76857
'u2152' 3073534 'rl1304' 231254
'u574' 112326 'rl1889' 854213
'u724' 55026 'vm1084' 543215
")
datos <- read.table(textConnection(texto), header = TRUE)

# Para identificar la transformación
g1 <- gghistogram ( datos ,
                    x = "Tiempo.A6..ms." ,
                    bins = 10 ,
                    xlab = "Tiempo algoritmo A6" ,
                    ylab = "Instancia" ,
                    color = "red" ,
                    fill = "red" )
print(g1)

g2 <- ggscatter( datos ,
                 x = "Tiempo.A6..ms." ,
                 y = "Instancia",
                 xlab = "Tiempo algoritmo A6" ,
                 ylab = "Instancia" ,
                 color = "red" ,
                 fill = "red" )
print(g2)

colnames(datos)[1] <- "Instancia_A6"
colnames(datos)[2] <- "Tiempo_A6"
colnames(datos)[3] <- "Instancia_B12"
colnames(datos)[4] <- "Tiempo_B12"

# Aplicar transformación logarítmica
log_Tiempo_A6 <- log(datos$Tiempo_A6)
log_Tiempo_B12 <- log(datos$Tiempo_B12)
datos <- data.frame(datos, log_Tiempo_A6, log_Tiempo_B12)

# Histogramas para el tiempo de ejecución del algoritmo A6 antes y después de 
# la transformación logarítmica.
g3 <- gghistogram ( datos ,
                    x = "Tiempo_A6",
                    bins = 10,
                    xlab = "Tiempo ejecución algoritmo A6 [ms]",
                    ylab = "Frecuencia",
                    color = "red",
                    fill = "red" )

g4 <- gghistogram ( datos ,
                    x = "log_Tiempo_A6",
                    bins = 10,
                    xlab = "Tiempo ejecución algoritmo A6 [log(ms)] " ,
                    ylab = "Frecuencia",
                    color = "red",
                    fill = "red" )

# Crear una única figura con ambos histogramas .
histograma <- ggarrange (g3 , g4 , ncol = 2 , nrow = 1 )

titulo <- text_grob ("Efecto de la transformación logarítmica ",
                     face = "bold",
                     size = 14 )

histograma <- annotate_figure (histograma , top = titulo)
print(histograma)

# Histogramas para el tiempo de ejecución del algoritmo B12 antes y después de 
# la transformación logarítmica.
g5 <- gghistogram ( datos ,
                    x = "Tiempo_B12",
                    bins = 10,
                    xlab = "Tiempo ejecución algoritmo B12 [ms]",
                    ylab = "Frecuencia",
                    color = "blue",
                    fill = "blue" )

g6 <- gghistogram ( datos ,
                    x = "log_Tiempo_B12",
                    bins = 10,
                    xlab = "Tiempo ejecución algoritmo B12 [log(ms)] " ,
                    ylab = "Frecuencia",
                    color = "blue",
                    fill = "blue" )

# Crear una única figura con ambos histogramas .
histograma2 <- ggarrange (g5 , g6 , ncol = 2 , nrow = 1 )

titulo2 <- text_grob ("Efecto de la transformación logarítmica ",
                      face = "bold",
                      size = 14 )

histograma2 <- annotate_figure (histograma2 , top = titulo)
print(histograma2)

# Gráficos de dispersión para la relación entre peso corporal y peso del
# cerebro , antes y después de aplicar la transformación logarítmica .
g7 <- ggscatter(datos ,
                x = "Tiempo_A6" ,
                y = "Tiempo_B12" ,
                color = "red" ,
                xlab = "Tiempo ejecución algoritmo A6 [ms]" ,
                ylab = "Tiempo ejecución algoritmo B12 [ms]" ) + rotate_x_text(45)


g8 <- ggscatter(datos ,
                x = "log_Tiempo_A6" ,
                y = "log_Tiempo_B12" ,
                color = "red" ,
                xlab = "Tiempo ejecución algoritmo A6 [log(ms)]" ,
                ylab = "Tiempo ejecución algoritmo B12 [log(ms)]" ) + rotate_x_text(45)

# Crear una única figura con los gráficos de dispersión .
dispersion <- ggarrange (g7 , g8 , ncol = 2 , nrow = 1 )
texto <- " Relaci ó n entre el peso corporal y el peso del cerebro "

titulo <- text_grob (texto ,
                     face = "bold",
                     size = 14 )

dispersion <- annotate_figure ( dispersion , top = titulo )
print(dispersion)

# Se verifican los cambios en los valores p mediante shapiro test.
shapiro.test(datos$Tiempo_A6)
shapiro.test(datos$log_Tiempo_A6)

shapiro.test(datos$Tiempo_B12)
shapiro.test(datos$log_Tiempo_B12)

datostTest <- data.frame(datos[1], datos[5], datos[3], datos[6])

# Se calculan estimadores puntuales y estadisticos utiles
mu_0 = 0
alfa <- 0.05

# Se decide aplicar la prueba t Student a los valores luego de aplicar la 
# tranformacion logaritmica.
t.test(x = datostTest$log_Tiempo_A6,
       y = datostTest$log_Tiempo_B12,
       paired = TRUE,
       alternative = "two.sided",
       mu = mu_0,
       conf.level = 1-alfa)

# Respuesta Problema A:
# Como con la prueba t Student se obtiene un valor p = 0.764, mayor al nivel 
# de significacion igual a 0.05, se falla en rechazar la hipotesis nula y se 
# concluye con un 95% de confianza que no hay diferencias significativas en 
# los tiempos de ejecución de las dos versiones de un algoritmo genético para 
# resolver instancias del problema del vendedor viajero.

# Respondiendo a la interrogante ¿Es uno de los algoritmos más rápido que el 
# otro? No, se podría decir que no hay un algoritmo más rápido que otro, debido
# a que ambos tienen tiempos de ejecución similares a pesar de resolver 
# instancias distintas.


#_______________________________________________________________________________


# Problema C
# En trabajo de título de un estudiante del DIINF, se reportan los siguientes 
# tiempos de ejecución ('Tpo' en milisegundos) medidos para dos versiones de un
# algoritmo genético (A6 y B12) para resolver instancias del problema del 
# vendedor viajero disponibles en repositorios públicos. 
# ¿Es uno de los algoritmos más rápido que el otro?

# H0: No hay diferencias significativas en los tiempos de ejecución de las dos 
#     versiones de un algoritmo genético.
# HA: La version A6 tiene tiempos de ejecucion menores a la version B12.

# En resumen:

# H0: Tpo_A6 - Tpo_B12 = 0 
# HA: Tpo_A6 - Tpo_B12 < 0 


texto <-("
      Instancia 'Tpo_A6' 'Tpo_B12'
      'rat575' 33349 32444
      'u724' 55026 64019
      'd657' 43352 52696
      'rat783' 65076 76857
      'u574' 112326 123456
      'pr1002' 136262 162808
      'fl1577' 3234574 3192222
      'nrw1379' 335608 393213
      'd1291' 268964 335566
      'u1432' 398653 472597
      'pcb1173' 303634 234658
      'fl1400' 337977 430748
      'u2152' 3073534 3253423
      'rl1323' 243679 132654
      'rl1304' 342321 231254
      'u1817' 876432 672542
      'vm1084' 413672 543215
      'rl1889' 1876432 854213
      'pr2392' 6764986 8765321
      'u1060' 3453176 432876
      ")

datos <- read.table(textConnection(texto), header = TRUE)

# Establecer nivel de significacion.
alfa <- 0.05

# Hacer la prueba de rangos con signo de Wilcoxon.
prueba <- wilcox.test(datos$Tpo_A6, 
                      datos$Tpo_B12,
                      alternative = "less",
                      paired = TRUE ,
                      conf.level = 1 - alfa)
print(prueba)

# Respuesta Problema C
# Dados los resultados obtenidos con la prueba de de rangos con signo de 
# Wilcoxon, donde se obtiene p = 0.4347, mayor al nivel de significación 0.05
# por lo que se falla en rechachar la hipotesis nula y se concluye que con un 
# 95% de confianza que noo hay diferencias significativas en los tiempos de 
# ejecución de las dos versiones de un algoritmo genético.

# Respondiendo a la interrogante ¿Es uno de los algoritmos más rápido que el 
# otro? No, se podría decir que no hay un algoritmo más rápido que otro, debido
# a que ambos tienen tiempos de ejecución similares.


#_______________________________________________________________________________


# Problema F:
# El siguiente texto muestra porcentaje de acierto alcanzado por tres algoritmos
# de clasificacio??n en diferentes conjuntos de prueba disponibles en el UCI Machine
# Learning Repository. Los algoritmos corresponden a C3: averaged one- dependence 
# estimator (AODE), C6: locally weighted naive-Bayes y C7: random forest. 
# ¿Existe un algoritmo mejor o peor que los otros?

# H0: Los tres algoritmos tienen porcentajes de acierto similares.
# HA: Al menos un algoritmo tiene un porcentaje de acierto distinto a los demás.

texto <-("
      Dataset C3 C6 C7
      'credit' 85,07 85,22 83,33
      'eucalyptus' 58,71 59,52 59,40
      'glass' 73,83 75,69 73,33
      'hepatitis' 83,79 82,50 81,25
      'iris' 92,67 92,00 93,33
      'optdigits' 96,90 94,20 91,80
      'page-blocks' 96,95 94,15 96,97
      'pendigits' 97,82 94,81 95,67
      'pima-diabetes' 75,01 74,75 72,67
      'primary-tumor' 47,49 49,55 38,31
      'solar-flare-C' 88,54 87,92 86,05
      'solar-flare-m' 87,92 86,99 85,46
      'solar-flare-X' 97,84 94,41 95,99
      'sonar' 81,26 80,79 78,36
      'waveform' 84,92 83,62 79,68
      'yeast' 56,74 57,48 56,26
      ")
datos <- read.table(textConnection(texto), header = TRUE, dec = ",")

datos$Dataset <- factor(datos$Dataset)

Tiempo <- c(datos$C3, datos$C6, datos$C7)

Algoritmo <- rep(c("C3","C6","C7"), each=16)

datosFinal <- data.frame(Tiempo , datos$Dataset, Algoritmo)

colnames(datosFinal)[2] <- "Dataset"

# Establecer nivel de significacio??n
alfa <- 0.05

# Hacer la prueba de Friedman.
prueba <- friedman.test(Tiempo ~ Algoritmo | Dataset, data=datosFinal)
print(prueba)

# Efectuar procedimiento post-hoc de Holm si se encuentran diferencias 
# significativas.

if(prueba$p.value < alfa) {
  post_hoc <- pairwise.wilcox.test(datosFinal$Tiempo ,
                                   datosFinal$Algoritmo ,
                                   p.adjust.method = "holm", 
                                   paired = TRUE)
  print(post_hoc)
}

# Respuesta Problema F:
# Considerando un nivel de significación igual a 0,05, se rechaza la hipotesis 
# nula a favor de la alternativa. En consecuencia, se concluye con un 95% de 
# confianza que al menos un algoritmo tiene un porcentaje de acierto distinto a 
# los demas. Ademas, a traves de la correccion de Holm como analisis post-hoc, 
# se sabe que la diferencia significativa está dada por C3-C7.

# ¿Existe un algoritmo mejor o peor que los otros?
# Dando respuesta a la pregunta planteada, podemos decir que si hay un algoritmo
# mejor o peor que los demas.
