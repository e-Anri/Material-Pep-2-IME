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
# Considerando un nivel de significación ??=0,05, se rechaza la hipotesis nula a
# favor de la alternativa. En consecuencia, se concluye con un 95% de confianza 
# que al menos un algoritmo tiene un porcentaje de acierto distinto a los demas.
# Ademas, a traves de la correccion de Holm como analisis post-hoc, se sabe que 
# la diferencia significativa está dada por C3-C7.

# ¿Existe un algoritmo mejor o peor que los otros?
# Dando respuesta a la pregunta planteada, podemos decir que si hay un algoritmo
# mejor o peor que otros.
 

