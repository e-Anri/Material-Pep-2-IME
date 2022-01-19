require(ez)
library(ggpubr)
library(tidyr)
library(MASS)
library(car)


"Ejercicio Practico 9
"
# Resolucion Ejercicios B, C y F

"
Enunciado Problema B 

El siguiente texto muestra los resultados de clasificadores en diferentes instancias
de prueba disponibles en el repositorio UCI. Los algoritmos corresponden a C1: J48 
decision tree, C3: averaged one-dependence estimator (AODE), C5: J48 graft, C6: locally 
weighted naive-Bayes y C7: random forest. ?Existe un algoritmo, o un grupo de algoritmos, mejor que otro(s)?
"
cat("Ejercicio B de resultados de algoritmos de clasificacion\n")
texto <- ("
Dataset C1 C3 C5 C6 C7
'credit' 84,93 85,07 84,93 85,22 83,33
'eucalyptus' 64,28 58,71 64,01 59,52 59,40
'glass' 71,58 73,83 71,10 75,69 73,33
'hepatitis' 79,46 83,79 79,46 82,50 81,25
'hungarian-14' 78,64 84,39 78,64 84,38 81,97
'hypothyroid' 99,28 98,54 99,28 98,62 98,97
'iris' 93,33 92,67 93,33 92,00 93,33
'mushroom' 100,00 99,95 100,00 99,84 100,00
'optdigits' 78,97 96,90 81,01 94,20 91,80
'page-blocks' 96,62 96,95 96,66 94,15 96,97
'pendigits' 89,05 97,82 89,87 94,81 95,67
'pima-diabetes' 73,70 75,01 73,56 74,75 72,67
'primary-tumor' 40,11 47,49 40,11 49,55 38,31
'solar-flare-C' 88,86 88,54 88,86 87,92 86,05
'solar-flare-m' 90,10 87,92 90,10 86,99 85,46
'solar-flare-X' 97,84 97,84 97,84 94,41 95,99
'sonar' 74,48 81,26 74,45 80,79 78,36
'waveform' 74,38 84,92 74,90 83,62 79,68
'yeast' 57,01 56,74 57,01 57,48 56,26
")

# se puede observar que se tienen muestras correlacionadas(repetidas)
#OBTENER LOS DATOS EN FORMATO ANCHO
dw1 <- read.table(
  file= textConnection(texto), 
  header = TRUE,
  dec = ","
)


#OBTENER LOS DATOS EN FORMATO LARGO
dl1 <- gather(
  data = dw1,
  key = "Algoritmo",
  value = "Resultado",
  -"Dataset"
)

dl1[["Dataset"]] <- factor(dl1[["Dataset"]])
dl1[["Algoritmo"]] <- factor(dl1[["Algoritmo"]])

# Como aproximación se comparan los algoritmos con una gráfico de
# cajas.


p1 <- ggboxplot(
  dl1,
  x = "Algoritmo", y = "Resultado",
  xlab = "Algoritmos",
  color = "Algoritmo"
)
print(p1)

#En el gráfico se pueden apreciar unos valores atipicos con una asimetria
# en C3, C6 Y C7
# transformando a una escala abierta con la funcion logit

Logit.resultado <- logit(dl1[["Resultado"]], adjust = 0.01)
dl1 <- cbind(dl1, Logit.resultado)

#revisando los datos transformados
p2 <- ggboxplot(
  dl1,
  x = "Algoritmo", y = "Logit.resultado",
  xlab = "Algoritmos",
  color = "Algoritmo"
)
print(p2)

# se pueden apreciar que los datos se encuentran mas simetricos, a pesar que 
# en C1 y C5 se aprecian valores atipicos pero mas cercanos a los datos originales

#prueba de normalidad

st1 <- by(
  data = dl1[["Logit.resultado"]],
  INDICES = dl1[["Algoritmo"]],
  FUN = shapiro.test
)
cat("Prueba de normalidad para logit.resultado de c/resultado \n")
print(st1)

# con esto, se puede observar que se obtuvieron datos para poder trabajar con
# una prueba parametrica para un analisis de varianza con medidas repetidas

t1 <- ezANOVA(
  data = dl1,
  dv = Logit.resultado,
  wid = Dataset,
  within = Algoritmo,
  type = 3,
  return_aov = TRUE
)

cat("Tabla ANOVA para logit.resultado de c/algoritmo \n")
print(summary(t1[["aov"]]))


# se rechaza la hipotesis nula, por lo que los datos no cumplen con la condicions
# que los datos son esfericos, por lo que se debe aplicar un factor de correccion


"
Enunciado Problema C no parametrico

En trabajo de t?tulo de un estudiante del DIINF, se reportan los siguientes tiempos
de ejecuci?n ('Tpo' en milisegundos) medidos para dos versiones de un algoritmo gen?tico
(A6 y B12) para resolver instancias del problema del vendedor viajero disponibles en 
repositorios p?blicos. ?Es uno de los algoritmos m?s r?pido que el otro?
"

texto1 <-("
Instancia 'Tpo A6' 'Tpo B12'
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


#Accedemos y obtenemos los datos de la tabla en formato ancho

datos1 <- read.table(
  file = textConnection(texto1),
  header = TRUE,
  stringsAsFactors = FALSE
)
names(datos1) <- c("Instancia", "A6", "B12")

#Ahora pasamos los datos a un formato largo

dat1 <- gather(
  data = datos1,
  key = "Algaritmo",
  value = "Tiempo",
  -"Instancia"
)

dat1[["Instancia"]] <-factor(dat1[["Instancia"]])
dat1[["Algoritmo"]] <-factor(dat1[["Algoritmo"]])

#Se realiza un gistorgrama para poder cer de manera grafica los datos

h1 <- gghistogram(
  dat1, 
  x = "Tiempo",
  xlab = "Algoritmo",
  color = "Algoritmo", fill = "Algoritmo",
  bins = 5
)
h1 <- h1 + facet_grid(~ Algoritmo)
print(h1)

"Lo que se puede ver anteriormente es una distribuci?n de tipo exponencial
Se tienen presente mediciones de dos tipos de algoritmos, por lo cual debemos utilizar una
una prueba T-student, pero como se solicita la altenativa no parametrica se debe utilizar 
la prueba de los rangos con signo de Wilcoxon"

#Aplicando la prueba de los rangos con signo de wilcoxon

test1 <- wilcox.test(datos1[["A6"]], datos1[["B12"]], paired = TRUE)
test2 <- wilcox.test(Tiempo ~ Algoritmo, dat1, paired = TRUE)

cat("Prueba de rangos con signo de wilcoxon \n")
print(test2)
"
Las hipotesis son:
H0: Las diferencias existentes entre los pares de valores presentan una distribuci?n simetrica
    cuyo valor se centra en 0
H1: Las diferencia existente entre los pares de valores no presentan una distribuci?n simetrica

conclusion:
Al ejecutar la prueba de Wilcoxon, se obtiene un valor p elevado,debido a los cual no se puede
rechazar la hipotesis nula, es asi como la diferencia existente entre los pares de 
valores tiene una distribuci?n simetrica centrada en cero, en otras palabras no 
existe diferencias significativas entre los tiepos de ejecucion de los algoritmos "

"
Enunciado Problema F no parametrico

El siguiente texto muestra porcentaje de acierto alcanzado por tres algoritmos de
clasificaci?n en diferentes conjuntos de prueba disponibles en el UCI Machine Learning
Repository. Los algoritmos corresponden a C3: averaged onedependence estimator (AODE),
C6: locally weighted naive-Bayes y C7: random forest. ?Existe un algoritmo mejor o peor
que los otros?
"
texto2 <-("
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
cat("Ejercicio F de resultados de 3 algoritmos de clasificacion\n")
#Accedemos y obtenemos los datos de la tabla en formato ancho
dw3 <- read.table(
  file = textConnection(texto2),
  header = TRUE,
  dec = ","
)

# Llevamos los datos a formato largo
dl3 <- gather(
  data = dw3,
  key = "Algoritmo",
  value = "Acierto",-"Dataset"
)

dl3[["Dataset"]] <- factor(dl3[["Dataset"]])
dl3[["Algoritmo"]] <- factor(dl3[["Algoritmo"]])

#Revisamos los datos de manera grafica con un grafico de cajas
p5 <- ggboxplot(
  dl3,
  x = "Algoritmo", y = "Acierto",
  xlab = "Algoritmo",
  color = "Algoritmo",
  add = "jitter"
)
#Realizamos la prueba no parametrica de Friedman
t3 <- friedman.test(Acierto ~ Algoritmo | Dataset, dl3)

cat("Prueba de Friedman\n")
print(t3)


# Calculando los rankings, que esta vez se calculan por sujeto
dw3.rank <- t(apply(-dw3[, -1], 1, rank))
dw3.rank <- data.frame(Dataset = dw3[, 1], dw3.rank)

# Llevamos los datos a formato largo
dl3.rank <- gather(
  data = dw3.rank,
  key = "Algoritmo",
  value = "Acierto",
  -"Dataset"
)

dl3.rank[["Dataset"]] <- factor(dl3.rank[["Dataset"]])
dl3.rank[["Algoritmo"]] <- factor(dl3.rank[["Algoritmo"]])

# Revisemos los datos graficamente con un diagrama de cajas
p7 <- ggboxplot(
  dl3.rank,
  x = "Algoritmo", y = "Acierto",
  xlab = "Algoritmo",
  color = "Algoritmo",
  add = "jitter"
)

# Utilizamos la prueba pairwise-wilcox para muestras pareadas en este caso
ph2 <- pairwise.wilcox.test(dl3[["Acierto"]], dl3[["Algoritmo"]],
                            paired = TRUE,
                            p.adjust.method = "BH")


cat("Comparaciones entre pares de algoritmos (post-hoc)\n")
print(ph2)

"Al realizar este test nos muestra que hay diferencias significativas entre los 
algoritmos c3 y c7, en otras palabras, c7 obtiene un acierto inferior que el de c3, sin embargo el
algoritmo c6 se encuentra en una situcai?n muy cercana"








