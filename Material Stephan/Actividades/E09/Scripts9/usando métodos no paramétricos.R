library(ez)
library(ggpubr)
library(tidyr)

cat("\n\n")
cat("Enunciado A1\n")
cat("============\n")
cat("\n\n")

texto1 <-("
  Instancia   'Tpo A6'   'Tpo B12'
  'rat575'       33349       32444
  'u724'         55026       64019
  'd657'         43352       52696
  'rat783'       65076       76857
  'u574'        112326      123456
  'pr1002'      136262      162808
  'fl1577'     3234574     3192222
  'nrw1379'     335608      393213
  'd1291'       268964      335566
  'u1432'       398653      472597
  'pcb1173'     303634      234658
  'fl1400'      337977      430748
  'u2152'      3073534     3253423
  'rl1323'      243679      132654
  'rl1304'      342321      231254
  'u1817'       876432      672542
  'vm1084'      413672      543215
  'rl1889'     1876432      854213
  'pr2392'     6764986     8765321
  'u1060'      3453176      432876
")

# Obtenemos los datos en formato ancho
dw1 <- read.table(
  file = textConnection(texto1),
  header = TRUE,
  stringsAsFactors = FALSE
)
names(dw1) <- c("Instancia", "A6", "B12")

# Llevamos los datos a formato largo
dl1 <- gather(
  data = dw1,
  key = "Algoritmo",
  value = "Tiempo",
  -"Instancia"
)
dl1[["Instancia"]] <- factor(dl1[["Instancia"]])
dl1[["Algoritmo"]] <- factor(dl1[["Algoritmo"]])

# Revisemos un histograma de los datos
p1 <- gghistogram(
  dl1,
  x = "Tiempo",
  xlab = "Algoritmo",
  color = "Algoritmo", fill = "Algoritmo",
  bins = 5
)
p1 <- p1 + facet_grid(~ Algoritmo)

# Como es usual al trabajar con tiempos, el comportamiento no
# es el de una distribución normal, sino que de tipo exponencial.

# ¿Qué prueba paramétrica deberíamos usar en este caso?
# Tenemos mediciones de dos algoritmos sobre las mismas instancias
# de prueba, correspondiendo una prueba T de Student para datos
# apareados.
# ¿Qué alternativa no paramétrica corresponde?
# La prueba de los rangos con signo de Wilcoxon

# Revisemos las condiciones (sec. 12a VassarStats)
# 1. cada par de valores en las muestras es independiente de
#    todos los otros pares y escogidos aleatoriamente
# 2. la variable dependiente es continua
# 3. las mediciones tienen al menos escala ordinal (existe =, <, >)

# La 1era condición es un poco difícil. La independencia está clara,
# ya que el tiempo que los algoritmos tandan en resolver una instancia
# no influye de manera alguna en cuánto tardan en otra instancia, pero
# no es tan claro que las instancias fueron escogidas al azar.
# Más probable es que hayan sido seleccionadas como "representativas"
# de todas las instancias disponibles. Eso todavía estaría bien,
# puesto que es eso lo que intenta conseguir la elección aleatoria.

# La 2da se cumple, es tiempo en milisegundos

# La 3era tambien se cumple, puesto que se tardó más o se tardó menos
# o se tardó lo mismo son apreciaciones válidas con esta variable.

# Podemos seguir con la prueba
# (aunque hay dos formas)
t1 <- wilcox.test(dw1[["A6"]], dw1[["B12"]], paired = TRUE)
t2 <- wilcox.test(Tiempo ~ Algoritmo, dl1, paired = TRUE)

cat("\n\n")
cat("Prueba de ranking con signo de Wilcoxon\n")
cat("---------------------------------------\n")
print(t2)


# Las hipótesis contrastadas por esta prueba puede resumierse
# de forma muy genérica en:
# H0: las diferencias entre los pares de valores tienen una
#     distribución simétrica centrada en cero
# H1: las diferencias entre los pares de valores no siguen
#     una distribución simétrica centrada en cero
#     (con mediana en cero)

# Luego, en este caso la conclusión es que ambos algoritmos tienen
# más o menos los tiempos de ejecución al resolver instancias
# del problema del vendedor viajero.





cat("\n\n")
cat("\n\n")
cat("Enunciado B1\n")
cat("============\n")
cat("\n\n")


texto2 <- ("
   Dataset                    C2     Dataset                C4
   'anneal'                98,00     'cmc'               51,05
   'contact-lenses'        68,33     'credit'            86,23
   'ecoli'                 80,04     'grub-damage'       47,79
   'kr-s-kp'               92,46     'monks'             62,24
   'monks1'               100,00     'mushroom'          95,83
   'nursery'               94,28     'page-blocks'       93,51
   'pasture-production'    85,83     'postoperatie'      66,67
   'primary-tumor'         48,08     'segment'           91,30
   'solar-flare-C'         88,24     'soybean'           92,08
   'squash-stored'         58,00     'squash-unstored'   61,67
   'tae'                   44,38     'waveform'          79,86
   'white-clover'          79,29     --                  --
")

# Obtenemos los datos en formato ancho
dw2 <- read.table(
  file = textConnection(texto2),
  header = TRUE,
  dec = ",",                 # Números en español
  na.strings = "--",         # Hay datos perdidos
  stringsAsFactors = FALSE   # Mantener los strings
)

# Llevamos los datos a formato largo
Dataset <- c(dw2[[1]], dw2[[3]])
Acierto <- c(dw2[[2]], dw2[[4]])
Algoritmo <- c(rep("C2", nrow(dw2)), rep("C4", nrow(dw2)))
dl2 <- data.frame(Dataset, Algoritmo, Acierto)
dl2[["Dataset"]] <- factor(dl2[["Dataset"]])
dl2[["Algoritmo"]] <- factor(dl2[["Algoritmo"]])
# Quitamos el valor perdido
dl2 <- dl2[complete.cases(dl2), ]

# Revisemos los datos con un diagrama de cajas
p2 <- ggboxplot(
  dl2,
  x = "Algoritmo", y = "Acierto",
  xlab = "Algoritmo",
  color = "Algoritmo",
  add = "jitter"
)

# Los datos parecen tener un comportamiento más o menos normal,
# pero debemos recordar que estamos tratando con porcentajes.
# Si no fuera así, usaríamos una prueba T de Student para muestras
# independientes.

# ¿Qué alternativa no paramétrica corresponde?
# La prueba U de Mann-Whitney

# Revisemos las condiciones (sec. 11a VassarStats)
# 1. las muestras son independientes y aleatorias
# 2. la variable dependiente es continua
# 3. las mediciones tienen al menos escala ordinal (existe =, <, >)

# Nuevamente la 1era condición es difícil de cumplir.
# Si bien la independencia está clara (cómo un algoritmo clasifica
# en una instancia no influye en cómo clasifica en otra), no sabemos
# si las instancias fueron escogidas al azar.
# Supongamos que fue así, o que se eligieron como "representativas"
# de todas las instancias disponibles, ya que vienen de un estudio
# publicado en un jounal muy serio.
# [A Benavoli, G Corani, F Mangili (2016). Should we really use
#   post-hoc tests based on mean-ranks?. The Journal of Machine
#   Learning Research 17.1]

# La 2da se cumple, es porcentaje, por lo que en teoría podría
# tener un número arbitrario de decimales.

# La 3era tambien se cumple, puesto que un porcentaje puede ser
# igual, mayor o menor que otro porcentaje.

# Podemos seguir con la prueba
# (dos formas)
t3 <- wilcox.test(dw2[["C2"]], dw2[["C4"]][complete.cases(dw2)])
t4 <- wilcox.test(Acierto ~ Algoritmo, dl2)

cat("\n\n")
cat("Prueba U de Mann-Whitney\n")
cat("------------------------\n")
print(t4)

# Hay varias formas de expresar la hipótesis nula. Una de las más
# genéricas es:
# H0: las muestran vienen de la misma población
# H1: las poblaciones difieren al menos en una medida de tendencia
#     central

# Así, la conclusión es que ambos algoritmos tienen la misma
# distribución de porcentaje de aciertos al clasificar datasets
# en el repositorio UCI.





cat("\n\n")
cat("\n\n")
cat("Enunciado A2\n")
cat("============\n")
cat("\n\n")


texto3 <- ("
           Nueva Actual Control
           81 48 18
           32 31 49
           42 25 33
           62 22 19
           37 30 24
           44 30 17
           38 32 48
           47 15 22
           49 40 --
           41 -- --
")

# Obtenemos los datos en formato ancho
dw3 <- read.table(
  file = textConnection(texto3),
  header = TRUE,
  na.strings = "--"         # Hay datos perdidos
)

# Llevamos los datos a formato largo
dl <- gather(
  data = dw3,
  key = "Crema",
  value = "Manchas"
)
dl[["Crema"]] <- factor(dl[["Crema"]])
Sujeto <- factor(1:30)
dl3 <- data.frame(Sujeto, dl)
# Quitamos los valores perdidos
dl3 <- dl3[complete.cases(dl3), ]

# Revisemos los datos con un diagrama de cajas
p3 <- ggboxplot(
  dl3,
  x = "Crema", y = "Manchas",
  xlab = "Crema",
  color = "Crema",
  add = "jitter"
)

# Los datos de control y de la nueva crema parecen mostrar
# asimetrías importantes. Además, hay presencia de valores
# atípicos en todos ellos.
# Si no hubiera sospecha con los datos, usaríamos un
# procedimiento ANOVA para muestras independientes.

# ¿Qué alternativa no paramétrica corresponde?
# La prueba de Kruskal-Wallis

# Revisemos las condiciones, que son similares a las condiciones
# del la prueba U de Mann-Whitney:
# 1. las muestras son independientes y aleatorias
# 2. la variable dependiente es continua
# 3. las mediciones tienen al menos escala ordinal (existe =, <, >)

# La descripción del experimento en el enunciado indica que los
# voluntarios fueron separados de forma aleatoria e independiente.
# Podríamos dar esta condición como cumplida.

# La segunda es un poco más compleja, aunque en teoría, una crema
# podría eliminar 2.5 manchas en promedio, aunque los datos individuales
# parecen estar redondeados a números enteros.

# La condición 3 se cumple evidentemente.

# En R, podemos usar la función kruskal.test()

t5 <- kruskal.test(Manchas  ~ Crema, dl3)

cat("\n\n")
cat("Prueba de Kruskal-Wallis\n")
cat("------------------------\n")
print(t5)

# Según la lectura:
# H0: el ranking promedio de las muestran es el mismo
# H1: dos o más muestras tienen ranking promedio distintos

# Usando un nivel de significación igual a ?? = ,05, la conclusión
# sería que al menos dos muestras tienen ranking distintos, y
# habría que hacer un análisis post-hoc para determinar dónde
# están estas diferencias. Esto no ocurriría si usáramos un
# ?? = ,01! ya que estaríamos en el borde.

# Grafiquemos estos rankings
Ranking <- rank(dl3[["Manchas"]])
dl3 <- data.frame(dl3, Ranking)

p4 <- ggboxplot(
  dl3,
  x = "Crema", y = "Ranking",
  xlab = "Crema",
  color = "Crema",
  add = "jitter"
)

# Lo lógico es que apliquemos un análisis post-hoc basado en
# un método no paramétrico. Eso descarta, por ejemplo, tanto
# pruebas T de Student como el ajuste para comparaciones múltiples
# de Tukey, ya que ambas asumen normalidad de los datos.

ph1 <- pairwise.wilcox.test(dl3[["Manchas"]], dl3[["Crema"]],
                            p.adjust.method = "BH")

cat("\n\n")
cat("Post-hoc entre pares de cremas\n")
cat("------------------------------\n")
print(ph1)

# Vemos que hay una diferencia significativa entre la nueva
# crema y la crema actual (p = ,016) y en el borde con la
# crema humectante (p = ,049).

# También hay una corriente que pantea que es mejor hacer ANOVA
# (y post-hoc) directamente sobre los datos rankeados, si es que
# estos cumplen las condiciones, aunque esto sería otro método
# basado en la transformación de los datos problemáticos.
# Solo como ejemplo:

t6 <- by(dl3[["Ranking"]], dl3[["Crema"]], shapiro.test)
t7 <- ezANOVA(
  data = dl3,
  dv = Ranking,
  wid = Sujeto,
  between = Crema,
  type = 3,
  return_aov = TRUE
)
p5 <- ezPlot(
  data = dl3,
  dv = Ranking,
  wid = Sujeto,
  between = Crema,
  type = 3,
  x = Crema
)
ph2 <- pairwise.t.test(dl3[["Ranking"]], dl3[["Crema"]], "BH")





cat("\n\n")
cat("\n\n")
cat("Enunciado B2\n")
cat("============\n")
cat("\n\n")


texto4 <- ("
   Dataset               C3        C6        C7
   'credit'           85,07     85,22     83,33
   'eucalyptus'       58,71     59,52     59,40
   'glass'            73,83     75,69     73,33
   'hepatitis'        83,79     82,50     81,25
   'iris'             92,67     92,00     93,33
   'optdigits'        96,90     94,20     91,80
   'page-blocks'      96,95     94,15     96,97
   'pendigits'        97,82     94,81     95,67
   'pima-diabetes'    75,01     74,75     72,67
   'primary-tumor'    47,49     49,55     38,31
   'solar-flare-C'    88,54     87,92     86,05
   'solar-flare-m'    87,92     86,99     85,46
   'solar-flare-X'    97,84     94,41     95,99
   'sonar'            81,26     80,79     78,36
   'waveform'         84,92     83,62     79,68
   'yeast'            56,74     57,48     56,26
")

# Obtenemos los datos en formato ancho
dw4 <- read.table(
  file = textConnection(texto4),
  header = TRUE,
  dec = ","
)

# Llevamos los datos a formato largo
dl4 <- gather(
  data = dw4,
  key = "Algoritmo",
  value = "Acierto",
  -"Dataset"
)
dl4[["Dataset"]] <- factor(dl4[["Dataset"]])
dl4[["Algoritmo"]] <- factor(dl4[["Algoritmo"]])

# Revisemos los datos con un diagrama de cajas
p6 <- ggboxplot(
  dl4,
  x = "Algoritmo", y = "Acierto",
  xlab = "Algoritmo",
  color = "Algoritmo",
  add = "jitter"
)

# Observamos asimetrías evidentes, que nos hace sospechar sobre
# la naturaleza 'normal' de los datos.
# Si no fuera así, usaríamos un análisis de varianza con medidas
# repetidas.

# ¿Qué alternativa no paramétrica corresponde?
# La prueba de Friedman

# Revisemos las condiciones, que no son muchas:
# 1. el grupo medido fue elegido de forma aleatoria
# 2. la variable dependiente es continua o tiene una escala ordinal

# Ya hemos discutido que que la 1era condición es un poco difícil de
# asegurar y que lo más probable es que los 'datasets' hayan sido
# seleccionadas como "representativos" de todas las conjuntos
# disponibles. Si bien esto no es aleatorio en estricto rigor,
# podemos suponer que no nos dará problemas si fue hecho honestamente.

# La condición 2 se cumple evidentemente, puesto 90% acierto es mayor
# que 80% y menor a 95%.

# Podemos usar una función de R para aplicar la prueba

t8 <- friedman.test(Acierto ~ Algoritmo | Dataset, dl4)

cat("\n\n")
cat("Prueba de Friedman\n")
cat("------------------\n")
print(t8)


# La hipótesis nula de la prueba de Friedman puede describirse
# como:
# H0: el ranking promedio de las diferentes mediciones son las
#     mismas
# H1: dos o más medidas tienen ranking promedio distintos

# Usando un nivel de significación igual a ?? = ,05, la conclusión
# sería que al menos dos mediciones tienen ranking promedio
# distintos, y habría que hacer un análisis post-hoc.
# Coincidentemente, otra vez estaríamos en el borde si usáramos
# un ?? = ,01!

# Calculemos los rankings, que esta vez se calculan por sujeto...
dw4.rank <- t(apply(-dw4[, -1], 1, rank))
dw4.rank <- data.frame(Dataset = dw4[, 1], dw4.rank)

# Llevamos los datos a formato largo
dl4.rank <- gather(
  data = dw4.rank,
  key = "Algoritmo",
  value = "Acierto",
  -"Dataset"
)
dl4.rank[["Dataset"]] <- factor(dl4.rank[["Dataset"]])
dl4.rank[["Algoritmo"]] <- factor(dl4.rank[["Algoritmo"]])

# Revisemos los datos con un diagrama de cajas
p7 <- ggboxplot(
  dl4.rank,
  x = "Algoritmo", y = "Acierto",
  xlab = "Algoritmo",
  color = "Algoritmo",
  add = "jitter"
)

# Vemos que, en este caso, la asimetría empeora! Por lo que, tal vez,
# deberíamos descartar el uso de ANOVA RM.

# Apliquemos post-hoc no paramétrico
ph2 <- pairwise.wilcox.test(dl4[["Acierto"]], dl4[["Algoritmo"]],
                            paired = TRUE,
                            p.adjust.method = "BH")

cat("\n\n")
cat("Comparaciones entre pares de algoritmos (post-hoc)\n")
cat("--------------------------------------------------\n")
print(ph2)

# Tenemos que concluir que solo hay una diferencia significativa
# entre el algoritmo C3 y C7, es decir, el algoritmo C7 obtiene,
# en general, peor acierto que el algoritmo C3 (en menos ocasiones).
# Aunque debemos notar que las diferencias con el algoritmo C6 está
# en la condición de borde.