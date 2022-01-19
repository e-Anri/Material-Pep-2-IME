library(WRS2)

# library(ez)
# library(ggpubr)
# library(tidyr)

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

# En el paquete WRS2 existe una alternativa robusta
# para analizar la diferencia de dos muestras apareadas
# usando medias podadas:

t1.1 <- yuend(dw1[["A6"]], dw1[["B12"]], tr = 0.2)

cat("\n\n")
cat("Prueba de Yuen con medias podadas para muestras correlacionadas\n")
cat("---------------------------------------------------------------\n")
print(t1.1)

# Otra alternativa en este paquete es comparar diferentes
# percentiles en la distribución remuestreada (boot)

t1.2 <- Dqcomhd(dw1[["A6"]], dw1[["B12"]],
                q = c(1:9) / 10,
                nboot = 1000
)

cat("\n\n")
cat("Comparación percentiles para muestras correlacionadas\n")
cat("-----------------------------------------------------\n")
print(t1.2)

# En ambos casos, no se observan diferencias significativas
# entre los tiempos mostrados por ambos algoritmos.





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

# En este caso, podemos aplicar las mismas pruebas
# que en el caso anterior, pero diseñadas para muestras
# independientes.

t2.1 <- yuen(Acierto ~ Algoritmo, dl2, tr = 0.2)

cat("\n\n")
cat("Prueba de Yuen con medias podadas para muestras independientes\n")
cat("--------------------------------------------------------------\n")
print(t2.1)

# Aquí hay una opción con remuestreo:

t2.2 <- yuenbt(formula = Acierto ~ Algoritmo,
               data = dl2,
               tr = 0.2, nboot = 2000
)

cat("\n\n")
cat("Prueba de Yuen remuestrada (boot) para muestras independientes\n")
cat("--------------------------------------------------------------\n")
print(t2.2)

# Y también existe la posibilidad de comparar diferentes
# percentiles en la distribución remuestreada (boot)

t2.3 <- qcomhd(formula = Acierto ~ Algoritmo,
               data = dl2,
               q = c(1:9) / 10,
               nboot = 2000,
               alpha = 0.05, ADJ.CI = TRUE
)

cat("\n\n")
cat("Comparación percentiles para muestras independientes\n")
cat("----------------------------------------------------\n")
print(t2.3)

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

# En WRS2 existe una prueba que permite analizar datos complicados
# con un símil al ANOVA (de una vía)

t3.1 <- t1way(formula = Manchas ~ Crema, data = dl3)

cat("\n\n")
cat("ANOVA de una vía con medias podadas\n")
cat("-----------------------------------\n")
print(t3.1)

# Parece haber un efecto significativo con 95% confianza.

# Hacemos entonces un análisis post-hoc robusto

ph3.1 <- lincon(formula = Manchas ~ Crema, data = dl3)

cat("\n\n")
cat("Análisis post-hoc con medias podadas\n")
cat("------------------------------------\n")
print(ph3.1)

# Este análisis nos indica que la crema nueva es mejor
# eliminando manchas que la crema actual (p = .005) y que
# una crema humectante (p = .049), aunque la evidencia es
# débil para este último caso.





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

# WRS2 también proporciona una prueba para analizar datos complicados
# equivalente a un ANOVA para medidas repetidas (de una vía).

t4.1 <- rmanova(
  y = dl4[["Acierto"]],
  groups = dl4[["Algoritmo"]],
  blocks = dl4[["Dataset"]]
)

cat("\n\n")
cat("ANOVA de una vía para medidas repetidas podadas\n")
cat("-----------------------------------------------\n")
print(t4.1)

# Hay un claro efecto significativo (p = .001)

# Hacemos entonces un análisis post-hoc robusto

ph4.1 <- rmmcp(
  y = dl4[["Acierto"]],
  groups = dl4[["Algoritmo"]],
  blocks = dl4[["Dataset"]]
)

cat("\n\n")
cat("Análisis post-hoc con medidas repetidas podadas\n")
cat("-----------------------------------------------\n")
print(ph4.1)

# Así, solo el algoritmo C3 consigue mejores niveles de acierto que
# el algoritmo C7 (p = .017).