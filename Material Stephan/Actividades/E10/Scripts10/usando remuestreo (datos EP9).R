library(boot)
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

# Definimos una función que calcule el estadístico
# de interes, en este caso la media de las diferencias
# entre los pares de tiempos de ejecución.

mean.diff.rm <- function(x, i)
{
  datos <- x[i, 2:3]
  diffs <- datos[[2]] - datos[[1]]
  mean(diffs)
}

# Hacemos remuestreo con la técnica de bootstrapping
n.boot <- 9999
set.seed(13*17)
bt1 <- boot(data = dw1, statistic = mean.diff.rm, R = n.boot)

cat("\n\n")
cat("Remuestreo (boot) para media de las diferencias\n")
cat("-----------------------------------------------\n")
print(bt1)

# La función boot() retorna un objeto con muchas cosas...
# En lo básico:
# Si theta es el parámetro poblaciónal que queremos conocer,
# en este caso la media de las diferencia de pares, y T
# es el estadístico que estima theta, entonces:
# bt1[["t0"]] contiene el valor de T en la muestra original
#             (los datos en dw1)
# bt1[["t"]] contiene estimaciones de T obtenidas desde la
#            muestra original con R remuestreos
#
# Así, existe una analogía entre la relación bt1[["t"]] con
# bt1[["t0"]] y la ya conocida relación entre la distribución
# muestral (con realizaciones de T) y el parámetro theta.
#
# Si bien nos interesa (T - theta), es decir qué tan buena es la
# estimación que proporciona T de theta, solo tenemos la estimación
# (bt1[["t"]] - bt1[["t0"]]).
#
# Luego si:
T0 <- mean(bt1[["t"]])
# es decir, la media de la distribución bt1[["t"]], entonces
B0 <- T0 - bt1[["t0"]]
# sería el sesgo observado y una estimación de (T - theta).
# Similarmente,
Tse <- sd(bt1[["t"]])
# es una estimación del error estándar de T.

# Con esto podemos estimar el primer intervalo de confianza
# para theta, usando lo que ya conocemos (del método de Wald),
# aunque "corrigiendo" el sesgo detectado:
?? <- 0.05
p.lower <- ?? / 2
p.upper <- 1 - ?? / 2

ci.normal <- (bt1[["t0"]] - B0) +
  c(-1, 1) * qnorm(p.upper) * Tse

# Otra opción es tomar directamente los valores en la
# distribución de remuestreo que corresponden a estos
# percentiles:

i.lower <- round((n.boot + 1) * p.lower)
i.upper <- round((n.boot + 1) * p.upper)
st <- sort(bt1[["t"]])

ci.percentile <- c(st[i.lower], st[i.upper])

# Otra idea es usar este margen de error corrigiendo el
# sesgo:

delta1 <- T0 - ci.percentile[1]
delta2 <- ci.percentile[2] - T0

ci.basic <- c(bt1[["t0"]] - B0 - delta2,
              bt1[["t0"]] - B0 + delta1)
# Este intervalo de confianza se conoce como "básico" y
# como "empírico".


# Existen varios otras propuestas para calcular intervalos
# de confianza. Hay métodos bastante sofisticados como los
# descritos en un artículo de T. DiCiccio y B. Efron
# [Statistical Science 1996, Vol. 11, No. 3, 189-228]:
# Bootstrap-t, BCa y ABC.
# El primero un remuestreo del estadísticos t de Student para
# obtener los percentiles del intervalo de confianza; el
# segundo estima, de forma numérica, un factor de corrección
# que enmienda los límites del intervalo percentil descrito
# considerando tanto el sesgo como la asimetría de la
# distribución bt1[["t"]];
# la tercera es una usa una aproximación analítica al factor
# de corrección del método BCa.

# Es más, este método Bca se comporta bastante bien, en
# especial cuando la distribución del remuestreo es asimétrica,
# y puede obtenerse, junto a los métodos mostrados arriba,
# con la siguiente llamada a la función boot.ci:

ci1 <- boot.ci(
  boot.out = bt1,
  type = c("norm", "basic", "perc", "bca")
)

cat("\n")
cat("Intervalos de confianza\n")
cat("-----------------------\n")
print(ci1)

# Claramente no hay diferencias significativas, ya que el
# cero (no diferencia) es parte de todos los intervalos de
# confianza obtenidos.

# Pero ¿cómo obtenemos un p-valor?
# Primero necesitamos la distribución de las diferencias
# suponiendo que H0 es verdadera, es decir, cuando está
# centrada en cero:

bt1.H0 <- bt1[["t"]] - T0

# Y obtenemos cuántas veces se tienen valores menores
# o mayores que el estadístico inicial:
n.out1 <- sum(abs(bt1.H0) > abs(bt1[["t0"]]))
p.val1 <- (n.out1 + 1) / (n.boot + 1)

cat("\n")
cat("P-valor:", round(p.val1, 3), "\n")

# Este valor confirma que es bastante probable (p = .808) que
# la diferencia en tiempo de ejecución que observamos en la
# muestra (original) se deba al azar (a las instancias
# elegidas para el experimento) y no a una verdadera
# diferencia entre los algoritmos.







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

# Llevamos los datos a vectores
Dataset.1 <- dw2[complete.cases(dw2[[1]]), 1]
Dataset.2 <- dw2[complete.cases(dw2[[3]]), 3]
Dataset <- factor(c(Dataset.1, Dataset.2))

Acierto.1 <- dw2[complete.cases(dw2[[2]]), 2]
Acierto.2 <- dw2[complete.cases(dw2[[4]]), 4]
Acierto <- c(Acierto.1, Acierto.2)

n.1 <- length(Acierto.1)
stopifnot(n.1 == length(Dataset.1))
n.2 <- length(Acierto.2)
stopifnot(n.2 == length(Dataset.2))

n <- n.1 + n.2

# Definimos una función que calcule el estadístico
# de interes, en este caso la diferencia de las medias
# del acierto de cada algoritmo, a partir de una
# combinación de índices para el segundo grupo.

mean.diff.indep <- function(i)
{
  mean(Acierto[i]) - mean(Acierto[-i])
}

# Así, la diferencia en la muestra original, usando esta
# función, sería:

dif.obs <- mean.diff.indep((n.1 + 1):n)

# Hacemos remuestreo, esta vez calculando la diferencia
# de las medias de todas las combinaciones de n.2 aciertos
# que se pueden tomar.
# Es decir, usando la técnica de permutación.

set.seed(13*17)
perm <- combn(1:n, n.2, mean.diff.indep)

n.perm <- length(perm)
n.perm.str <- format(n.perm, big.mark = ".", decimal.mark = ",")
tit2 <- paste(n.perm.str, "permutaciones de aciertos")
cat("\n\n")
cat("Remuestreo (perm) para la diferencia de las medias\n")
cat("--------------------------------------------------\n")
print(tit2)
print(summary(perm))


# Podemos hacer un gráfico de la situación
p1 <- gghistogram(
  data.frame(Diferencia = perm),
  x = "Diferencia",
  xlab = "Diferencia en acierto medio",
  ylab = "Frecuencia",
  title = tit2,
  bins = 30
)
p1 <- p1 + geom_vline(
  xintercept = dif.obs,
  linetype = "solid", color = "blue"
)

# Vemos que, nuevamente, la diferencia observada en la muestra
# original no parece estar muy lejos del centro de la
# distribución remuestreada, por lo que la diferencia entre
# los algoritmos parece no deberse al azar.

# Obtengamos un valor p

n.out2 <- sum(abs(perm) > abs(dif.obs))
p.val2 <- (n.out2 + 1) / (n.perm + 1)    # Incluimos la observada

# Y un intervalo de confianza (95%):
?? <- 0.05
p.lower <- ?? / 2
p.upper <- 1 - ?? / 2
i.lower <- round((n.perm + 1) * p.lower)
i.upper <- round((n.perm + 1) * p.upper)

sp <- sort(perm)
ci.perm2 <- c(sp[i.lower], sp[i.upper])
ci.perm2 <- round(ci.perm2, 3)

cat("\n")
cat("P-valor:", round(p.val2, 3), "\n")
cat("95% IC: [", ci.perm2[1], ", ", ci.perm2[2], "]\n", sep = "")

# Vemos que tanto el p-valor como el intervalo de
# confianza confirmar la información gráfica.





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
Nueva <- dw3[complete.cases(dw3[[1]]), 1]
Actual <- dw3[complete.cases(dw3[[2]]), 2]
Control <- dw3[complete.cases(dw3[[3]]), 3]
Manchas <- c(Nueva, Actual, Control)

n.nueva <- length(Nueva)
n.actual <- length(Actual)
n.control <- length(Control)
n <- n.nueva + n.actual + n.control

voluntarios <- 1:n
Voluntarios <- factor(voluntarios)

# Puede ser difícil usar remuestreo basado en una
# permutación rigurosa, ya que el número de alternativas
# se eleva a:
# choose(27, 10) * choose(17, 9) = 205.086.088.350

# De todas formas podemos usar "muchas" permutaciones.
n.perms = 1999

# En este caso, se tienen 3 grupos... pero un factor:
Crema <- factor(c(
  rep("nueva", n.nueva),
  rep("actual", n.actual),
  rep("control", n.control)
))

# ¿Pero ¿qué estadístico remuestreamos ahora?
# Tenemos tres grupos, por lo que deberíamos pensar en
# un análisis de varianza.

datos.orig <- data.frame(Voluntarios, Manchas, Crema)
aov.orig <- suppressWarnings( suppressMessages(
  ezANOVA(
    data = datos.orig,
    dv = Manchas,
    wid = Voluntarios,
    between = Crema,
    type = 3,
    return_aov = TRUE
  ))
  # Se suprimen los warnings y mensajes que produce la función
  # ezANOVA() porque estamos usando muestras con diferentes tamaños.
)

# Sin embargo, solo nos interesa el estadístico F que indica
# la razón entre las varianzas entre e intra grupos.

F.orig <- aov.orig[["ANOVA"]][["F"]]

# Y entonces tenemos que construir una función que permita obtener
# este estadístico de las permutaciones.

get.F <- function()
{
  # Obtenemos una permutación (usando los índices)
  i <- sample(voluntarios)
  
  # Obtenemos los datos permutados
  d <- data.frame(Voluntarios, Manchas = Manchas[i], Crema)
  
  # Aplicamos ANOVA
  ezaov <- suppressWarnings( suppressMessages(
    ezANOVA(
      data = d,
      dv = Manchas,
      wid = Voluntarios,
      between = Crema,
      type = 3
    )))
  
  # Devolvemos el estadístico F
  ezaov[["ANOVA"]][["F"]]
}

# Hacemos el remuestreo usando la técnica de permutación,
# aunque no todas las alternativas posibles e incluso con
# la posibilidad de tener algunas repetidas.

set.seed(13*17)
F.perm <- replicate(n.perms, get.F())

n.perms.str <- format(n.perms, big.mark = ".", decimal.mark = ",")
tit3 <- paste(n.perms.str, "permutaciones del n° manchas eliminadas")
cat("\n\n")
cat("Remuestreo (perm) de la razón de varianzas inter/intra grupos \n")
cat("--------------------------------------------------------------\n")
print(tit3)
print(summary(F.perm))

# Nuevamente, hacemos un gráfico de la situación
p3 <- gghistogram(
  data = data.frame(F = c(F.orig, F.perm)),
  x = "F",
  xlab = "Estadístico F",
  ylab = "Frecuencia",
  title = tit3,
  bins = 30
)
p3 <- p3 + geom_vline(
  xintercept = F.orig,
  linetype = "solid", color = "blue"
)

# Vemos que la razón de varianzas observada en la muestra
# original aparece muy lejos del centro de la distribución
# remuestreada, por lo que pareciera haber diferencias
# significativas entre la cantidad media de manchas eliminadas
# por las diferentes cremas.

# Obtengamos un p-valor

n.out3 <- sum(F.perm > F.orig)
p.val3 <- (n.out3 + 1) / (n.perms + 1)    # Incluimos la observada

cat("\n")
cat("P-valor:", round(p.val3, 3), "\n")

# Vemos que el p-valor confirma que hay pocas probabilidades
# que el estadístico observado (> 6) se deba al azar (la
# elección de las personas en cada grupo).


# Debemos hacer un análisis post-hoc con remuestreo !!!!

mean.diff.indep3 <- function(x, i, c1, c2)
{
  mean(x[i, c1], na.rm = TRUE) - mean(x[i, c2], na.rm = TRUE)
}

n.boot3 = n.perms
set.seed(13*17)
bt3.1 <- boot(data = dw3, statistic = mean.diff.indep3, R = n.boot3,
              c1 = 1, c2 = 2)
bt3.2 <- boot(data = dw3, statistic = mean.diff.indep3, R = n.boot3,
              c1 = 1, c2 = 3)
bt3.3 <- boot(data = dw3, statistic = mean.diff.indep3, R = n.boot3,
              c1 = 2, c2 = 3)

# Obtenemos las distribuciones bajo H0
bt3.1.H0 <- bt3.1[["t"]] - mean(bt3.1[["t"]])
bt3.2.H0 <- bt3.2[["t"]] - mean(bt3.2[["t"]])
bt3.3.H0 <- bt3.3[["t"]] - mean(bt3.3[["t"]])

# Obtenemos las diferencias originales
orig3.1 <- mean.diff.indep3(dw3, 1:nrow(dw3), 1, 2)
orig3.2 <- mean.diff.indep3(dw3, 1:nrow(dw3), 1, 3)
orig3.3 <- mean.diff.indep3(dw3, 1:nrow(dw3), 2, 3)

# Y obtenemos los p-valores
n.out3.1 <- sum(abs(bt3.1.H0) > abs(orig3.1))
p.val3.1 <- (n.out3.1 + 1) / (n.boot3 + 1)

n.out3.2 <- sum(abs(bt3.2.H0) > abs(orig3.2))
p.val3.2 <- (n.out3.2 + 1) / (n.boot3 + 1)

n.out3.3 <- sum(abs(bt3.3.H0) > abs(orig3.3))
p.val3.3 <- (n.out3.3 + 1) / (n.boot3 + 1)

n.boot3.str <- format(n.boot3, big.mark = ".", decimal.mark = ",")
tit3.1 <- paste(n.boot3.str,
                " remuestras (boot) de la diferencia\n",
                "de las medias del nº de manchas eliminadas",
                sep = ""
)

cat("\n\n")
cat("Análisis post-hoc (boot) para la diferencia de las medias\n")
cat("---------------------------------------------------------\n")
cat(tit3.1, "\n")
cat("P-valores:\n")
cat(sprintf("%7s - %7s: %.3f\n", colnames(dw3)[1], colnames(dw3[2]), p.val3.1))
cat(sprintf("%7s - %7s: %.3f\n", colnames(dw3)[1], colnames(dw3[3]), p.val3.2))
cat(sprintf("%7s - %7s: %.3f\n", colnames(dw3)[2], colnames(dw3[3]), p.val3.3))

# Luego, la nueva crema elimina en promedio (47.3) significativamente más
# manchas que la crema actualmente comercializada (30.33, p < .001) y que
# una crema humectante (28.8, p = .018).






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

# Aquí tenemos medidas repetidas para 16 conjuntos de datos
# (datasets) del repositorio. Luego, habría que remuestrear
# "trios" de valores, es decir, las tres mediciones de un
# sujeto están o no en la remuestra.

# Definimos una función que calcula el estadístico de
# interes, en este caso la rzón entre las varianzas entre
# los grupos y al interior de los grupos, es decir, F.

get.F.rm <- function(x, i)
{
  # Seleccionamos los sujetos
  dw <- x[i, ]
  dw[["Dataset"]] <- 1:nrow(dw)  # procuramos id's distintos
  
  # Llevamos los datos a formato largo
  dl <- gather(
    data = dw,
    key = "Algoritmo",
    value = "Acierto",
    -"Dataset"
  )
  dl[["Dataset"]] <- factor(dl[["Dataset"]])
  dl[["Algoritmo"]] <- factor(dl[["Algoritmo"]])
  
  # Aplicamos ANIVA RM
  ezaov <- ezANOVA(
    data = dl,
    dv = Acierto,
    wid = Dataset,
    within = Algoritmo,
    type = 3
  )
  
  # Devolvemos el valor F
  ezaov[["ANOVA"]][["F"]]
}

# Obtenemos el estadístico observado en la muestra original

Frm.orig <- get.F.rm(dw4, 1:nrow(dw4))

# Sin embargo, esta función no estima F cuando H0 es cierta,
# ni es claro cómo conseguir la correspondiente a H0 a partir
# de la que esta genera, por lo que no tiene utilidad.
# ¿Qué hacemos? Recordar que bajo H0, las medias de las medidas
# repetidas son iguales.
# Luego, debemos "mover" las medias a un valor común, lo que se
# consigue "centrando" cada medición alrededor del cero:

medias <- apply(dw4[, -1], 2, mean)
dw5 <- data.frame(
  Dataset = dw4[["Dataset"]],
  C3 = dw4[["C3"]] - medias[1],
  C6 = dw4[["C6"]] - medias[2],
  C7 = dw4[["C7"]] - medias[3]
)

# En estos datos, bajo H0, las columnan tienen la misma media
# (cero) pero mantienen las varianzas originales intactas.
# Hacemos remuestreo con la técnica de bootstrapping usando
# estos datos.

n.boot4 <- 1999

set.seed(13*17)
bt4 <- boot(data = dw5, statistic = get.F.rm, R = n.boot4)


n.boot4.str <- format(n.boot4, big.mark = ".", decimal.mark = ",")
tit4 <- paste(n.boot4.str, "remuestras (boot) del acierto")

cat("\n\n")
cat("Remuestreo (boot) para F en medidas repetidas recentradas\n")
cat("---------------------------------------------------------\n")
print(tit3)
print(bt4)

# Nuevamente, hacemos un gráfico de la situación
p4 <- gghistogram(
  data = data.frame(F = c(Frm.orig, bt4[["t"]])),
  x = "F",
  xlab = "Estadístico F",
  ylab = "Frecuencia",
  title = tit4,
  bins = 30
)
p4 <- p4 + geom_vline(
  xintercept = F.orig,
  linetype = "solid", color = "blue"
)

# Obtengamos un p-valor

n.out4 <- sum(bt4[["t"]]  > Frm.orig)
p.val4 <- (n.out4 + 1) / (n.boot4 + 1)

cat("\n")
cat("P-valor:", round(p.val4, 3), "\n")


# La evidencia soporta la idea de rechazar H0 con nivel de
# significación 0.05 (pero no con nivel 0.01).


# Nuevamente debemos hacer un análisis post-hoc con remuestreo !!!!

mean.diff.rm4 <- function(x, i, c1, c2) mean(x[i, c1] - x[i, c2])

set.seed(13*17)
bt4.1 <- boot(data = dw4, statistic = mean.diff.rm4, R = n.boot4,
              c1 = 2, c2 = 3)
bt4.2 <- boot(data = dw4, statistic = mean.diff.rm4, R = n.boot4,
              c1 = 2, c2 = 4)
bt4.3 <- boot(data = dw4, statistic = mean.diff.rm4, R = n.boot4,
              c1 = 3, c2 = 4)

# Obtenemos las distribuciones bajo H0
bt4.1.H0 <- bt4.1[["t"]] - mean(bt4.1[["t"]])
bt4.2.H0 <- bt4.2[["t"]] - mean(bt4.2[["t"]])
bt4.3.H0 <- bt4.3[["t"]] - mean(bt4.3[["t"]])

# Obtenemos las diferencias originales
orig4.1 <- mean.diff.rm4(dw4, 1:nrow(dw4), 2, 3)
orig4.2 <- mean.diff.rm4(dw4, 1:nrow(dw4), 2, 4)
orig4.3 <- mean.diff.rm4(dw4, 1:nrow(dw4), 3, 4)

# Y obtenemos los p-valores
n.out4.1 <- sum(abs(bt4.1.H0) > abs(orig4.1))
p.val4.1 <- (n.out4.1 + 1) / (n.boot4 + 1)

n.out4.2 <- sum(abs(bt4.2.H0) > abs(orig4.2))
p.val4.2 <- (n.out4.2 + 1) / (n.boot4 + 1)

n.out4.3 <- sum(abs(bt4.3.H0) > abs(orig4.3))
p.val4.3 <- (n.out4.3 + 1) / (n.boot4 + 1)

tit4.1 <- paste(n.boot4.str,
                " remuestras (boot) de las medias de las diferencias",
                " en acierto",
                sep = ""
)

cat("\n\n")
cat("Análisis post-hoc (boot) para la diferencia de las medias\n")
cat("---------------------------------------------------------\n")
cat(tit4.1, "\n")
cat("P-valores:\n")
cat(sprintf("%5s - %2s: %.3f\n", colnames(dw4)[2], colnames(dw4[3]), p.val4.1))
cat(sprintf("%5s - %2s: %.3f\n", colnames(dw4)[2], colnames(dw4[4]), p.val4.2))
cat(sprintf("%5s - %2s: %.3f\n", colnames(dw4)[3], colnames(dw4[4]), p.val4.3))

# Luego, los algoritmos C3 (81.6, p = .001) y C6 (80.9, p = .035) obtienen
# niveles de aciertos promedios significativamente mayores que el algoritmo
# C7 (79.2) (al nivel 0.05).
# Podemos ver que este es, probablemente, un ejemplo en que existen
# diferencias estadísticamente diferentes, pero que la diferencia en práctica
# (tamaño del efecto) no es tan importante.