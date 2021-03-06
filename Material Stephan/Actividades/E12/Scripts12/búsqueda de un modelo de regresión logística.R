# ��� ADVERTENCIA !!!

# Una parte del algoritmo generador de n�meros pseudo
# aleatorios fue cambiado desde la versi�n 3.6.0 de R.
# Este script fue hecho con una versi�n anterior, por lo
# que es necesario ejecutar la siguiente l�nea antes de
# fijar una semilla para obtener la misma secuencia que
# en la versi�n descrita:
RNGkind(sample.kind = "Rounding")

# Si no se ejecuta esta l�nea, se ver�n datos/observaciones
# distintas a las que aparec�an en los comentarios de este script.
# Puede hacer eso para ejercitar con regresi�n log�stica m�ltiple !

# Vimos que con regresi�n m�ltiple, uno pod�a hacer predecciones
# suponiendo la existencia de un modelo con la forma:
#
#     Y = (??0 + ??1�mu_Y|X1  + ??2�mu_Y|X2  + ... + ??k�mu_Y|Xk) + error
#
# que se estimaba consiguiendo aproximaciones a los par�metros ??:
#
#     Yi_est = b0 + b1 � X1i + b2 � X2i + ... + bk � Xki
#
# minimizando sum[(Yi - Yi_est)^2].
#
# Tambi�n mencionamos que esta generalizaci�n tambi�n es v�lida con la
# regresi�n log�stica binaria, cuando la variable dependiente representa
# dos posibles categor�as (o clases; es decir un factor con dos niveles).
# Esto es extremadamente com�n en las aplicaciones diarias:
# cliente satisfecho o no, paciente tiene c�ncer o no, solicitud deber�a
# aceptarse o no, etc.
#
# Vimos que construir este tipo de modelo usa la "funci�n log�stica",
# que aparece en diversos modelos de crecimiento sigmoidal de una
# magnitud (por ejemplo epidemias, ventas de productos novedosos,
# viralizaci�n en redes sociales, poblaciones de microorganismos y
# mam�feros marinos).
# Su forma general es:
#
#      f(z) = 1 / (1 + exp(-z))
#
# Pero recordemos que nuestra variable de salida (dependiente) es
# categ�rica con dos valores (de ah�, binaria).
# Por eso su nombre invita al error: si bien se llama "regresi�n", la
# verdad es que el modelo es de clasificaci�n.
# �C�mo podemos relacionar clases con una funci�n sigmoide?
# Usando nuevamente un ejemplo con estudio: "supongamos" que las horas
# de estudio semanal tiene alguna relaci�n con las probabilidades de
# aprobar una asignatura. As�:
#
#    horas estudio semanal   P(aprobar)   Predicci�n
#    ---------------------   ----------   ----------
#                      0.0   .017         R
#                      0.1   .019         R
#                      0.5   .035         R
#                      1.0   .071         R
#                      2.0   .256         R
#                      3.0   .607         A
#                      4.0   .874         A
#                      5.0   .969         A
#                      6.0   .993         A
#                      7.0   .998         A
#
# Luego es la "probabilidad de aprobar" la que parte muy baja, y a medida
# que crece el n�mero de horas estudio, esta probabilidad aumenta hasta
# que pasa el umbral 0.5 (cambiando la predicci�n) y sigue aumentando,
# acerc�ndose asint�ticamente a 1.
#
# Si hablamos de clase "positiva" y "negativa", podemos pensar que la
# probabilidad asociada a la clase positiva sigue una funci�n log�stica
# de una combinaci�n lineal de las variables predictoras.
#
#     P(Y = +) =                            1
#                -----------------------------------------------------
#                1 + exp(-(b0 + b1 � X1i + b2 � X2i + ... + bk � Xki))
#
#
# Este es el modelo que se construye en la regresi�n log�stica binaria.
# Notemos que hay una regresi�n lineal multivariable aqu� para poder
# obtener los coeficientes b0, b1, b2, ..., bk.
#

# Veamos otro ejemplo, volviendo a las medidas del cuerpo humano que
# hemos estado usando (body.csv).

# Indicar directorio
dir <- "~/../Downloads"

# Estos datos son medidas del cuerpo humano.
# Est� descrito en el paquete "gclus" y en
# Heinz et al. (2003). Exploring Relationships in Body Dimensions
# Journal of Statistics Education 11(2)
# Disponible en ww2.amstat.org/publications/jse/v11n2/datasets.heinz.html

basename <- "body.csv"
file <- file.path(dir, basename)

datos.todos <- read.csv(
  file = file,
  sep = ""
)
# print(str(datos.todos))

# Tomamos la misma muestra con igual n�mero de varones y mujeres
i <- which(datos.todos[["Gender"]] == 0)
j <- which(datos.todos[["Gender"]] == 1)
set.seed(13)
N <- 15
i <- sample(i, N)
j <- sample(j, N)
datos.wide <- datos.todos[c(i, j), ]

# Creemos una variable binaria en nuestro ejemplo para probar estas, y
# otras, ideas. Usemos "peso normal" o "sobrepeso".

BMI <- round(datos.wide[["Weight"]] / (datos.wide[["Height"]] / 100)^2, 1)
categor�a <- rep("Normal", length(BMI))
categor�a[BMI >= 25] <- "Sobrepeso"

# En el futuro (otro curso) se ver�n m�todos para abordar clasificaci�n
# con m�s de dos clases. Por ejemplo, basado en el �ndice de masa
# corporal tambi�n se puede identificar:
# categor�a[BMI < 18.5] <- "Bajopeso"
# categor�a[BMI >= 30] <- "Obesidad"
# categor�a[BMI >= 40] <- "Obesidad m�rbida"

# Llevamos los datos a formato largo para su an�lisis
library(tidyr)
datos.long <- gather(datos.wide, key = "Medida", value = "Valor")
datos.long[["Medida"]] <- factor(datos.long[["Medida"]])

# Agregamos la clase: Normal o Sobrepeso
datos.long[["Categor�a"]] <- factor(rep(categor�a, ncol(datos.wide)))
datos.wide[["Categor�a"]] <- factor(categor�a)

# Algunos gr�ficos. �Podemos decir a priori qu� variables nos ayudar�an
# a clasificar a una persona sin conocer su BMI?

library(ggpubr)
pb <- ggboxplot(
  data = datos.long,
  x = "Categor�a", y = "Valor",
  color = "Medida", legend = "none"
)
pbf <- facet(pb, facet.by = "Medida", scales = "free")
print(pbf)

# cat("\n\n");stop("*** SIN ERROR ***")


# Para obtener un modelo de regresi�n log�stica confiable, se deben
# cumplir varias supocisiones/condiciones.
# Varias de ellas se comparte con regresi�n m�ltiple:
#
# 1) Linealidad
#
# A diferencia del caso de regresi�n lineal, no es posible establecer
# una relaci�n lineal entre la variable de salida y las variables
# predictoras. Pero al suponer que la probabilidad de una de las dos
# clases tiene un crecimiento log�stico, se est� suponiendo que el
# logaritmo de la "oportunidad" de la clase tiene una relaci�n lineal
# con las variables predictoras.
# "Oportunidad" aqu� es una posible traducci�n de la palabra "odds" del
# ingl�s, que a veces se traduce como "chance", "momios",
# "posibilidades", "cuota" y otros m�s (incluso , "probabilidades" !!).
# Sin embargo, en estad�stica tiene una definici�n matem�tica clara:
#
#     Oportunidad = p / (1 - p)
#
# Es decir, la raz�n entre la probabilidad de que ocurra un evento y la
# probabilidad de que no ocurra el evento. Luego, si p es P(Y = +):
#
# log(p / (1 - p)) = b0 + b1 � X1i + b2 � X2i + ... + bk � Xki
#
# El cumplimiento de la suposici�n de linealidad puede verificarse
# revisando si existe interacci�n (significativa) entre los predictores
# usados en un modelo y su transformaci�n logar�tmica.
#
#
# 2) Independencia del error (residuales)
#
# Al igual que en regresi�n lineal, se asume que cada caso en los datos
# no influye en otros casos.
# Notemos que esta condici�n es violada con medidas repetidas, para las
# cuales se requiere otro tipo de m�todo.
#
#

# Situaciones problem�ticas
# -------------------------
#
# Hay muchas situaciones problem�ticas para poder obtener matem�tica-
# mente un modelo de regresi�n log�stica. En estos casos, R t�picamente
# reclama que el m�todo de los m�nimos cuadrados no converge.
#
# 1) Multicolinealidad
#
# Al igual que en regresi�n multivariada, correlaciones importantes
# entre predictores puede traernos problemas, ya que aumenta el error
# est�ndar de los coeficientes de regresi�n b, aumentando la incerti-
# dumbre de las estimaciones.
# Y al igual que en la regresi�n multivariada, podemos revisar esta
# seudo-condici�n por medio del factor de inflaci�n de varianza (VIF),
# que indica si un predictor tiene una relaci�n lineal fuerte con 
# otro(s) predictor(es), y el relacionado estad�stico tolerancia
# (1 / VIF), o m�todos m�s sofisticados.
# Seg�n Field et al. (2012), VIF > 10 para una variable predictora
# podr�a ser preocupante. Tambi�n un valor de VIF medio > 1 es 
# indicaci�n que la multicolinealidad podr�a estar sesgando el modelo.
# Tambi�n un valor de tolerancia < 0.1 es preocupante, aunque valores
# menores a 0.2 deber�an revisarse tambi�n.
# [Field, A., Miles, J., & Field, Z. (2012). Discovering statistics
# using R. Sage publications.]
#
#
# 2) Informaci�n incompleta
#
# Obviamente es dif�cil tener *varios* representantes de cada posible
# combinaci�n de predictores, en especial con variables categ�ricas.
# Las pruebas ??^2 que se usan con la regresi�n log�stica, por
# ejemplo, asume un valor m�nimo de 1 en cada celda y no m�s de 20% de
# celdas con menos de 5 casos.
# Por ejemplo, revisemos qu� pasa con nuestra variable de g�nero:

library(gmodels)
CrossTable(
  datos.wide[["Gender"]], datos.wide[["Categor�a"]],
  format = "SPSS", expected = TRUE
)

# 3) Separaci�n perfecta
#
# Esto ocurre cuando una variable separa completamente a las clases,
# no hay traslape. Revisemos nuevamente el gr�fico.


# cat("\n\n");stop("*** SIN ERROR ***")


# Vamos a crear nuestro primer modelo, que incluye a todas las variables
# (por eso "~ .")
modelo <- glm(
  Categor�a ~ .,
  family = binomial(link = "logit"),
  data = datos.wide
)
print(summary(modelo))


# cat("\n\n");stop("*** SIN ERROR ***")


# Fij�monos en el "warning" que arroja R
# �Qu� paso?
#
#
# Bueno, estamos incluyendo Estatura y Peso, las variables que usamos
# para calcular BMI, lo que separa perfectamente las categor�as !!!

d <- datos.wide[, c("Height", "Weight", "Categor�a")]

intercept <- -68.5
slope <- 0.83
y <- intercept + slope * d[["Height"]]

d <- cbind(d, y)

pp <- ggscatter(
  data = d,
  x = "Height", y = "Weight",
  color = "Categor�a", legend = "none"
)
ppl <- pp + geom_line(
  aes(x = Height, y = y),
  colour = "#3C5488B2", size = 1.5
)
print(ppl)

# cat("\n\n");stop("*** SIN ERROR ***")


# Tenemos el clasificador perfecto !!
# Fome: No vamos a considerar estas variables en nuestro modelo, porque
# son las variables que est�n detr�s de la clasificaci�n que queremos
# predecir. No hay misterior aqu�. Obviamente, esto no ocurre en la
# pr�ctica.

# Mirando a los gr�ficos, las variables m�s �tiles (menos traslape de
# clases) parec�an ser (en orden, al ojo):
#   Waist.Girth (grosor de la cintura),
#   Chest.Girth (grosor del pecho),
#   Hip.Girth (grosor de la cadera),
#   Calf.Maximum.Girth (maximo grosor de la pantorrilla),
#   Navel.Girth (grosor a la altura del ombligo),
#   Shoulder.Girth (grosor del hombro),
#   ...
#
#
# Probemos con la primera variable predictora, grosor de la cintura
modelo <- glm(
  Categor�a ~ Waist.Girth,
  family = binomial(link = "logit"),
  data = datos.wide)
print(summary(modelo))


# cat("\n\n");stop("*** SIN ERROR ***")


# R reporta el estad�stico "desviaci�n" (deviance), tambi�n referido
# como -2LL, porque corresponde a -2�log-likelihood(modelo), esto es el
# logaritmo natural de la "funci�n de verosimilitud" de los par�metros
# del modelo y el conjunto de datos.
# El estad�stico desviaci�n sigue una distribuci�n ??^2, lo que permite
# calcular la significanci�n estad�stica de su valor (p-valor).
# "Null deviance" corresponde a la desviaci�n del modelo nulo, es decir
# el que solo considera un "intercept". La diferencia de esta desviaci�n
# con la de nuestro modelo es una indicaci�n de su calidad. Esta
# diferencia tambi�n sigue una ??^2.
# De hecho, varias alternativas para obtener una medida an�loga a R^2
# para regresi�n log�stica se han propuesto en base a estos estad�sticos,
# pero todav�a no hay uno aceptado ampliamente.
#
# Por eso AIC y BIC suelen ser m�s usadas.
#
# Debemos notar que, como para regresi�n lineal m�ltiple, R nos entrega
# una medida de la contribuci�n de cada predictor, aunque en este caso
# se usa la distribuci�n normal (en vez de t).
#
# �Ser� este el mejor modelo?
#
# No es rid�culo pensar que el grosor de la cadera pueda ser un buen
# predictor, sobretodo en mujeres. Podemos entonces agregar esta
# variable. Hacemos esto, actualizando la f�rmula del modelo:

nuevo <- update(modelo, . ~ . + Hip.Girth)
print(summary(nuevo))

# Vemos que se redujo tanto la desviaci�n como el AIC, por lo que el
# nuevo modelo predice mejor que el primero.
#
# Para los modelos de regresi�n lineal m�ltiple se pod�an comparar con
# una prueba F y obtener una tabla ANOVA con ellos. Para regresi�n
# log�stica, podemos hacer algo similar, aunque la prueba F no es
# apropiada, por lo que tenemos que usar una alternativa.
# Una muy usada es el Likelihood Ratio Test (LRT), que compara qu� tanto
# m�s "probable" son los datos con un modelo que con el otro.

cat("\n\n")
cat("Likelihood Ratio Test para los modelos\n")
cat("--------------------------------------\n")
print(anova(modelo, nuevo, test = "LRT"))


# cat("\n\n");stop("*** SIN ERROR ***")


# Luego, nos quedamos con el nuevo modelo:
modelo <- nuevo

# �Ser� este el mejor modelo?
#
# Tampoco es absurdo pensar que el grosor a la altura del ombligo pueda
# ser un buen predictor, sobretodo en hombres.
# Agreguemos esta variable entonces.

nuevo <- update(modelo, . ~ . + Navel.Girth)
print(summary(nuevo))

cat("Likelihood Ratio Test para el nuevo modelo\n")
cat("------------------------------------------\n")
print(anova(modelo, nuevo, test = "LRT"))

# Vemos que, si bien baja la desviaci�n, el AIC aument� un poco.
# Es m�s, la nueva variable no aparece impactando significativamente en
# el modelo.
# Luego, parece mejor mantener el modelo anterior, con dos variables
# (m�s simple).


# cat("\n\n");stop("*** SIN ERROR ***")


# Pero quedan muchas otras variables a probar. As�, para ampliar el
# modelo, podr�amos calcular todos los modelos posibles al incluir otra
# de las variable regresoras todav�a disponibles, compararlos con el
# original, y elegir aquel que "mejora m�s", tomando como referencia,
# por ejemplo, el �ndice AIC.
#
# Hacer esto es bastante tentador, puesto que R proporciona la funci�n
# add1() que lo hace muy simple. Tambi�n hay varias opciones de
# comparaci�n (test).
#
# De hecho podemos partir del modelo nulo y dejar que R elija por
# nosotros.
#
nulo <- glm(
  Categor�a ~ 1,
  family = binomial(link = "logit"),
  data = datos.wide
)
subc <- subset(datos.wide, select = -c(Categor�a, Height, Weight))
variables <- names(subc)
posibles <- add1(nulo, scope = variables, direction = "forward",
                 trace = 1, test = "LRT")

# Mostramos la lista de modelos de una variable ordenado por AIC (de
# menor a mayor) y el estad�stico LRT en caso de empate (de mayor a
# menor, i.e. de m�s a menos significativo).
orden <- order(posibles[["AIC"]], posibles[["LRT"]], decreasing = c(F, T))
cat("\n\n")
cat("Aporte al agregar una variable predictora al modelo nulo\n")
cat("--------------------------------------------------------\n")
print(posibles[orden, ])


# cat("\n\n");stop("*** SIN ERROR ***")


# Podemos ver el mejor modelo, con estos criterios, es que incluye el
# grosor de la cadera como variable predictora.
# Actualicemos nuestro modelo y repitamos la b�squeda.
modelo1 <- update(nulo, . ~ . + Hip.Girth)
variables <- variables[variables != "Hip.Girth"]
posibles <- add1(modelo1, scope = variables, direction = "forward",
                 trace = 1, test = "LRT")
orden <- order(posibles[["AIC"]], posibles[["LRT"]], decreasing = c(F, T))
cat("\n\n")
cat("Aporte al agregar una variable predictora al primer modelo\n")
cat("----------------------------------------------------------\n")
print(posibles[orden, ])


# cat("\n\n");stop("*** SIN ERROR ***")


# Vemos que ahora conviene agregar la variable grosor del pecho
modelo2 <- update(modelo1, . ~ . + Chest.Girth)


# Y as� podemos seguir, eligiendo y agregando variables hasta que no
# encontremos alguna variable que mejore el modelo.
#
# Pero sabemos quer R nos permite hacer esto mientras vamos a jugar con
# la Play! (eliminando primero las variables problem�ticas)
datos <- subset(datos.wide, select = -c(Height, Weight))
total <- glm(
  Categor�a ~ .,
  family = binomial(link = "logit"),
  data = datos
)
# Notemos que nuevamente hay problemas al usar todas las variables
# �Separaci�n perfecta con otra combinaci�n?

cat("\n\n")
cat("=============================================================\n")
cat("Agregando variables al modelo con selecci�n hacia adelante...\n")
cat("=============================================================\n")
cat("\n\n")
auto1 <- step(nulo, scope = list(lower = nulo, upper = total),
              direction = "forward", test = "LRT", trace = 1)

cat("\n\n")
cat("=============================================================\n")
cat("\n\n")
cat("Modelo obtenido autom�ticamente con selecci�n hacia adelante\n")
cat("------------------------------------------------------------\n")
print(auto1)

# cat("\n\n");stop("*** SIN ERROR ***")


# PERO DEBEMOS NOTAR, como dijimos anteriormente con la regresi�n lineal
# m�ltiple, QUE ESTO LO *PODEMOS* HACER *EN ESTE CASO* porque *NO*
# estamos tratando de *PROBAR* una teor�a, sino que estamos simplemente
# explorando los datos. Solo en estos casos, podemos darnos la LIBERTAD
# de usar una b�squeda step-wise (o de otro tipo).
#

# Otra cosa: esta es un proceso de OPTIMIZACI�N, que usa AIC como la
# funci�n a optimizar. Este step-wise no es otra cosa que un algoritmo
# goloso (greedy) para encontrar la mejor soluci�n.
# Luego, no necesariamente obtendremos el modelo �ptimo.
#
# �R puede buscar el �ptimo!
# Pero obviamente, eso toma m�s tiempo y es factible con conjuntos de
# datos de tama�os moderado. Ya vimos una alternativa.


# cat("\n\n");stop("*** SIN ERROR ***")


# Al igual que con los modelos de regresi�n multivariada, es importante
# revisar la calidad del modelo y de los datos que se usaron.

# Primero podemos ve rificar que el modelo no est� sobre influenciado por
# un n�mero peque�o de casos at�picos (outliers). Este se hace midiendo
# la influencia de los casos a traves de "estad�sticos de influencia"
# (influential statistics), que se presentan a continuaci�n.
# [A Field, J Miles, Z Field (2012), Discovering Statistics using R,
# secci�n 7.7.1.2]
#

modelo <- auto1
variables <- names(coef(modelo))[-1]

# Obtengamos solo los datos en el modelo
subdatos <- datos[, c(variables, "Categor�a")]

# Obtengamos los residuales y las estad�sticas
output <- data.frame(predicted.probabilities = fitted(modelo))
output[["standardized.residuals"]] <- rstandard(modelo)
output[["studentized.residuals"]] <- rstudent(modelo)
output[["cooks.distance"]] <- cooks.distance(modelo)
output[["dfbeta"]] <- dfbeta(modelo)
output[["dffit"]] <- dffits(modelo)
output[["leverage"]] <- hatvalues(modelo)

cat("\n\n")
cat("Estad�sticas de influencia para los casos\n")
cat("=========================================\n")

# Revisemos los residuales
# plot(modelo)

# Sabemos que 95% de los residuales estandarizados deber�an estar entre
# ???1.96 y +1.96, y que el 99% entre -2.58 y +2.58. Revisemos eso.
sospechosos1 <- which(abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1)
cat("\n\n")
cat("Residuales estandarizados fuera del 95% esperado\n")
cat("------------------------------------------------\n")
print(rownames(subdatos[sospechosos1, ]))

# Recomendaciones dicen deber�amos preocuparnos por los casos en que la
# distancia de Cook es mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat("\n\n")
cat("Residuales con una distancia de Cook alta\n")
cat("-----------------------------------------\n")
print(rownames(subdatos[sospechosos2, ]))

# Tambi�n se recomienda revisar casos cuyo "leverage" sea m�s del doble
# o triple del leverage promedio: (k + 1)/n
leverage.promedio <- ncol(subdatos) / nrow(datos)
sospechosos3 <- which(output[["leverage"]] > leverage.promedio)
sospechosos3 <- sort(sospechosos3)
cat("\n\n")
cat("Residuales con levarage fuera de rango (> ")
cat(round(leverage.promedio, 3), ")", "\n", sep = "")
cat("--------------------------------------\n")
print(rownames(subdatos[sospechosos3, ]))

# Tambi�n podr�amos revisar DFBeta, que deber�a ser < 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1, 1, any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4) <- NULL
cat("\n\n")
cat("Residuales con DFBeta sobre 1\n")
cat("-----------------------------\n")
print(rownames(subdatos[sospechosos4, ]))

# Puede verse que los casos sospechosos m�s o menos se repiten con cada
# estad�stica. Revisemos estos casos.

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4)
sospechosos <- sort(unique(sospechosos))
cat("\n\n")
cat("Casos sospechosos\n")
cat("-----------------\n")
print(subdatos[sospechosos, ])
cat("\n\n")
print(summary(subdatos[sospechosos, ]))
cat("\n\n")
print(output[sospechosos, ])

# Hay varias razones para preocuparse con estos casos (adem�s de los
# warnings() de R). Sus valores de Leverage son enormes, y hay un caso
# con distancia de Cook realmente mayor a 1. Tal vez este nos debiera
# preocupar y evaluar la eliminaci�n de estos casos del conjunto de
# datos para el an�lisis.


# cat("\n\n");stop("*** SIN ERROR ***")


# Tambi�n hab�amos dicho que, una vez que tuvieramos el modelo, deb�amos
# revisar si se cumpl�an las suposiciones detr�s de la regresi�n log�s-
# tica (o m�ltiple).

# 1) Linealidad
#
# Dijimos que esta condici�n puede verificarse revisando si existe
# interacci�n significativa entre los predictores usados en un modelo
# y su transformaci�n logar�tmica.
# [DW Hosmer, S Lemeshow (2000). Applied logistic regression. 2nd Ed.
# John Wiley & Sons,  Inc.]
#
# Debemos crear las interacciones de cada variable con su logaritmo.
datos.log <- subdatos[, 1:3] * log(subdatos[, 1:3])
colnames(datos.log) <- paste("log_int", colnames(subdatos)[1:3], sep = ".")
datos.log <- cbind(subdatos[, 1:3], datos.log, Categor�a = subdatos[[4]])
modelo.log <- glm(
  Categor�a ~ .,
  family = binomial(link = "logit"),
  data = datos.log
)
cat("\n\n")
cat("Modelo con interacciones logar�tmicas\n")
cat("-------------------------------------\n")
print(summary(modelo.log))


# cat("\n\n");stop("*** SIN ERROR ***")


# � Ni siquiera converge ! Parece haber un problema serio...


# 2) Independencia del error (residuales)
#
# Es se traduce a que no ha de existir autocorrelaci�n en los t�rminos
# residuales. Esto puede probarse con una prueba estad�stica espec�fica
# conocida con el nombre de sus autores: Durbin-Watson test, que
# verifica si dos residuales adyacentes (un retardo) est�n
# correlacionados.
# [J Durbin, GS Watson (1950). Testing for Serial Correlation in Least
# Squares Regression, I". Biometrika. 37(3-4):409-428;
# Durbin, GS Watson (1951). Testing for Serial Correlation in Least
# Squares Regression, II". Biometrika. 38(1-2):159-179]

# En R, es f�cil revisar m�s retardos, por ejemplo hasta 5 retardos:
library(car)
cat("\n\n")
cat("Prueba de Durbin-Watson para autocorrelaciones entre errores\n")
cat("------------------------------------------------------------\n")
print(durbinWatsonTest(modelo, max.lag = 5))


# cat("\n\n");stop("*** SIN ERROR ***")


# A diferencia de la mayor�a de las pruebas de hip�tesis, la Prueba de
# Durbin-Watson define tres regiones: rechazo de H0, no rechazo de H0 y
# una regi�n no concluyente, por lo que existen dos valores cr�ticos:
# dU (d-upper) and dL (d-lower), que deben buscarse en tablas publicadas.
# [D Halcoussis (2005). Understanding econometrics. South-Western.]
# La funci�n de R que hemos usado los calcula con bootstrapping.

# En este caso, el test indica autocorrelaciones significativas.
# As�, debemos dudar del modelo obtenido.
#
# En todo caso, debe tenerse cuidado con la Prueba de Durbin-Watson,
# puesto que depende del orden de los datos, por lo que reordenando
# se podr�a tener valores distintos. Aunque en este caso, es poco
# probable que cambie la conclusi�n de que existe autocorrelaci�n.
#


# cat("\n\n");stop("*** SIN ERROR ***")



# Situaciones problem�ticas: Multicolinealidad
# --------------------------------------------
#
# Dejamos pendiente la revisi�n de la multicolinealidad, que puede
# estropear las predicciones del modelo.
#
# Dijimos que podemos revisar esta seudo-condici�n por medio del factor
# de inflaci�n de varianza (VIF) y el estad�stico tolerancia (1 / VIF).
# Aunque no hay un acuerdo general, el valor VIF >= 10 se usa como
# umbral para preocuparse. Tambi�n se ha encontrado que si el VIF
# promedio es mayor a 1, podr�a haber sesgo en el modelo.
# En el caso de la tolerancia, se ha sugerido que valores bajo 0.2
# podr�an ser problem�ticos. Aunque algunos acad�micos creen que valores
# cercanos a 0.4 deber�an ser revisados.

vifs <- vif(modelo)
cat("\n")
cat("Factores de inflaci�n de la varianza\n")
cat("------------------------------------\n")
print(round(vifs, 1))

cat("\n")
cat("Factor de inflaci�n de la varianza medio\n")
cat("----------------------------------------\n")
print(round(mean(vifs), 1))

tols <- 1/vifs
cat("\n")
cat("Tolerancia\n")
cat("----------\n")
print(round(tols, 2))


# !!! Horrible !!!
# Esta es una confirmaci�n de que nuestro modelo tiene problemas.
# Probablemente, existe mucha relaci�n entre las variables incluidas
# en el modelo.
# �Qu� tal si eliminamos los tobillos?

reducido <- update(modelo, . ~ . - Ankles.diameter)


vifs <- vif(reducido)
cat("\n\n")
cat("Factores de inflaci�n de la varianza del modelo reducido (multicol.)\n")
cat("--------------------------------------------------------------------\n")
print(round(vifs, 1))

cat("\n\n")
cat("Prueba de Durbin-Watson para el modelo reducido (residuos indep.)\n")
cat("-----------------------------------------------------------------\n")
print(durbinWatsonTest(reducido, max.lag = 5))


cat("\n\n")
cat("Modelo reducido con interacciones logar�tmicas (linealidad)\n")
cat("-----------------------------------------------------------\n")
variables2 <- names(coef(reducido))[-1]
subdatos2 <- datos[, c(variables2, "Categor�a")]
datos.log2 <- subdatos2[, 1:2] * log(subdatos2[, 1:2])
colnames(datos.log2) <- paste("log_int", colnames(subdatos2)[1:2], sep = ".")
datos.log2 <- cbind(subdatos2[, 1:2], datos.log2, Categor�a = subdatos2[[3]])
modelo.log2 <- glm(
  Categor�a ~ .,
  family = binomial(link = "logit"),
  data = datos.log2
)
print(summary(modelo.log2))

# Mucho mejor... al borrar esa variable (Ankles.diameter) parece que
# se resolveron los problemas de con las condiciones de linealidad e
# independencia de los errores, y la problem�tica multicolinealidad.
