# ¡¡¡ ADVERTENCIA !!!

# Una parte del algoritmo generador de números pseudo
# aleatorios fue cambiado desde la versión 3.6.0 de R.
# Este script fue hecho con una versión anterior, por lo
# que es necesario ejecutar la siguiente línea antes de
# fijar una semilla para obtener la misma secuencia que
# en la versión descrita:
RNGkind(sample.kind = "Rounding")

# Si no se ejecuta esta línea, se verán datos/observaciones
# distintas a las que aparecían en los comentarios de este script.
# Puede hacer eso para ejercitar con regresión logística múltiple !

# Vimos que con regresión múltiple, uno podía hacer predecciones
# suponiendo la existencia de un modelo con la forma:
#
#     Y = (??0 + ??1·mu_Y|X1  + ??2·mu_Y|X2  + ... + ??k·mu_Y|Xk) + error
#
# que se estimaba consiguiendo aproximaciones a los parámetros ??:
#
#     Yi_est = b0 + b1 · X1i + b2 · X2i + ... + bk · Xki
#
# minimizando sum[(Yi - Yi_est)^2].
#
# También mencionamos que esta generalización también es válida con la
# regresión logística binaria, cuando la variable dependiente representa
# dos posibles categorías (o clases; es decir un factor con dos niveles).
# Esto es extremadamente común en las aplicaciones diarias:
# cliente satisfecho o no, paciente tiene cáncer o no, solicitud debería
# aceptarse o no, etc.
#
# Vimos que construir este tipo de modelo usa la "función logística",
# que aparece en diversos modelos de crecimiento sigmoidal de una
# magnitud (por ejemplo epidemias, ventas de productos novedosos,
# viralización en redes sociales, poblaciones de microorganismos y
# mamíferos marinos).
# Su forma general es:
#
#      f(z) = 1 / (1 + exp(-z))
#
# Pero recordemos que nuestra variable de salida (dependiente) es
# categórica con dos valores (de ahí, binaria).
# Por eso su nombre invita al error: si bien se llama "regresión", la
# verdad es que el modelo es de clasificación.
# ¿Cómo podemos relacionar clases con una función sigmoide?
# Usando nuevamente un ejemplo con estudio: "supongamos" que las horas
# de estudio semanal tiene alguna relación con las probabilidades de
# aprobar una asignatura. Así:
#
#    horas estudio semanal   P(aprobar)   Predicción
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
# que crece el número de horas estudio, esta probabilidad aumenta hasta
# que pasa el umbral 0.5 (cambiando la predicción) y sigue aumentando,
# acercándose asintóticamente a 1.
#
# Si hablamos de clase "positiva" y "negativa", podemos pensar que la
# probabilidad asociada a la clase positiva sigue una función logística
# de una combinación lineal de las variables predictoras.
#
#     P(Y = +) =                            1
#                -----------------------------------------------------
#                1 + exp(-(b0 + b1 · X1i + b2 · X2i + ... + bk · Xki))
#
#
# Este es el modelo que se construye en la regresión logística binaria.
# Notemos que hay una regresión lineal multivariable aquí para poder
# obtener los coeficientes b0, b1, b2, ..., bk.
#

# Veamos otro ejemplo, volviendo a las medidas del cuerpo humano que
# hemos estado usando (body.csv).

# Indicar directorio
dir <- "~/../Downloads"

# Estos datos son medidas del cuerpo humano.
# Está descrito en el paquete "gclus" y en
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

# Tomamos la misma muestra con igual número de varones y mujeres
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
categoría <- rep("Normal", length(BMI))
categoría[BMI >= 25] <- "Sobrepeso"

# En el futuro (otro curso) se verán métodos para abordar clasificación
# con más de dos clases. Por ejemplo, basado en el índice de masa
# corporal también se puede identificar:
# categoría[BMI < 18.5] <- "Bajopeso"
# categoría[BMI >= 30] <- "Obesidad"
# categoría[BMI >= 40] <- "Obesidad mórbida"

# Llevamos los datos a formato largo para su análisis
library(tidyr)
datos.long <- gather(datos.wide, key = "Medida", value = "Valor")
datos.long[["Medida"]] <- factor(datos.long[["Medida"]])

# Agregamos la clase: Normal o Sobrepeso
datos.long[["Categoría"]] <- factor(rep(categoría, ncol(datos.wide)))
datos.wide[["Categoría"]] <- factor(categoría)

# Algunos gráficos. ¿Podemos decir a priori qué variables nos ayudarían
# a clasificar a una persona sin conocer su BMI?

library(ggpubr)
pb <- ggboxplot(
  data = datos.long,
  x = "Categoría", y = "Valor",
  color = "Medida", legend = "none"
)
pbf <- facet(pb, facet.by = "Medida", scales = "free")
print(pbf)

# cat("\n\n");stop("*** SIN ERROR ***")


# Para obtener un modelo de regresión logística confiable, se deben
# cumplir varias supocisiones/condiciones.
# Varias de ellas se comparte con regresión múltiple:
#
# 1) Linealidad
#
# A diferencia del caso de regresión lineal, no es posible establecer
# una relación lineal entre la variable de salida y las variables
# predictoras. Pero al suponer que la probabilidad de una de las dos
# clases tiene un crecimiento logístico, se está suponiendo que el
# logaritmo de la "oportunidad" de la clase tiene una relación lineal
# con las variables predictoras.
# "Oportunidad" aquí es una posible traducción de la palabra "odds" del
# inglés, que a veces se traduce como "chance", "momios",
# "posibilidades", "cuota" y otros más (incluso , "probabilidades" !!).
# Sin embargo, en estadística tiene una definición matemática clara:
#
#     Oportunidad = p / (1 - p)
#
# Es decir, la razón entre la probabilidad de que ocurra un evento y la
# probabilidad de que no ocurra el evento. Luego, si p es P(Y = +):
#
# log(p / (1 - p)) = b0 + b1 · X1i + b2 · X2i + ... + bk · Xki
#
# El cumplimiento de la suposición de linealidad puede verificarse
# revisando si existe interacción (significativa) entre los predictores
# usados en un modelo y su transformación logarítmica.
#
#
# 2) Independencia del error (residuales)
#
# Al igual que en regresión lineal, se asume que cada caso en los datos
# no influye en otros casos.
# Notemos que esta condición es violada con medidas repetidas, para las
# cuales se requiere otro tipo de método.
#
#

# Situaciones problemáticas
# -------------------------
#
# Hay muchas situaciones problemáticas para poder obtener matemática-
# mente un modelo de regresión logística. En estos casos, R típicamente
# reclama que el método de los mínimos cuadrados no converge.
#
# 1) Multicolinealidad
#
# Al igual que en regresión multivariada, correlaciones importantes
# entre predictores puede traernos problemas, ya que aumenta el error
# estándar de los coeficientes de regresión b, aumentando la incerti-
# dumbre de las estimaciones.
# Y al igual que en la regresión multivariada, podemos revisar esta
# seudo-condición por medio del factor de inflación de varianza (VIF),
# que indica si un predictor tiene una relación lineal fuerte con 
# otro(s) predictor(es), y el relacionado estadístico tolerancia
# (1 / VIF), o métodos más sofisticados.
# Según Field et al. (2012), VIF > 10 para una variable predictora
# podría ser preocupante. También un valor de VIF medio > 1 es 
# indicación que la multicolinealidad podría estar sesgando el modelo.
# También un valor de tolerancia < 0.1 es preocupante, aunque valores
# menores a 0.2 deberían revisarse también.
# [Field, A., Miles, J., & Field, Z. (2012). Discovering statistics
# using R. Sage publications.]
#
#
# 2) Información incompleta
#
# Obviamente es difícil tener *varios* representantes de cada posible
# combinación de predictores, en especial con variables categóricas.
# Las pruebas ??^2 que se usan con la regresión logística, por
# ejemplo, asume un valor mínimo de 1 en cada celda y no más de 20% de
# celdas con menos de 5 casos.
# Por ejemplo, revisemos qué pasa con nuestra variable de género:

library(gmodels)
CrossTable(
  datos.wide[["Gender"]], datos.wide[["Categoría"]],
  format = "SPSS", expected = TRUE
)

# 3) Separación perfecta
#
# Esto ocurre cuando una variable separa completamente a las clases,
# no hay traslape. Revisemos nuevamente el gráfico.


# cat("\n\n");stop("*** SIN ERROR ***")


# Vamos a crear nuestro primer modelo, que incluye a todas las variables
# (por eso "~ .")
modelo <- glm(
  Categoría ~ .,
  family = binomial(link = "logit"),
  data = datos.wide
)
print(summary(modelo))


# cat("\n\n");stop("*** SIN ERROR ***")


# Fijémonos en el "warning" que arroja R
# ¿Qué paso?
#
#
# Bueno, estamos incluyendo Estatura y Peso, las variables que usamos
# para calcular BMI, lo que separa perfectamente las categorías !!!

d <- datos.wide[, c("Height", "Weight", "Categoría")]

intercept <- -68.5
slope <- 0.83
y <- intercept + slope * d[["Height"]]

d <- cbind(d, y)

pp <- ggscatter(
  data = d,
  x = "Height", y = "Weight",
  color = "Categoría", legend = "none"
)
ppl <- pp + geom_line(
  aes(x = Height, y = y),
  colour = "#3C5488B2", size = 1.5
)
print(ppl)

# cat("\n\n");stop("*** SIN ERROR ***")


# Tenemos el clasificador perfecto !!
# Fome: No vamos a considerar estas variables en nuestro modelo, porque
# son las variables que están detrás de la clasificación que queremos
# predecir. No hay misterior aquí. Obviamente, esto no ocurre en la
# práctica.

# Mirando a los gráficos, las variables más útiles (menos traslape de
# clases) parecían ser (en orden, al ojo):
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
  Categoría ~ Waist.Girth,
  family = binomial(link = "logit"),
  data = datos.wide)
print(summary(modelo))


# cat("\n\n");stop("*** SIN ERROR ***")


# R reporta el estadístico "desviación" (deviance), también referido
# como -2LL, porque corresponde a -2·log-likelihood(modelo), esto es el
# logaritmo natural de la "función de verosimilitud" de los parámetros
# del modelo y el conjunto de datos.
# El estadístico desviación sigue una distribución ??^2, lo que permite
# calcular la significanción estadística de su valor (p-valor).
# "Null deviance" corresponde a la desviación del modelo nulo, es decir
# el que solo considera un "intercept". La diferencia de esta desviación
# con la de nuestro modelo es una indicación de su calidad. Esta
# diferencia también sigue una ??^2.
# De hecho, varias alternativas para obtener una medida análoga a R^2
# para regresión logística se han propuesto en base a estos estadísticos,
# pero todavía no hay uno aceptado ampliamente.
#
# Por eso AIC y BIC suelen ser más usadas.
#
# Debemos notar que, como para regresión lineal múltiple, R nos entrega
# una medida de la contribución de cada predictor, aunque en este caso
# se usa la distribución normal (en vez de t).
#
# ¿Será este el mejor modelo?
#
# No es ridículo pensar que el grosor de la cadera pueda ser un buen
# predictor, sobretodo en mujeres. Podemos entonces agregar esta
# variable. Hacemos esto, actualizando la fórmula del modelo:

nuevo <- update(modelo, . ~ . + Hip.Girth)
print(summary(nuevo))

# Vemos que se redujo tanto la desviación como el AIC, por lo que el
# nuevo modelo predice mejor que el primero.
#
# Para los modelos de regresión lineal múltiple se podían comparar con
# una prueba F y obtener una tabla ANOVA con ellos. Para regresión
# logística, podemos hacer algo similar, aunque la prueba F no es
# apropiada, por lo que tenemos que usar una alternativa.
# Una muy usada es el Likelihood Ratio Test (LRT), que compara qué tanto
# más "probable" son los datos con un modelo que con el otro.

cat("\n\n")
cat("Likelihood Ratio Test para los modelos\n")
cat("--------------------------------------\n")
print(anova(modelo, nuevo, test = "LRT"))


# cat("\n\n");stop("*** SIN ERROR ***")


# Luego, nos quedamos con el nuevo modelo:
modelo <- nuevo

# ¿Será este el mejor modelo?
#
# Tampoco es absurdo pensar que el grosor a la altura del ombligo pueda
# ser un buen predictor, sobretodo en hombres.
# Agreguemos esta variable entonces.

nuevo <- update(modelo, . ~ . + Navel.Girth)
print(summary(nuevo))

cat("Likelihood Ratio Test para el nuevo modelo\n")
cat("------------------------------------------\n")
print(anova(modelo, nuevo, test = "LRT"))

# Vemos que, si bien baja la desviación, el AIC aumentó un poco.
# Es más, la nueva variable no aparece impactando significativamente en
# el modelo.
# Luego, parece mejor mantener el modelo anterior, con dos variables
# (más simple).


# cat("\n\n");stop("*** SIN ERROR ***")


# Pero quedan muchas otras variables a probar. Así, para ampliar el
# modelo, podríamos calcular todos los modelos posibles al incluir otra
# de las variable regresoras todavía disponibles, compararlos con el
# original, y elegir aquel que "mejora más", tomando como referencia,
# por ejemplo, el índice AIC.
#
# Hacer esto es bastante tentador, puesto que R proporciona la función
# add1() que lo hace muy simple. También hay varias opciones de
# comparación (test).
#
# De hecho podemos partir del modelo nulo y dejar que R elija por
# nosotros.
#
nulo <- glm(
  Categoría ~ 1,
  family = binomial(link = "logit"),
  data = datos.wide
)
subc <- subset(datos.wide, select = -c(Categoría, Height, Weight))
variables <- names(subc)
posibles <- add1(nulo, scope = variables, direction = "forward",
                 trace = 1, test = "LRT")

# Mostramos la lista de modelos de una variable ordenado por AIC (de
# menor a mayor) y el estadístico LRT en caso de empate (de mayor a
# menor, i.e. de más a menos significativo).
orden <- order(posibles[["AIC"]], posibles[["LRT"]], decreasing = c(F, T))
cat("\n\n")
cat("Aporte al agregar una variable predictora al modelo nulo\n")
cat("--------------------------------------------------------\n")
print(posibles[orden, ])


# cat("\n\n");stop("*** SIN ERROR ***")


# Podemos ver el mejor modelo, con estos criterios, es que incluye el
# grosor de la cadera como variable predictora.
# Actualicemos nuestro modelo y repitamos la búsqueda.
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


# Y así podemos seguir, eligiendo y agregando variables hasta que no
# encontremos alguna variable que mejore el modelo.
#
# Pero sabemos quer R nos permite hacer esto mientras vamos a jugar con
# la Play! (eliminando primero las variables problemáticas)
datos <- subset(datos.wide, select = -c(Height, Weight))
total <- glm(
  Categoría ~ .,
  family = binomial(link = "logit"),
  data = datos
)
# Notemos que nuevamente hay problemas al usar todas las variables
# ¿Separación perfecta con otra combinación?

cat("\n\n")
cat("=============================================================\n")
cat("Agregando variables al modelo con selección hacia adelante...\n")
cat("=============================================================\n")
cat("\n\n")
auto1 <- step(nulo, scope = list(lower = nulo, upper = total),
              direction = "forward", test = "LRT", trace = 1)

cat("\n\n")
cat("=============================================================\n")
cat("\n\n")
cat("Modelo obtenido automáticamente con selección hacia adelante\n")
cat("------------------------------------------------------------\n")
print(auto1)

# cat("\n\n");stop("*** SIN ERROR ***")


# PERO DEBEMOS NOTAR, como dijimos anteriormente con la regresión lineal
# múltiple, QUE ESTO LO *PODEMOS* HACER *EN ESTE CASO* porque *NO*
# estamos tratando de *PROBAR* una teoría, sino que estamos simplemente
# explorando los datos. Solo en estos casos, podemos darnos la LIBERTAD
# de usar una búsqueda step-wise (o de otro tipo).
#

# Otra cosa: esta es un proceso de OPTIMIZACIÓN, que usa AIC como la
# función a optimizar. Este step-wise no es otra cosa que un algoritmo
# goloso (greedy) para encontrar la mejor solución.
# Luego, no necesariamente obtendremos el modelo óptimo.
#
# ¡R puede buscar el óptimo!
# Pero obviamente, eso toma más tiempo y es factible con conjuntos de
# datos de tamaños moderado. Ya vimos una alternativa.


# cat("\n\n");stop("*** SIN ERROR ***")


# Al igual que con los modelos de regresión multivariada, es importante
# revisar la calidad del modelo y de los datos que se usaron.

# Primero podemos ve rificar que el modelo no esté sobre influenciado por
# un número pequeño de casos atípicos (outliers). Este se hace midiendo
# la influencia de los casos a traves de "estadísticos de influencia"
# (influential statistics), que se presentan a continuación.
# [A Field, J Miles, Z Field (2012), Discovering Statistics using R,
# sección 7.7.1.2]
#

modelo <- auto1
variables <- names(coef(modelo))[-1]

# Obtengamos solo los datos en el modelo
subdatos <- datos[, c(variables, "Categoría")]

# Obtengamos los residuales y las estadísticas
output <- data.frame(predicted.probabilities = fitted(modelo))
output[["standardized.residuals"]] <- rstandard(modelo)
output[["studentized.residuals"]] <- rstudent(modelo)
output[["cooks.distance"]] <- cooks.distance(modelo)
output[["dfbeta"]] <- dfbeta(modelo)
output[["dffit"]] <- dffits(modelo)
output[["leverage"]] <- hatvalues(modelo)

cat("\n\n")
cat("Estadísticas de influencia para los casos\n")
cat("=========================================\n")

# Revisemos los residuales
# plot(modelo)

# Sabemos que 95% de los residuales estandarizados deberían estar entre
# ???1.96 y +1.96, y que el 99% entre -2.58 y +2.58. Revisemos eso.
sospechosos1 <- which(abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1)
cat("\n\n")
cat("Residuales estandarizados fuera del 95% esperado\n")
cat("------------------------------------------------\n")
print(rownames(subdatos[sospechosos1, ]))

# Recomendaciones dicen deberíamos preocuparnos por los casos en que la
# distancia de Cook es mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat("\n\n")
cat("Residuales con una distancia de Cook alta\n")
cat("-----------------------------------------\n")
print(rownames(subdatos[sospechosos2, ]))

# También se recomienda revisar casos cuyo "leverage" sea más del doble
# o triple del leverage promedio: (k + 1)/n
leverage.promedio <- ncol(subdatos) / nrow(datos)
sospechosos3 <- which(output[["leverage"]] > leverage.promedio)
sospechosos3 <- sort(sospechosos3)
cat("\n\n")
cat("Residuales con levarage fuera de rango (> ")
cat(round(leverage.promedio, 3), ")", "\n", sep = "")
cat("--------------------------------------\n")
print(rownames(subdatos[sospechosos3, ]))

# También podríamos revisar DFBeta, que debería ser < 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1, 1, any))
sospechosos4 <- sort(sospechosos4)
names(sospechosos4) <- NULL
cat("\n\n")
cat("Residuales con DFBeta sobre 1\n")
cat("-----------------------------\n")
print(rownames(subdatos[sospechosos4, ]))

# Puede verse que los casos sospechosos más o menos se repiten con cada
# estadística. Revisemos estos casos.

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

# Hay varias razones para preocuparse con estos casos (además de los
# warnings() de R). Sus valores de Leverage son enormes, y hay un caso
# con distancia de Cook realmente mayor a 1. Tal vez este nos debiera
# preocupar y evaluar la eliminación de estos casos del conjunto de
# datos para el análisis.


# cat("\n\n");stop("*** SIN ERROR ***")


# También habíamos dicho que, una vez que tuvieramos el modelo, debíamos
# revisar si se cumplían las suposiciones detrás de la regresión logís-
# tica (o múltiple).

# 1) Linealidad
#
# Dijimos que esta condición puede verificarse revisando si existe
# interacción significativa entre los predictores usados en un modelo
# y su transformación logarítmica.
# [DW Hosmer, S Lemeshow (2000). Applied logistic regression. 2nd Ed.
# John Wiley & Sons,  Inc.]
#
# Debemos crear las interacciones de cada variable con su logaritmo.
datos.log <- subdatos[, 1:3] * log(subdatos[, 1:3])
colnames(datos.log) <- paste("log_int", colnames(subdatos)[1:3], sep = ".")
datos.log <- cbind(subdatos[, 1:3], datos.log, Categoría = subdatos[[4]])
modelo.log <- glm(
  Categoría ~ .,
  family = binomial(link = "logit"),
  data = datos.log
)
cat("\n\n")
cat("Modelo con interacciones logarítmicas\n")
cat("-------------------------------------\n")
print(summary(modelo.log))


# cat("\n\n");stop("*** SIN ERROR ***")


# ¡ Ni siquiera converge ! Parece haber un problema serio...


# 2) Independencia del error (residuales)
#
# Es se traduce a que no ha de existir autocorrelación en los términos
# residuales. Esto puede probarse con una prueba estadística específica
# conocida con el nombre de sus autores: Durbin-Watson test, que
# verifica si dos residuales adyacentes (un retardo) están
# correlacionados.
# [J Durbin, GS Watson (1950). Testing for Serial Correlation in Least
# Squares Regression, I". Biometrika. 37(3-4):409-428;
# Durbin, GS Watson (1951). Testing for Serial Correlation in Least
# Squares Regression, II". Biometrika. 38(1-2):159-179]

# En R, es fácil revisar más retardos, por ejemplo hasta 5 retardos:
library(car)
cat("\n\n")
cat("Prueba de Durbin-Watson para autocorrelaciones entre errores\n")
cat("------------------------------------------------------------\n")
print(durbinWatsonTest(modelo, max.lag = 5))


# cat("\n\n");stop("*** SIN ERROR ***")


# A diferencia de la mayoría de las pruebas de hipótesis, la Prueba de
# Durbin-Watson define tres regiones: rechazo de H0, no rechazo de H0 y
# una región no concluyente, por lo que existen dos valores críticos:
# dU (d-upper) and dL (d-lower), que deben buscarse en tablas publicadas.
# [D Halcoussis (2005). Understanding econometrics. South-Western.]
# La función de R que hemos usado los calcula con bootstrapping.

# En este caso, el test indica autocorrelaciones significativas.
# Así, debemos dudar del modelo obtenido.
#
# En todo caso, debe tenerse cuidado con la Prueba de Durbin-Watson,
# puesto que depende del orden de los datos, por lo que reordenando
# se podría tener valores distintos. Aunque en este caso, es poco
# probable que cambie la conclusión de que existe autocorrelación.
#


# cat("\n\n");stop("*** SIN ERROR ***")



# Situaciones problemáticas: Multicolinealidad
# --------------------------------------------
#
# Dejamos pendiente la revisión de la multicolinealidad, que puede
# estropear las predicciones del modelo.
#
# Dijimos que podemos revisar esta seudo-condición por medio del factor
# de inflación de varianza (VIF) y el estadístico tolerancia (1 / VIF).
# Aunque no hay un acuerdo general, el valor VIF >= 10 se usa como
# umbral para preocuparse. También se ha encontrado que si el VIF
# promedio es mayor a 1, podría haber sesgo en el modelo.
# En el caso de la tolerancia, se ha sugerido que valores bajo 0.2
# podrían ser problemáticos. Aunque algunos académicos creen que valores
# cercanos a 0.4 deberían ser revisados.

vifs <- vif(modelo)
cat("\n")
cat("Factores de inflación de la varianza\n")
cat("------------------------------------\n")
print(round(vifs, 1))

cat("\n")
cat("Factor de inflación de la varianza medio\n")
cat("----------------------------------------\n")
print(round(mean(vifs), 1))

tols <- 1/vifs
cat("\n")
cat("Tolerancia\n")
cat("----------\n")
print(round(tols, 2))


# !!! Horrible !!!
# Esta es una confirmación de que nuestro modelo tiene problemas.
# Probablemente, existe mucha relación entre las variables incluidas
# en el modelo.
# ¿Qué tal si eliminamos los tobillos?

reducido <- update(modelo, . ~ . - Ankles.diameter)


vifs <- vif(reducido)
cat("\n\n")
cat("Factores de inflación de la varianza del modelo reducido (multicol.)\n")
cat("--------------------------------------------------------------------\n")
print(round(vifs, 1))

cat("\n\n")
cat("Prueba de Durbin-Watson para el modelo reducido (residuos indep.)\n")
cat("-----------------------------------------------------------------\n")
print(durbinWatsonTest(reducido, max.lag = 5))


cat("\n\n")
cat("Modelo reducido con interacciones logarítmicas (linealidad)\n")
cat("-----------------------------------------------------------\n")
variables2 <- names(coef(reducido))[-1]
subdatos2 <- datos[, c(variables2, "Categoría")]
datos.log2 <- subdatos2[, 1:2] * log(subdatos2[, 1:2])
colnames(datos.log2) <- paste("log_int", colnames(subdatos2)[1:2], sep = ".")
datos.log2 <- cbind(subdatos2[, 1:2], datos.log2, Categoría = subdatos2[[3]])
modelo.log2 <- glm(
  Categoría ~ .,
  family = binomial(link = "logit"),
  data = datos.log2
)
print(summary(modelo.log2))

# Mucho mejor... al borrar esa variable (Ankles.diameter) parece que
# se resolveron los problemas de con las condiciones de linealidad e
# independencia de los errores, y la problemática multicolinealidad.
