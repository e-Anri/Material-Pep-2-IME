# Indicar directorio con los datos
dir <- ""

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

# Una parte del algoritmo generador de números pseudo
# aleatorios fue cambiado desde la versión 3.6.0 de R.
# Este script fue hecho con una versión anterior, por lo
# que es necesario ejecutar la siguiente línea antes de
# fijar una semilla para obtener la misma secuencia que
# en las versiones anteriores
RNGkind(sample.kind = "Rounding")

# Tomamos la misma muestra con igual número de varones y mujeres
i <- which(datos.todos[["Gender"]] == 0)
j <- which(datos.todos[["Gender"]] == 1)
set.seed(13)
N <- 15
i <- sample(i, N)
j <- sample(j, N)
datos <- datos.todos[c(i, j), ]

# Vimos que con regresión múltiple, uno podía hacer predecciones
# suponiendo la existencia de un modelo con la forma:
#
#     Y = (??0 + ??1·mu_Y|X1 + ??2·mu_Y|X2 + ... + ??k·mu_Y|Xk) + error
#
# que se estimaba consiguiendo aproximaciones a los parámetros ??:
#
#     Yi_est = b0 + b1 · X1i + b2 · X2i + ... + bk · Xki
#
# minimizando sum[(Yi - Yi_est)^2].
#
# En este contexto, consideramos Género como otra variable predictora
# del Peso (además de Estatura), pero encontramos que, al parecer, no
# valía la pena.
#
# También vimos que el coeficiente de determinación R^2 presenta
# un problema en el caso múltiple: siempre aumenta cuando se agrega una
# variable a un modelo, por lo que se suele ajustar de acuerdo a la
# disminución de los grados de libertad del modelo.
#
# Además se mencionó que a esta medida ajustada se le critica que no
# considera el desempeño del modelo en datos distintos a los usados
# para construirlo. Aunque no hay consenso, existen propuestas para
# obtener una medición llamada Coeficiente de Validación Cruzada, que
# haría exactamente esto.
#
# Más aún, dijimos que existen medidas alternativas para evaluar modelos
# que también consideran parsimonia. Dos de las más populares son el
# Criterio de Información de Akaike (AIC) y el Criterio de Información
# Bayesiano de Schwarz (BIC).
#
# Entonces, teniendo la posibilidad de medir la calidad de un modelo de
# regresión multivariable, ¿no es razonable probar varios modelos y
# quedarse con el mejor? De hecho, los datos de "body" tiene 24 posibles
# variables predictivas. ¡Podríamos probar otros 23 modelos para
# predecir Peso! ¡Y otros 275 modelos con dos variables predictoras!
# ¡Y 2.024 modelos con tres variables predictoras! ¡Y ...! y un modelo
# con 24 variables predictoras.
# Son muchos modelos...
#
# Por esto se habla de los "métodos de regresión", que se refieren a
# formas de ir "mejorando o depurando" modelos multivariados, por ejemplo
# regresión jerárquica, regresión de entrada forzada, regresión
# escalonada (o paso a paso), y regresión exhaustiva.
#
# Tratemos regresión escalonada. Aquí hay tres posibilidades:
# - selección hacia adelante: se parte con un modelo simple
#   (generalmente el modelo nulo, con todos los ??'s en cero, y que
#   contiene solo la intersección, una constante, que no es otra cosa
#   que la media de los Yi observados) y se va agregando, una a una, las
#   variable predictoras más promisorias
# - eliminación hacia atrás: se parte con un modelo completo, con todas
#   las variables predictoras, y se comienza a eliminar una a una las
#   variables que parecen menos relevantes
# - mixto o con retrocedo: se parte hacia adelante y luego se optimiza
#   eliminando variables que ya no son útiles
#
# Probemos la construcción de un modelo con el método escalonado hacia
# adelante, sin ninguna variable predictora.

nulo <- lm(Weight ~ 1, data = datos)

# Agregemos una variable al modelo y evaluemos sin obtenemos algo mejor.
# Ya vimos que Estatura parece ser un buen predictor.
estatura <- lm(Weight ~ Height, data = datos)

cat("\n\n")
cat("Comparando el modelo nulo con el modelo que usa Estatura\n")
cat("--------------------------------------------------------\n")
print(anova(nulo, estatura))

# cat("\n\n");stop("*** SIN ERROR ***")

# Podemos ver que agregar la variable Estatura mejoró de forma
# significativa el ajuste del modelo a los datos (de Peso) con respecto
# al modelo nulo, F(1, 28) = 33.928, p < .001.-

# También podemos comparar los modelos en base a AIC, por ejemplo:

cat("\n\n")
cat("Comparando estos modelos con AIC\n")
cat("--------------------------------\n")
print(extractAIC(nulo))
print(extractAIC(estatura))

# cat("\n\n");stop("*** SIN ERROR ***")

# Pero no existe forma de saber si la reducción en AIC (o BIC) es
# significativa.

# ¿Pero será Estatura la mejor alternativa para hacer crecer el modelo?
# Habría que probar con otros modelos (o todos) los modelos con una
# variable predictiva y ver cúal logra el R^2 ajustado mayor (con todas
# las críticas que este método tiene) o el menor AIC.

# Pero R nos da otra alternativa... la función add1() permite evaluar
# todas las posibles extenciones de un modelo.
# Para esto es necesario especificar el alcance de esta extensión. La
# opción más burda, es usar el modelo completo, con todas las posibles
# variables.
completo <- lm(Weight ~ ., data = datos)

cat("\n\n.....\n")
print(add1(nulo, scope = completo, test = "F"))

# cat("\n\n");stop("*** SIN ERROR ***")

# Podemos ver que, en realidad, la mejor variable predictora (la que
# explica más varianza de la variable Peso, y además muestra el menor
# valor de AIC) es el ¡grosor a la altura de los hombros!
# Creemos entonces este modelo.

shoulder <- lm(Weight ~ Shoulder.Girth, data = datos)

# Ahora repetimos el proceso

cat("\n\n.....\n")
print(add1(shoulder, scope = completo, test = "F"))

# cat("\n\n");stop("*** SIN ERROR ***")

# Deberíamos agregar el grosor a la altura de las caderas.
# Y así sucesivamente...

# ¡Pero R puede hacer todo esto por nosotros!

cat("\n\n")
cat("=============================================================\n")
cat("Agregando variables al modelo con selección hacia adelante...\n")
cat("=============================================================\n")
cat("\n\n")
auto1 <- step(
  nulo,
  scope = list(upper = completo),
  direction = "forward"
)
cat("\n\n")
cat("=============================================================\n")
cat("\n\n")
cat("Modelo obtenido automáticamente con selección hacia adelante\n")
cat("------------------------------------------------------------\n")
print(auto1)

# cat("\n\n");stop("*** SIN ERROR ***")

# Así obtenemos el modelo:
# Weight ~ Shoulder.Girth + Hip.Girth + Knee.Girth + Height +
#          Waist.Girth + Ankle.Minimum.Girth + Age + Chest.diameter +
#          Knees.diameter + Ankles.diameter + Biacromial.diameter +
#          Wrists.diameter

# Sin embargo, este modelo podría no ser tan bueno, debido al denominado
# efecto supresor, que quiere decir que al agregar una variable, esta
# podría hacer irrelevante otra variable que ya existe en el modelo.
#
# Una forma de enfrentar esto, es aplicar eliminación escalonada
# después de construir el modelo hacia adelante:

cat("\n\n.....\n")
print(drop1(auto1, test = "F"))

# cat("\n\n");stop("*** SIN ERROR ***")

# Vemos que el modelo no se ve afectado si borramos ¡la variable
# Estatura! Creemos el nuevo modelo, más reducido, y volvemos a revisar
# si hay otra variable irrelevante.

# Claro que, una vez más, podemos solicitar a R que haga esto
# automáticamente...

cat("\n\n")
cat("===========================================================\n")
cat("Quitando variables al modelo con eliminación hacia atrás...\n")
cat("===========================================================\n")
cat("\n\n")
auto2 <- step(
  auto1,
  scope = list(lower = nulo),
  direction = "backward"
)
cat("\n\n")
cat("=============================================================\n")
cat("\n\n")
cat("Modelo obtenido automáticamente con eliminación hacia atrás\n")
cat("-----------------------------------------------------------\n")
print(auto2)

# cat("\n\n");stop("*** SIN ERROR ***")

# Así obtenemos un modelo más reducido:
# Weight ~ Shoulder.Girth + Hip.Girth + Knee.Girth + Waist.Girth +
#          Ankle.Minimum.Girth + Age + Chest.diameter + Knees.diameter +
#          Biacromial.diameter + Wrists.diameter

# Claro que podríamos haber llegado a un modelo bien similar
# encargándole todo a R mientras vamos a tomar desayuno...

cat("\n\n")
cat("==================================================================\n")
cat("Creando un modelo con regresión escalonada en ambas direcciones...\n")
cat("==================================================================\n")
cat("\n\n")
modelo <- step(
  nulo,
  scope = list(lower = nulo, upper = completo),
  direction = "both"
)
cat("\n\n")
cat("=============================================================\n")
cat("\n\n")
cat("Modelo obtenido automáticamente con regresión escalonada\n")
cat("--------------------------------------------------------\n")
print(modelo)

# Así obtenemos un modelo muy similar que el anterior:
# Weight ~ Shoulder.Girth + Hip.Girth + Knee.Girth + Waist.Girth +
#          Ankle.Minimum.Girth + Age + Chest.diameter + Knees.diameter +
#          Biacromial.diameter + Wrists.diameter + Navel.Girth

# cat("\n\n");stop("*** SIN ERROR ***")

# PERO DEBEMOS NOTAR QUE ESTO LO *PODEMOS* HACER *EN ESTE CASO* porque
# *NO* estamos tratando de *PROBAR* una teoría, sino que estamos
# simplemente explorando los datos. Solo en estos casos, podemos darnos
# la LIBERTAD de usar una búsqueda step-wise (o de otro tipo).
# *NO* es ético hacer este tipo de búsqueda para encontrar *el* modelo
# que queremos.

# Otra cosa: este es un proceso de OPTIMIZACIÓN, que usa AIC como la
# función a optimizar. Esta construcción escalonada no es otra cosa que
# un algoritmo goloso (greedy) para encontrar la mejor solución.
# Luego, no necesariamente obtendremos el modelo óptimo.
#
# ¡R puede buscar el óptimo!
# Pero obviamente, eso toma más tiempo y es factible con conjuntos de
# datos de tamaños moderado.
# Por ejemplo, le podemos pedir a R que pruebe todos los subconjuntos
# de variables de tamaño 1 a tamaño 24 (nvmax = 24) y que nos entregue
# los 3 mejores subconjunto por cada tamaño probado, a través de las
# funciones proporcionadas en el paquete

require(leaps)
subjs <- regsubsets(
  Weight ~ .,
  data = datos,
  method = "exhaustive",
  nbest = 3,
  nvmax = 24
)

# Y luego revisarlos de forma gráfica
plot(subjs)

# cat("\n\n");stop("*** SIN ERROR ***")

# Sin embargo es algo peligroso confiar en este "mejor modelo",
# porque muchas, muchas veces, un pequeño cambio en los datos,
# incluso de un solo punto, produce cambios en el ranking de
# modelos. De hecho, se han estudiado casos en que esto ha sido
# utilizado para declarar tal o cual producto o empresa como
# el/la "mejor", con todas la falta de ética que conlleva. 


# Y, lamentablemente, a pesar de todo este trabajo, podemos tener un 
# modelo bien rasca que en realidad no ayude a predecir nada.
# Es importante entonces hacer una serie de diagnósticos para revisar 
# la calidad del modelo (y confiar en él) y de los datos que se usaron.

# Partamos por esto último. Primero podemos verificar que el modelo no
# esté sobreinfluenciado por un número pequeño de casos atípicos
# (outliers). Este se hace midiendo la influencia de los casos a traves
# de "estadísticos de influencia" (influential statistics), que se
# presentan a continuación.
# [A Field, J Miles, Z Field (2012), Discovering Statistics using R,
# sección 7.7.1.2]
#

# Obtengamos solo los datos en el modelo
variables <- names(coef(modelo))[-1]
subdatos <- datos[, c(variables, "Weight")]

# Obtengamos los residuos y las estadísticas
output <- data.frame(predicted.probabilities = fitted(modelo))
output[["standardized.residuals"]] <- rstandard(modelo)
output[["studentized.residuals"]] <-rstudent(modelo)
output[["cooks.distance"]] <- cooks.distance(modelo)
output[["dfbeta"]] <- dfbeta(modelo)
output[["dffit"]] <- dffits(modelo)
output[["leverage"]] <- hatvalues(modelo)
output[["covariance.ratios"]] <- covratio(modelo)

cat("\n\n")
cat("Estadísticas de influencia para los casos\n")
cat("=========================================\n")

# cat("\n\n");stop("*** SIN ERROR ***")

# Sabemos que 95% de los residuos estandarizados deberían estar entre
# ???1.96 y +1.96, y que el 99% entre -2.58 y +2.58. Revisemos eso.
sospechosos1 <- which(abs(output[["standardized.residuals"]]) > 1.96)
cat("\n\n")
cat("Residuos estandarizados fuera del 95% esperado\n")
cat("----------------------------------------------\n")
print(sospechosos1)

# Recomendaciones dicen deberíamos preocuparnos por los casos en que la
# distancia de Cook es mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
cat("\n\n")
cat("Residuos con una distancia de Cook alta\n")
cat("---------------------------------------\n")
print(sospechosos2)

# También se recomienda revisar casos cuyo "leverage" sea más del doble
# o triple del leverage promedio: (k + 1)/n
leverage.promedio <- ncol(subdatos) / nrow(datos)
sospechosos3 <- which(output[["leverage"]] > 2*leverage.promedio)
cat("\n\n")
cat("Levarage promedio:", leverage.promedio, "\n")
cat("Residuos con levarage fuera de rango\n")
cat("------------------------------------\n")
print(sospechosos3)

# También podríamos revisar DFBeta, que debería ser < 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
cat("\n\n")
cat("Residuos con DFBeta sobre 1\n")
cat("---------------------------\n")
print(sospechosos4)

# Finalmente, los casos no deberían desviarse significativamente
# de los límites recomendados para el ratio de la covarianza:
# CVRi > 1 + [3(k + 1)/n]
# CVRi < 1 - [3(k + 1)/n]
CVRi.lower <- 1 - 3 * leverage.promedio
CVRi.upper <- 1 + 3 * leverage.promedio
sospechosos5 <- which(output[["covariance.ratios"]] < CVRi.lower |
                        output[["covariance.ratios"]] > CVRi.upper)
cat("\n\n")
cat("Límites para el ratio de la covarianza: ")
cat("[", CVRi.lower, ", ", CVRi.upper, "]", sep = "", "\n")
cat("Residuos con ratio de la covarianza fuera de rango\n")
cat("--------------------------------------------------\n")
print(sospechosos5)

sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4,
                 sospechosos5)
sospechosos <- sort(unique(sospechosos))
cat("\n\n")
cat("Casos sospechosos\n")
cat("-----------------\n")
print(round(
  output[sospechosos, c("cooks.distance", "leverage",
                        "covariance.ratios")], 3
)
)

# Podemos ver que, si bien hay casos cuyo ratio de varianza están bien
# fuera de rango, ninguno de ellos tiene una distancia de Cook mayor
# a 1, por lo que no debieran causar mucho problema.
# Tal vez el caso 78 podría ser un candidato para evaluar su eliminación
# del conjunto de datos para el análisis.

# cat("\n\n");stop("*** SIN ERROR ***")

# Ahora el otro punto: el modelo debe cumplir varias suposiciones para
# que podamos generalizar conclusiones del modelo a la población.
# [A Field, J Miles, Z Field (2012), Discovering Statistics using R,
# sección 7.7.2]

# Algunas son más de diseño del análisis (varias provienen de la
# regresión lineal simple):
# - Las variables predictoras (independientes) deben ser categóricas o
#   numéricas a nivel de intervalo, y la variable dependiente debe ser
#   numéricas a nivel de intervalo sin restricciones.
# - Los datos no pueden tener varianza cero
# - Los valores de la variable dependiente son independientes entre sí
# - La variable dependiente se relaciona linealmente con las variables
#   regresoras.
# - Las variables predictoras no están correlacionadas con variables
#   externas (que no estén incluidas en el modelo) que influyan en la
#   variable dependiente.

# Pero otras requieren trabajo matemático para verificarlas:

# 1) Independencia del error (residuos)
#
# Esto es que no ha de existir autocorrelación en los residuos. Esto
# puede probarse con una prueba estadística específica conocida con
# el nombre de sus autores: Durbin-Watson test, que verifica si dos
# residuos adyacentes (un retardo) están correlacionados.
# [J Durbin, GS Watson (1950). Testing for Serial Correlation in Least
# Squares Regression, I". Biometrika. 37(3-4):409-428;
# Durbin, GS Watson (1951). Testing for Serial Correlation in Least
# Squares Regression, II". Biometrika. 38(1-2):159-179]

# A diferencia de la mayoría de las pruebas de hipótesis, la Prueba de
# Durbin-Watson define tres regiones: rechazo de H0, no rechazo de H0 y
# una región no concluyente, por lo que existen dos valores críticos:
# dU (d-upper) and dL (d-lower), que deben buscarse en tablas publicadas.
# En todo caso, la siguiente función del paquete "car" nos da un p-valor:
library(car)

# En R, es fácil revisar más retardos, por ejemplo hasta 5 retardos:
cat("\n\n")
cat("Prueba de Durbin-Watson para autocorrelaciones entre errores\n")
cat("------------------------------------------------------------\n")
print(durbinWatsonTest(modelo, max.lag = 5))

# cat("\n\n");stop("*** SIN ERROR ***")

# En este caso, entonces, la prueba indica que no hay autocorrelaciones
# significativas.
#
# Debe tenerse cuidado con la Prueba de Durbin-Watson, puesto que
# depende del orden de los datos, por lo que reordenando se podría tener
# valores distintos.
#

# 2) Residuos distribuidos de forma normal, y
# 3) con homocedasticidad
#
# Tal cual, se asume que los residuos tienen distribución normal y que
# los residuos de cada nivel de cada variable predictora tienen
# aproximadamente la misma varianza.

# Normalmente, esto se evalúa de forma gráfica:
cat("\n\n")
# plot(modelo)

# Claro que también podría hacerse con una prueba de normalidad (como
# Shapiro-Wilk) y una prueba de homocedasticidad (como la prueba de 
# White) o la (más limitada) prueba de Breusch-Pagan.

# cat("\n\n");stop("*** SIN ERROR ***")


# 4) Multicolinealidad
#
# Esto se traduce a que no debería existir una relación lineal perfecta
# entre dos o más variables predictoras. Es decir, la correlación entre
# estas variables no debiera ser my alta.

# Podemos revisar esta condición por medio del factor de inflación de
# varianza (VIF) y el estadístico tolerancia (1 / VIF).
# Aunque no hay un acuerdo general, el valor VIF >= 10 se usa como
# umbral para preocuparse. También se ha encontrado que si el VIF
# promedio es mayor a 1, podría haber sesgo en el modelo.
# En el caso de la tolerancia, se ha sugerido que valores bajo 0.2
# podrían ser problemáticos. Aunque algunos académicos creen que valores
# cercanos a 0.4 deberían ser revisados.

vifs <- vif(modelo)
cat("\n\n")
cat("Factores de inflación de la varianza\n")
cat("------------------------------------\n")
print(round(vifs, 1))

cat("\n\n")
cat("Factor de inflación de la varianza medio\n")
cat("----------------------------------------\n")
print(round(mean(vifs), 1))

tols <- 1/vifs
cat("\n\n")
cat("Tolerancia\n")
cat("----------\n")
print(round(tols, 2))

# cat("\n\n");stop("*** SIN ERROR ***")

# Ooop! ¡Estos índices no están buenos!
# Es decir, los coeficientes ?? estimados por el modelo ¡no son
# confiables!
# ¿Qué hacemos?

# Veamos el modelo obtenido
print(summary(modelo))

# Una propuesta es eliminar una a una las variables que estén fuera de
# rango. Por ejemplo:
VIF.MAX <- 10
vars <- names(vifs)
while(any(vifs > VIF.MAX))
{
  cat("\n----\n")
  print(vifs)
  
  # Obtiene la variable con mayor vif y la elimina
  var.with.max.vif <- names(which(vifs == max(vifs)))
  vars <- vars[!(vars) %in% var.with.max.vif]
  fmla <- as.formula(paste("Weight ~ ", paste (vars, collapse=" + "), sep=""))
  
  # Obtiene nuevo modelo
  nuevo <- lm(fmla, data = datos)
  
  # Actualiza vifs
  vifs <- vif(nuevo)
}
cat("\n---- Final\n")
print(vifs)
cat("\n----\n")
print(summary(nuevo))

# Y ahora aparecen variables que ya no son relevantes y hay unas cuantas
# que están en el límite. Habría que comenzar a evaluar su eliminación,
# si es que tiene sentido, de acuerdo a lo que se conoce del contexto.

nuevo2 <- update(nuevo, . ~ . - Navel.Girth - Wrists.diameter)
cat("\n---- Eliminando nuevas variables irrelevantes\n")
print(summary(nuevo2))
cat("\n----\n")

# En este caso, las rodillas también parecen irrelevantes (en el límite)

nuevo3 <- update(nuevo2, . ~ . - Knees.diameter)
cat("\n---- Eliminando nuevas variables irrelevantes\n")
print(summary(nuevo3))
cat("\n----\n")

# Podemos decidir entre usando el criterio de Akaike

cat("\n---- Verificando con el criterio de Akaike\n")
print(AIC(nuevo, nuevo2, nuevo3))
cat("\n----\n")

# De acuerdo a este criterio de información, el modelo nuevo2
# parece ser un tanto mejor que los otros.

# cat("\n\n");stop("*** SIN ERROR ***")
