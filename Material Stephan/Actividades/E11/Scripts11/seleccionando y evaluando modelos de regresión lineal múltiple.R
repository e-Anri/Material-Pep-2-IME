# Indicar directorio con los datos
dir <- ""

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

# Una parte del algoritmo generador de n�meros pseudo
# aleatorios fue cambiado desde la versi�n 3.6.0 de R.
# Este script fue hecho con una versi�n anterior, por lo
# que es necesario ejecutar la siguiente l�nea antes de
# fijar una semilla para obtener la misma secuencia que
# en las versiones anteriores
RNGkind(sample.kind = "Rounding")

# Tomamos la misma muestra con igual n�mero de varones y mujeres
i <- which(datos.todos[["Gender"]] == 0)
j <- which(datos.todos[["Gender"]] == 1)
set.seed(13)
N <- 15
i <- sample(i, N)
j <- sample(j, N)
datos <- datos.todos[c(i, j), ]

# Vimos que con regresi�n m�ltiple, uno pod�a hacer predecciones
# suponiendo la existencia de un modelo con la forma:
#
#     Y = (??0 + ??1�mu_Y|X1 + ??2�mu_Y|X2 + ... + ??k�mu_Y|Xk) + error
#
# que se estimaba consiguiendo aproximaciones a los par�metros ??:
#
#     Yi_est = b0 + b1 � X1i + b2 � X2i + ... + bk � Xki
#
# minimizando sum[(Yi - Yi_est)^2].
#
# En este contexto, consideramos G�nero como otra variable predictora
# del Peso (adem�s de Estatura), pero encontramos que, al parecer, no
# val�a la pena.
#
# Tambi�n vimos que el coeficiente de determinaci�n R^2 presenta
# un problema en el caso m�ltiple: siempre aumenta cuando se agrega una
# variable a un modelo, por lo que se suele ajustar de acuerdo a la
# disminuci�n de los grados de libertad del modelo.
#
# Adem�s se mencion� que a esta medida ajustada se le critica que no
# considera el desempe�o del modelo en datos distintos a los usados
# para construirlo. Aunque no hay consenso, existen propuestas para
# obtener una medici�n llamada Coeficiente de Validaci�n Cruzada, que
# har�a exactamente esto.
#
# M�s a�n, dijimos que existen medidas alternativas para evaluar modelos
# que tambi�n consideran parsimonia. Dos de las m�s populares son el
# Criterio de Informaci�n de Akaike (AIC) y el Criterio de Informaci�n
# Bayesiano de Schwarz (BIC).
#
# Entonces, teniendo la posibilidad de medir la calidad de un modelo de
# regresi�n multivariable, �no es razonable probar varios modelos y
# quedarse con el mejor? De hecho, los datos de "body" tiene 24 posibles
# variables predictivas. �Podr�amos probar otros 23 modelos para
# predecir Peso! �Y otros 275 modelos con dos variables predictoras!
# �Y 2.024 modelos con tres variables predictoras! �Y ...! y un modelo
# con 24 variables predictoras.
# Son muchos modelos...
#
# Por esto se habla de los "m�todos de regresi�n", que se refieren a
# formas de ir "mejorando o depurando" modelos multivariados, por ejemplo
# regresi�n jer�rquica, regresi�n de entrada forzada, regresi�n
# escalonada (o paso a paso), y regresi�n exhaustiva.
#
# Tratemos regresi�n escalonada. Aqu� hay tres posibilidades:
# - selecci�n hacia adelante: se parte con un modelo simple
#   (generalmente el modelo nulo, con todos los ??'s en cero, y que
#   contiene solo la intersecci�n, una constante, que no es otra cosa
#   que la media de los Yi observados) y se va agregando, una a una, las
#   variable predictoras m�s promisorias
# - eliminaci�n hacia atr�s: se parte con un modelo completo, con todas
#   las variables predictoras, y se comienza a eliminar una a una las
#   variables que parecen menos relevantes
# - mixto o con retrocedo: se parte hacia adelante y luego se optimiza
#   eliminando variables que ya no son �tiles
#
# Probemos la construcci�n de un modelo con el m�todo escalonado hacia
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

# Podemos ver que agregar la variable Estatura mejor� de forma
# significativa el ajuste del modelo a los datos (de Peso) con respecto
# al modelo nulo, F(1, 28) = 33.928, p < .001.-

# Tambi�n podemos comparar los modelos en base a AIC, por ejemplo:

cat("\n\n")
cat("Comparando estos modelos con AIC\n")
cat("--------------------------------\n")
print(extractAIC(nulo))
print(extractAIC(estatura))

# cat("\n\n");stop("*** SIN ERROR ***")

# Pero no existe forma de saber si la reducci�n en AIC (o BIC) es
# significativa.

# �Pero ser� Estatura la mejor alternativa para hacer crecer el modelo?
# Habr�a que probar con otros modelos (o todos) los modelos con una
# variable predictiva y ver c�al logra el R^2 ajustado mayor (con todas
# las cr�ticas que este m�todo tiene) o el menor AIC.

# Pero R nos da otra alternativa... la funci�n add1() permite evaluar
# todas las posibles extenciones de un modelo.
# Para esto es necesario especificar el alcance de esta extensi�n. La
# opci�n m�s burda, es usar el modelo completo, con todas las posibles
# variables.
completo <- lm(Weight ~ ., data = datos)

cat("\n\n.....\n")
print(add1(nulo, scope = completo, test = "F"))

# cat("\n\n");stop("*** SIN ERROR ***")

# Podemos ver que, en realidad, la mejor variable predictora (la que
# explica m�s varianza de la variable Peso, y adem�s muestra el menor
# valor de AIC) es el �grosor a la altura de los hombros!
# Creemos entonces este modelo.

shoulder <- lm(Weight ~ Shoulder.Girth, data = datos)

# Ahora repetimos el proceso

cat("\n\n.....\n")
print(add1(shoulder, scope = completo, test = "F"))

# cat("\n\n");stop("*** SIN ERROR ***")

# Deber�amos agregar el grosor a la altura de las caderas.
# Y as� sucesivamente...

# �Pero R puede hacer todo esto por nosotros!

cat("\n\n")
cat("=============================================================\n")
cat("Agregando variables al modelo con selecci�n hacia adelante...\n")
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
cat("Modelo obtenido autom�ticamente con selecci�n hacia adelante\n")
cat("------------------------------------------------------------\n")
print(auto1)

# cat("\n\n");stop("*** SIN ERROR ***")

# As� obtenemos el modelo:
# Weight ~ Shoulder.Girth + Hip.Girth + Knee.Girth + Height +
#          Waist.Girth + Ankle.Minimum.Girth + Age + Chest.diameter +
#          Knees.diameter + Ankles.diameter + Biacromial.diameter +
#          Wrists.diameter

# Sin embargo, este modelo podr�a no ser tan bueno, debido al denominado
# efecto supresor, que quiere decir que al agregar una variable, esta
# podr�a hacer irrelevante otra variable que ya existe en el modelo.
#
# Una forma de enfrentar esto, es aplicar eliminaci�n escalonada
# despu�s de construir el modelo hacia adelante:

cat("\n\n.....\n")
print(drop1(auto1, test = "F"))

# cat("\n\n");stop("*** SIN ERROR ***")

# Vemos que el modelo no se ve afectado si borramos �la variable
# Estatura! Creemos el nuevo modelo, m�s reducido, y volvemos a revisar
# si hay otra variable irrelevante.

# Claro que, una vez m�s, podemos solicitar a R que haga esto
# autom�ticamente...

cat("\n\n")
cat("===========================================================\n")
cat("Quitando variables al modelo con eliminaci�n hacia atr�s...\n")
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
cat("Modelo obtenido autom�ticamente con eliminaci�n hacia atr�s\n")
cat("-----------------------------------------------------------\n")
print(auto2)

# cat("\n\n");stop("*** SIN ERROR ***")

# As� obtenemos un modelo m�s reducido:
# Weight ~ Shoulder.Girth + Hip.Girth + Knee.Girth + Waist.Girth +
#          Ankle.Minimum.Girth + Age + Chest.diameter + Knees.diameter +
#          Biacromial.diameter + Wrists.diameter

# Claro que podr�amos haber llegado a un modelo bien similar
# encarg�ndole todo a R mientras vamos a tomar desayuno...

cat("\n\n")
cat("==================================================================\n")
cat("Creando un modelo con regresi�n escalonada en ambas direcciones...\n")
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
cat("Modelo obtenido autom�ticamente con regresi�n escalonada\n")
cat("--------------------------------------------------------\n")
print(modelo)

# As� obtenemos un modelo muy similar que el anterior:
# Weight ~ Shoulder.Girth + Hip.Girth + Knee.Girth + Waist.Girth +
#          Ankle.Minimum.Girth + Age + Chest.diameter + Knees.diameter +
#          Biacromial.diameter + Wrists.diameter + Navel.Girth

# cat("\n\n");stop("*** SIN ERROR ***")

# PERO DEBEMOS NOTAR QUE ESTO LO *PODEMOS* HACER *EN ESTE CASO* porque
# *NO* estamos tratando de *PROBAR* una teor�a, sino que estamos
# simplemente explorando los datos. Solo en estos casos, podemos darnos
# la LIBERTAD de usar una b�squeda step-wise (o de otro tipo).
# *NO* es �tico hacer este tipo de b�squeda para encontrar *el* modelo
# que queremos.

# Otra cosa: este es un proceso de OPTIMIZACI�N, que usa AIC como la
# funci�n a optimizar. Esta construcci�n escalonada no es otra cosa que
# un algoritmo goloso (greedy) para encontrar la mejor soluci�n.
# Luego, no necesariamente obtendremos el modelo �ptimo.
#
# �R puede buscar el �ptimo!
# Pero obviamente, eso toma m�s tiempo y es factible con conjuntos de
# datos de tama�os moderado.
# Por ejemplo, le podemos pedir a R que pruebe todos los subconjuntos
# de variables de tama�o 1 a tama�o 24 (nvmax = 24) y que nos entregue
# los 3 mejores subconjunto por cada tama�o probado, a trav�s de las
# funciones proporcionadas en el paquete

require(leaps)
subjs <- regsubsets(
  Weight ~ .,
  data = datos,
  method = "exhaustive",
  nbest = 3,
  nvmax = 24
)

# Y luego revisarlos de forma gr�fica
plot(subjs)

# cat("\n\n");stop("*** SIN ERROR ***")

# Sin embargo es algo peligroso confiar en este "mejor modelo",
# porque muchas, muchas veces, un peque�o cambio en los datos,
# incluso de un solo punto, produce cambios en el ranking de
# modelos. De hecho, se han estudiado casos en que esto ha sido
# utilizado para declarar tal o cual producto o empresa como
# el/la "mejor", con todas la falta de �tica que conlleva. 


# Y, lamentablemente, a pesar de todo este trabajo, podemos tener un 
# modelo bien rasca que en realidad no ayude a predecir nada.
# Es importante entonces hacer una serie de diagn�sticos para revisar 
# la calidad del modelo (y confiar en �l) y de los datos que se usaron.

# Partamos por esto �ltimo. Primero podemos verificar que el modelo no
# est� sobreinfluenciado por un n�mero peque�o de casos at�picos
# (outliers). Este se hace midiendo la influencia de los casos a traves
# de "estad�sticos de influencia" (influential statistics), que se
# presentan a continuaci�n.
# [A Field, J Miles, Z Field (2012), Discovering Statistics using R,
# secci�n 7.7.1.2]
#

# Obtengamos solo los datos en el modelo
variables <- names(coef(modelo))[-1]
subdatos <- datos[, c(variables, "Weight")]

# Obtengamos los residuos y las estad�sticas
output <- data.frame(predicted.probabilities = fitted(modelo))
output[["standardized.residuals"]] <- rstandard(modelo)
output[["studentized.residuals"]] <-rstudent(modelo)
output[["cooks.distance"]] <- cooks.distance(modelo)
output[["dfbeta"]] <- dfbeta(modelo)
output[["dffit"]] <- dffits(modelo)
output[["leverage"]] <- hatvalues(modelo)
output[["covariance.ratios"]] <- covratio(modelo)

cat("\n\n")
cat("Estad�sticas de influencia para los casos\n")
cat("=========================================\n")

# cat("\n\n");stop("*** SIN ERROR ***")

# Sabemos que 95% de los residuos estandarizados deber�an estar entre
# ???1.96 y +1.96, y que el 99% entre -2.58 y +2.58. Revisemos eso.
sospechosos1 <- which(abs(output[["standardized.residuals"]]) > 1.96)
cat("\n\n")
cat("Residuos estandarizados fuera del 95% esperado\n")
cat("----------------------------------------------\n")
print(sospechosos1)

# Recomendaciones dicen deber�amos preocuparnos por los casos en que la
# distancia de Cook es mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
cat("\n\n")
cat("Residuos con una distancia de Cook alta\n")
cat("---------------------------------------\n")
print(sospechosos2)

# Tambi�n se recomienda revisar casos cuyo "leverage" sea m�s del doble
# o triple del leverage promedio: (k + 1)/n
leverage.promedio <- ncol(subdatos) / nrow(datos)
sospechosos3 <- which(output[["leverage"]] > 2*leverage.promedio)
cat("\n\n")
cat("Levarage promedio:", leverage.promedio, "\n")
cat("Residuos con levarage fuera de rango\n")
cat("------------------------------------\n")
print(sospechosos3)

# Tambi�n podr�amos revisar DFBeta, que deber�a ser < 1.
sospechosos4 <- which(apply(output[["dfbeta"]] >= 1, 1, any))
names(sospechosos4) <- NULL
cat("\n\n")
cat("Residuos con DFBeta sobre 1\n")
cat("---------------------------\n")
print(sospechosos4)

# Finalmente, los casos no deber�an desviarse significativamente
# de los l�mites recomendados para el ratio de la covarianza:
# CVRi > 1 + [3(k + 1)/n]
# CVRi < 1 - [3(k + 1)/n]
CVRi.lower <- 1 - 3 * leverage.promedio
CVRi.upper <- 1 + 3 * leverage.promedio
sospechosos5 <- which(output[["covariance.ratios"]] < CVRi.lower |
                        output[["covariance.ratios"]] > CVRi.upper)
cat("\n\n")
cat("L�mites para el ratio de la covarianza: ")
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

# Podemos ver que, si bien hay casos cuyo ratio de varianza est�n bien
# fuera de rango, ninguno de ellos tiene una distancia de Cook mayor
# a 1, por lo que no debieran causar mucho problema.
# Tal vez el caso 78 podr�a ser un candidato para evaluar su eliminaci�n
# del conjunto de datos para el an�lisis.

# cat("\n\n");stop("*** SIN ERROR ***")

# Ahora el otro punto: el modelo debe cumplir varias suposiciones para
# que podamos generalizar conclusiones del modelo a la poblaci�n.
# [A Field, J Miles, Z Field (2012), Discovering Statistics using R,
# secci�n 7.7.2]

# Algunas son m�s de dise�o del an�lisis (varias provienen de la
# regresi�n lineal simple):
# - Las variables predictoras (independientes) deben ser categ�ricas o
#   num�ricas a nivel de intervalo, y la variable dependiente debe ser
#   num�ricas a nivel de intervalo sin restricciones.
# - Los datos no pueden tener varianza cero
# - Los valores de la variable dependiente son independientes entre s�
# - La variable dependiente se relaciona linealmente con las variables
#   regresoras.
# - Las variables predictoras no est�n correlacionadas con variables
#   externas (que no est�n incluidas en el modelo) que influyan en la
#   variable dependiente.

# Pero otras requieren trabajo matem�tico para verificarlas:

# 1) Independencia del error (residuos)
#
# Esto es que no ha de existir autocorrelaci�n en los residuos. Esto
# puede probarse con una prueba estad�stica espec�fica conocida con
# el nombre de sus autores: Durbin-Watson test, que verifica si dos
# residuos adyacentes (un retardo) est�n correlacionados.
# [J Durbin, GS Watson (1950). Testing for Serial Correlation in Least
# Squares Regression, I". Biometrika. 37(3-4):409-428;
# Durbin, GS Watson (1951). Testing for Serial Correlation in Least
# Squares Regression, II". Biometrika. 38(1-2):159-179]

# A diferencia de la mayor�a de las pruebas de hip�tesis, la Prueba de
# Durbin-Watson define tres regiones: rechazo de H0, no rechazo de H0 y
# una regi�n no concluyente, por lo que existen dos valores cr�ticos:
# dU (d-upper) and dL (d-lower), que deben buscarse en tablas publicadas.
# En todo caso, la siguiente funci�n del paquete "car" nos da un p-valor:
library(car)

# En R, es f�cil revisar m�s retardos, por ejemplo hasta 5 retardos:
cat("\n\n")
cat("Prueba de Durbin-Watson para autocorrelaciones entre errores\n")
cat("------------------------------------------------------------\n")
print(durbinWatsonTest(modelo, max.lag = 5))

# cat("\n\n");stop("*** SIN ERROR ***")

# En este caso, entonces, la prueba indica que no hay autocorrelaciones
# significativas.
#
# Debe tenerse cuidado con la Prueba de Durbin-Watson, puesto que
# depende del orden de los datos, por lo que reordenando se podr�a tener
# valores distintos.
#

# 2) Residuos distribuidos de forma normal, y
# 3) con homocedasticidad
#
# Tal cual, se asume que los residuos tienen distribuci�n normal y que
# los residuos de cada nivel de cada variable predictora tienen
# aproximadamente la misma varianza.

# Normalmente, esto se eval�a de forma gr�fica:
cat("\n\n")
# plot(modelo)

# Claro que tambi�n podr�a hacerse con una prueba de normalidad (como
# Shapiro-Wilk) y una prueba de homocedasticidad (como la prueba de 
# White) o la (m�s limitada) prueba de Breusch-Pagan.

# cat("\n\n");stop("*** SIN ERROR ***")


# 4) Multicolinealidad
#
# Esto se traduce a que no deber�a existir una relaci�n lineal perfecta
# entre dos o m�s variables predictoras. Es decir, la correlaci�n entre
# estas variables no debiera ser my alta.

# Podemos revisar esta condici�n por medio del factor de inflaci�n de
# varianza (VIF) y el estad�stico tolerancia (1 / VIF).
# Aunque no hay un acuerdo general, el valor VIF >= 10 se usa como
# umbral para preocuparse. Tambi�n se ha encontrado que si el VIF
# promedio es mayor a 1, podr�a haber sesgo en el modelo.
# En el caso de la tolerancia, se ha sugerido que valores bajo 0.2
# podr�an ser problem�ticos. Aunque algunos acad�micos creen que valores
# cercanos a 0.4 deber�an ser revisados.

vifs <- vif(modelo)
cat("\n\n")
cat("Factores de inflaci�n de la varianza\n")
cat("------------------------------------\n")
print(round(vifs, 1))

cat("\n\n")
cat("Factor de inflaci�n de la varianza medio\n")
cat("----------------------------------------\n")
print(round(mean(vifs), 1))

tols <- 1/vifs
cat("\n\n")
cat("Tolerancia\n")
cat("----------\n")
print(round(tols, 2))

# cat("\n\n");stop("*** SIN ERROR ***")

# Ooop! �Estos �ndices no est�n buenos!
# Es decir, los coeficientes ?? estimados por el modelo �no son
# confiables!
# �Qu� hacemos?

# Veamos el modelo obtenido
print(summary(modelo))

# Una propuesta es eliminar una a una las variables que est�n fuera de
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
# que est�n en el l�mite. Habr�a que comenzar a evaluar su eliminaci�n,
# si es que tiene sentido, de acuerdo a lo que se conoce del contexto.

nuevo2 <- update(nuevo, . ~ . - Navel.Girth - Wrists.diameter)
cat("\n---- Eliminando nuevas variables irrelevantes\n")
print(summary(nuevo2))
cat("\n----\n")

# En este caso, las rodillas tambi�n parecen irrelevantes (en el l�mite)

nuevo3 <- update(nuevo2, . ~ . - Knees.diameter)
cat("\n---- Eliminando nuevas variables irrelevantes\n")
print(summary(nuevo3))
cat("\n----\n")

# Podemos decidir entre usando el criterio de Akaike

cat("\n---- Verificando con el criterio de Akaike\n")
print(AIC(nuevo, nuevo2, nuevo3))
cat("\n----\n")

# De acuerdo a este criterio de informaci�n, el modelo nuevo2
# parece ser un tanto mejor que los otros.

# cat("\n\n");stop("*** SIN ERROR ***")
