# Recordamos el modelo de la regresi�n log�stica que asocia variables
# predictoras X1, X2, ..., Xk a la probabilidad de que el caso sea de
# clase "positiva":
#
#     P(Y = +) =                            1 
#                -----------------------------------------------------
#                1 + exp(-(b0 + b1 � X1i + b2 � X2i + ... + bk � Xki))
# 
# Notemos que hay una regresi�n lineal multivariable para obtener los
# coeficientes b0, b1, b2, ..., bk.
# 
# 
# Tambi�n construimos un modelo de regresi�n log�stica para "detectar"
# personas con sobrepeso usando un par de medidas del cuerpo humano
# (body.csv). Reconstruyamos ese modelo.

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
RNGkind(sample.kind = "Rounding")
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

# Llevamos los datos a formato largo para su an�lisis
library(tidyr)
datos.long <- gather(datos.wide, key = "Medida", value = "Valor")
datos.long[["Medida"]] <- factor(datos.long[["Medida"]])

# Agregamos la clase: Normal o Sobrepeso
datos.long[["Categor�a"]] <- factor(rep(categor�a, ncol(datos.wide)))
datos.wide[["Categor�a"]] <- factor(categor�a)


# Aprovechemos este instance para introducir  el paquete "caret", que nos
# va a facilitar los procedimientos.
# Kuhn, M. (2008), "Building predictive models in R using the caret
# package, " Journal of Statistical Software
# (http://www.jstatsoft.org/article/view/v028i05/v28i05.pdf).

library(caret)

# Una funci�n bien �til, por ejemplo, podemos obtener f�cilmente un gr�fico
# con pares de variables predictoras donde se aprecien las clases.
# Para efectos de mejor visualizaci�n, evaluemos solo 4 de las variables
# identificadas como "potenciales" en la exploraci�n que hicimos la clase
# pasada:
vps <- c("Hip.Girth", "Chest.Girth", "Calf.Maximum.Girth", "Waist.Girth")
pl1 <- featurePlot(datos.wide[, vps], datos.wide[["Categor�a"]], plot = "pairs")


# cat("\n\n");stop("*** SIN ERROR ***")


# Al ojo, pareciera haber menos traslape con Hip.Girth y Chest.Girth, que es
# el modelo que se obtuvo con los "m�todos tradicionales" que usamos la
# clases pasada.
# 
# Grafiquemos estos 

library(ggpubr)
theme_set(theme_minimal() + theme(legend.position = "top"))
cols <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
          "#FFFF33", "#A65628", "#F781BF", "#999999")
bincols <- cols[1:2]
pl2 <- ggplot(datos.wide, aes(x = Hip.Girth, y = Chest.Girth))
pl2 <- pl2 + geom_point(aes(colour = Categor�a, fill = Categor�a,
                            size = I(6), alpha = I(.5)))
pl2 <- pl2 + scale_colour_manual(name = 'Categor�a', values = bincols)
pl2 <- pl2 + theme(legend.position = "top")
pl2 <- pl2 + coord_fixed(ratio = .5)


# cat("\n\n");stop("*** SIN ERROR ***")


# Y obtengamos el modelo log�stico que resulta de estas variables
# predictoras.
modelo <- glm(
  Categor�a ~ Hip.Girth + Chest.Girth,
  family = binomial(link = "logit"),
  data = datos.wide
)

# Podemos agregar al gr�fico la l�nea de decisi�n
n.puntos <- 300
x <- seq(min(datos.wide[["Hip.Girth"]]), max(datos.wide[["Hip.Girth"]]), length.out = n.puntos)
y <- seq(min(datos.wide[["Chest.Girth"]]), max(datos.wide[["Chest.Girth"]]), length.out = n.puntos)
grid <- expand.grid(Hip.Girth = x, Chest.Girth = y)
predicci�n <- predict(modelo, grid, type = "response")
clases.pred <- rep("Normal", length(predicci�n))
clases.pred[predicci�n > 0.5] <- "Sobrepeso"
clases.pred <- factor(clases.pred, levels = c("Normal", "Sobrepeso"))
d <- cbind(grid, Categor�a = clases.pred)
pl3 <- pl2 + geom_contour(data = d, aes(z = as.numeric(Categor�a)), 
                          color = cols[3], breaks = c(1.5))


# cat("\n\n");stop("*** SIN ERROR ***")


# Ya vimos que podemos evaluar la confiabilidad del modelo revisando la
# posible (mala) influencia de casos extremos, la linealidad de los
# predictores, la independencia de los residuos, la inexistencia de
# multicolinealidad.
# Pero en aplicaciones reales, importa tambi�n (y a veces m�s) la
# capacidad clasificadora del modelo.
# Veamos este punto.

predicci�n <- predict(modelo, datos.wide, type = "response")
clases.pred <- rep("Normal", length(predicci�n))
clases.pred[predicci�n > 0.5] <- "Sobrepeso"
clases.pred <- factor(clases.pred, levels = c("Normal", "Sobrepeso"))
matriz.confusion <- confusionMatrix(
  data = clases.pred,
  reference = datos.wide[["Categor�a"]],
  positive = "Sobrepeso"
)

cat("\n\n")
cat("Resultados de clasificaci�n (mismos datos)\n")
cat("------------------------------------------\n")
print(matriz.confusion)


# cat("\n\n");stop("*** SIN ERROR ***")


# Podemos ver que el modelo alcanza un 90% de aciertos, con una alta
# sensibilidad (81,8%) y especificidad (94,7%).

# Taambi�n podemos generar una curva ROC para el modelo
# Usando el paquete 'pROC'

library(pROC)

modelo.roc1 <- roc(datos.wide[["Categor�a"]], predicci�n)
# En porciento
modelo.roc2 <- roc(datos.wide[["Categor�a"]], predicci�n, percent = TRUE)
# Suavisado
modelo.roc3 <- roc(datos.wide[["Categor�a"]], predicci�n, percent = TRUE,
                   smooth = TRUE)
plot(modelo.roc1)
plot(modelo.roc2)
plot(modelo.roc3)


# cat("\n\n");stop("*** SIN ERROR ***")


#
# Pero �estamos haciendo trampa!
# Obvio, porque estamos "evaluando" el modelo con los mismos datos con
# que se construy�.
# M�s que trampa, esto es un error: nos lleva a un efecto que se conoce
# como "sobreajuste" o "sobreaprendizaje", que se refiere a que el modelo
# no es una buena generalizaci�n del l�mite de entre las clases, sino que
# m�s bien representa el l�mite entre clases que existe solo en los datos
# que tenemos y que hemos usados para construir el modelo.
# 
# Una opci�n es tener dos conjuntos de datos por separado: un "conjunto
# de entrenamiento" y un "conjunto de prueba". Construimos el modelo
# con el primero, y lo evaluamos con el segundo.
# Construyamos estos conjuntos, con 50% de los datos para "entrenar" y
# 50% para probar. Estas proporciones dependen de la cantidad de datos.
# Mitad y mitad, como aqu�, suele usarse cuando se tienen pocos datos en
# total. Con muestras m�s numerosas, 75-25% o 80-20% es m�s usado.
# 
# Para eso usamos la funci�n createDataPartition() que lo hace de forma
# aleatoria. Hay otras t�cnicas, que ver�n en otro curso (An�lisis de
# Datos)

set.seed(109)
i.para.entrenar <- createDataPartition(
  y = datos.wide[["Categor�a"]],
  times = 1,
  p = .5, 
  list = FALSE
)
i.para.entrenar1 <- c(i.para.entrenar)
i.para.probar1 <- (1:nrow(datos.wide))[-i.para.entrenar]
datos.train1 <- datos.wide[i.para.entrenar1, ]
datos.test1  <- datos.wide[-i.para.probar1, ]

# Ahora, "entrenamos" el modelo y lo probamos
modelo1 <- glm(
  Categor�a ~ Hip.Girth + Chest.Girth,
  family = binomial(link = "logit"),
  data = datos.train1
)

predicci�n1 <- predict(modelo1, datos.test1, type = "response")
clases.pred1 <- rep("Normal", length(predicci�n1))
clases.pred1[predicci�n1 > 0.5] <- "Sobrepeso"
clases.pred1 <- factor(clases.pred1, levels = c("Normal", "Sobrepeso"))
matriz.confusion1 <- confusionMatrix(
  data = clases.pred1,
  reference = datos.test1[["Categor�a"]],
  positive = "Sobrepeso"
)

cat("\n\n")
cat("Resultados de clasificaci�n (separaci�n 1)\n")
cat("------------------------------------------\n")
print(matriz.confusion1)

modelo1.roc <- roc(datos.test1[["Categor�a"]], predicci�n1,
                   percent = TRUE, print.auc = TRUE)
plot(modelo1.roc)


# cat("\n\n");stop("*** SIN ERROR ***")


# Vemos que el modelo ahora alcanza un 81,3% de aciertos, con un
# 66,7% de sensibilidad y muy buena especificidad (90,0%).

# Pero ahora cabe preguntarnos si, dado que la separaci�n de los datos
# usados para "entrenar" y para "probar" fue aleatoria, obtendr�amos
# resultados similares con otras separaciones. Repitamos el proceso.

set.seed(49)
i.para.entrenar <- createDataPartition(
  y = datos.wide[["Categor�a"]],
  times = 1,
  p = .5, 
  list = FALSE
)
i.para.entrenar2 <- c(i.para.entrenar)
i.para.probar2 <- (1:nrow(datos.wide))[-i.para.entrenar]
datos.train2 <- datos.wide[i.para.entrenar2, ]
datos.test2  <- datos.wide[-i.para.probar2, ]

# Ahora, "entrenamos" el modelo y lo probamos
modelo2 <- glm(
  Categor�a ~ Hip.Girth + Chest.Girth,
  family = binomial(link = "logit"),
  data = datos.train2
)

predicci�n2 <- predict(modelo2, datos.test2, type = "response")
clases.pred2 <- rep("Normal", length(predicci�n2))
clases.pred2[predicci�n2 > 0.5] <- "Sobrepeso"
clases.pred2 <- factor(clases.pred2, levels = c("Normal", "Sobrepeso"))
matriz.confusion2 <- confusionMatrix(
  data = clases.pred2,
  reference = datos.test2[["Categor�a"]],
  positive = "Sobrepeso"
)

cat("\n\n")
cat("Resultados de clasificaci�n (separaci�n 2)\n")
cat("------------------------------------------\n")
print(matriz.confusion2)

modelo2.roc <- roc(datos.test2[["Categor�a"]], predicci�n2,
                   percent = TRUE, print.auc = TRUE)
plot(modelo2.roc)


# cat("\n\n");stop("*** SIN ERROR ***")


# Vemos que los resultados son bastante distintos: los aciertos suben
# al 93,8%, la sensibilidad es perfecta (100%) y la especificidad se
# mantuvo, dando origen a una curva ROC muy diferente a la anterior.

# Nuevamente, esto es por el n�mero reducido de datos en la muestra, lo
# que hace que elegir uno u otro caso tenga una influencia notoria en la
# construcci�n o evaluaci�n del modelo.

# Debemos notar que esto ocurre a pesar de que a funci�n
# createDataPartition() intenta mantener la distribuci�n de las clases
# original al separar los datos.

pd <- summary(datos.wide[["Categor�a"]])
pctr1 <- summary(datos.train1[["Categor�a"]])
pcte1 <- summary(datos.test1[["Categor�a"]])
pctr2 <- summary(datos.train2[["Categor�a"]])
pcte2 <- summary(datos.test2[["Categor�a"]])
pd <- round(pd / sum(pd) * 100, 1)
pctr1 <- round(pctr1 / sum(pctr1) * 100, 1)
pcte1 <- round(pcte1 / sum(pcte1) * 100, 1)
pctr2 <- round(pctr2 / sum(pctr2) * 100, 1)
pcte2 <- round(pcte2 / sum(pcte2) * 100, 1)
cat("\n\n")
cat("Proporciones de las clases global[%]\n")
print(pd)
cat("Proporciones separaci�n 1 [%](train, test)\n")
print(pctr1)
print(pcte1)
cat("Proporciones separaci�n 2 [%](train, test)\n")
print(pctr2)
print(pcte2)


# cat("\n\n");stop("*** SIN ERROR ***")


# Y entonces �qu� hacemos?
# Una idea obvia es repetir esto varias veces y sacar un promedio...
# Esto es f�cil de hacer con la funci�n train()

set.seed(41 * 43 * 47)
modelo3 <- train(
  Categor�a ~ Hip.Girth + Chest.Girth,
  data = datos.wide,
  method = "glm"
)
cat("\n\n")
cat("Modelo con train() (parametros por defecto)\n")
cat("-------------------------------------------\n")
print(modelo3)

# Vemos que este modelo se obtuvo con 25 iteraciones de re-muestreo.
# En promedio, se tuvo una tasa de aciertos de 84,3%.


# cat("\n\n");stop("*** SIN ERROR ***")


# Todo esto se puede cambiar. Por ejemplo, especificar una semilla
# para el remuetro (para repitabilidad), el n�mero de iteraciones de
# re-muestreo, usar kappa (en vez de aciertos) como medida de desempe�o,
# etc.
# 
# De hecho, antes usamos una divisi�n del conjunto de datos, y no
# re-muestreo. Esto tambi�n es posible de hacer, definiendo un "control"
# para la funci�n train().

# Por ejemplo, podemos indicar al algoritmo que no haga nada y use todos
# los datos proporcionados.

control4 <- trainControl(method = "none")
modelo4 <- train(
  Categor�a ~ Hip.Girth + Chest.Girth,
  data = datos.wide,
  method = "glm",
  trControl = control4
)

cat("\n\n")
cat("Coeficientes del modelo original usando glm()\n")
cat("---------------------------------------------\n")
print(modelo$coefficients)
cat("\n")
cat("Coeficientes del modelo con train() (control = \"none\")\n")
cat("------------------------------------------------------\n")
print(modelo4$finalModel$coefficients)


# cat("\n\n");stop("*** SIN ERROR ***")


# Vemos que obtenemos el mismo modelo original.

# Claro que esto no nos interesa.
# M�s nos gusta el modelo hecho con re-muestreo.
# Veamos si resulta mejor al compararlo con los modelos que "entrenamos"
# manualmente. Esto lo hacemos con la funci�n resamples().

clases.pred3.1 <- predict(modelo3, newdata = datos.test1)
clases.pred3.2 <- predict(modelo3, newdata = datos.test2)

cat("\n\n")
cat("Modelo con train() (parametros por defecto)\n")
cat("-------------------------------------------\n")
cat("\n")
cat("Predicci�n con separaci�n 1\n")
print(clases.pred3.1)
cat("\n")
cat("Predicci�n con separaci�n 2\n")
print(clases.pred3.2)


# cat("\n\n");stop("*** SIN ERROR ***")


# Podemos ver que la funci�n predict() ahora devuelve �las clases
# predichas! (que antes era un poco engorroso de lograr). 
# Podemos obtener matrices de confusi�n con estos resultados

matriz.confusion3.1 <- confusionMatrix(
  data = clases.pred3.1,
  reference = datos.test1[["Categor�a"]],
  positive = "Sobrepeso"
)
matriz.confusion3.2 <- confusionMatrix(
  data = clases.pred3.2,
  reference = datos.test2[["Categor�a"]],
  positive = "Sobrepeso"
)

cat("\n")
cat("Desempe�o con separaci�n 1\n")
print(matriz.confusion3.1)
cat("\n")
cat("Desempe�o con separaci�n 2\n")
print(matriz.confusion3.2)


# cat("\n\n");stop("*** SIN ERROR ***")


# Para poder hacer una curva ROC, necesitamos las probabilidades, las
# tenemos que solicitar expl�citamente.

prob.pred3.1 <- predict(modelo3, newdata = datos.test1, type = "prob")
prob.pred3.2 <- predict(modelo3, newdata = datos.test2, type = "prob")

cat("\n")
cat("Probabilidades con separaci�n 1\n")
print(prob.pred3.1)
cat("\n")
cat("Probabilidades con separaci�n 2\n")
print(prob.pred3.2)


# cat("\n\n");stop("*** SIN ERROR ***")


# Para poder hacer una curva ROC, necesitamos las probabilidades, las
# tenemos que solicitar expl�citamente.

modelo3.roc.1 <- roc(datos.test1[["Categor�a"]],
                     prob.pred3.1[["Sobrepeso"]],
                     percent = TRUE,
                     print.auc = TRUE)
modelo3.roc.2 <- roc(datos.test2[["Categor�a"]],
                     prob.pred3.2[["Sobrepeso"]],
                     percent = TRUE,
                     print.auc = TRUE)

# Los juntamos en un solo gr�fico

# library(grid)
# library(gridExtra)

ggroc1 <- ggroc(modelo1.roc) + ggtitle("Modelo 1, test 1")
ggroc2 <- ggroc(modelo2.roc) + ggtitle("Modelo 2, test 2")
ggroc3.1 <- ggroc(modelo3.roc.1) + ggtitle("Modelo 3, test 1")
ggroc3.2 <- ggroc(modelo3.roc.2) + ggtitle("Modelo 3, test 2")

rocs <- ggarrange(ggroc1, ggroc3.1, ggroc2, ggroc3.2,
                  nrow = 2, ncol = 2)
print(rocs)


# cat("\n\n");stop("*** SIN ERROR ***")


# De hecho, podemos usar el �rea bajo la curva (AUC) ROC como criterio
# para seleccionar el mejor modelo.

control5 <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

set.seed(61 * 67 * 71)
modelo5 <- train(
  Categor�a ~ Hip.Girth + Chest.Girth,
  data = datos.wide,
  method = "glm",
  trControl = control5
)
cat("\n\n")
cat("Modelo evaluado con AUC-ROC\n")
cat("---------------------------\n")
print(modelo5)


# cat("\n\n");stop("*** SIN ERROR ***")


# Tambi�n podemos conseguir un modelo usando validaci�n cruzada.
# Aqu�, como son 30 datos, haremos 6 folds de 5 casos cada uno.

control6 <- trainControl(
  method = "cv",
  number = 6,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

set.seed(61 * 67 * 71)
modelo6 <- train(
  Categor�a ~ Hip.Girth + Chest.Girth,
  data = datos.wide,
  method = "glm",
  trControl = control6
)
cat("\n\n")
cat("Modelo evaluado con AUC-ROC, 6-fold CV\n")
cat("--------------------------------------\n")
print(modelo6)


# cat("\n\n");stop("*** SIN ERROR ***")


# Y podemos llevar la validaci�n cruzada al extremo, usando todos menos
# un caso para entrenamiento. 

control7 <- trainControl(
  method = "LOOCV",
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

set.seed(61 * 67 * 71)
modelo7 <- train(
  Categor�a ~ Hip.Girth + Chest.Girth,
  data = datos.wide,
  method = "glm",
  trControl = control7
)
cat("\n\n")
cat("Modelo evaluado con AUC-ROC, Leave-one-out CV\n")
cat("---------------------------------------------\n")
print(modelo7)


# cat("\n\n");stop("*** SIN ERROR ***")


# Pero, �son estos modelos distintos?
cat("\n\n")
cat("Coeficientes de los modelos obtenidos\n")
cat("-------------------------------------\n")
print(modelo$coefficients)
print(modelo3$finalModel$coefficients)
print(modelo4$finalModel$coefficients)
print(modelo5$finalModel$coefficients)
print(modelo6$finalModel$coefficients)
print(modelo7$finalModel$coefficients)

# Vemos que obtenemos el mismo modelo siempre.
# �Por qu�?

# La respuesta es simple: todos estos m�todos est�n pensados en el
# "ajuste" de un modelo a los datos. Usualmente, este ajuste se hace
# "sintonizando" uno o m�s par�metros *del* modelo. As�, el mejor modelo
# con los datos *proporcionados* debe *buscarse*. Por eso, para saber
# cuan bien *generaliza* un modelo, se utiliza re-muestreo o validaci�n
# cruzada de alg�n tipo.
# Pero, la regresi�n log�stica *NO* tiene de estos par�metros que deben
# sintonizarse, puesto que buscar la recta que mejor encaja en el
# exponente de la funci�n log�stica.
# As�, no importa la b�squeda, siempre llegamos al mismo modelo, que la
# funci�n train() construye al final con todos los datos.

modelo.orig <- glm(
  Categor�a ~ Hip.Girth + Chest.Girth,
  family = binomial(link = "logit"),
  data = datos.wide
)

cat("\n\n")
cat("Coeficiente con el model construido sin sintonizaci�n:\n")
print(modelo.orig$coefficients)


# cat("\n\n");stop("*** SIN ERROR ***")


# �C�mo podemos obtener diferentes modelos de regresi�n log�stica?
# Vimos que eso pasa al usar diferentes permutaciones de variables
# predictoras.
# 
# Obviamente, esto no es exclusivo de la regresi�n log�stica y la
# selecci�n de las mejores variables predictoras (feature selection) es
# un tema muy discutido y estudiado en aprendizaje autom�tico (machine
# learning), miner�a de datos, etc.
# 
# �Podemos hacer eso en caret?
# Podemos f�cilmente con regresi�n lineal, y con un poco m�s de
# dificultad en el caso de la regresi�n log�stica.

# Veamos un ejemplo, tratando de predecir Peso

nombre.salida <- "Weight"
nombres.predictoras <- colnames(datos.todos)
i <- which(nombres.predictoras == nombre.salida)
nombres.predictoras <- nombres.predictoras[-i]

predictoras <- datos.wide[, nombres.predictoras]
salida <- datos.wide[, nombre.salida]

# En caret, se implementa lo que vimos como "regresi�n escalonada hacia
# atr�s", que tambi�n tiene el nombre de Recursive Feature Elimination,
# de ah� que la funci�n que lo implementa se llame rfe().

# Para esta b�squeda, tambi�n podemos definir alternativas de control
# que gu�en la b�squeda. Esto incluye funciones wrapper para el tipo de
# modelo. El paquete caret proporciona la funci�n wrapper lmFuncs para
# regresi�n lineal.


set.seed(13)
control1 <- rfeControl(functions = lmFuncs)
mrl1.1 <- rfe(predictoras, salida, rfeControl = control1)
cat("\n\n")
cat("Probando Recursive Feature Elimination (default)\n")
cat("------------------------------------------------\n")
print(mrl1.1)
prl1.1 <- ggplot(mrl1.1)
print(prl1.1)


# cat("\n\n");stop("*** SIN ERROR ***")


# Por defecto, rfe() prueba conjuntos de variables predictoras que son
# potencias de 2 (es decir, 4, 8 y 16, para ser exactos).
# El gr�fico muestra c�mo evoluciona la ra�z del error cuadr�tico medio
# (RSME) a medida que se van considerando m�s y m�s variables
# predictoras. El algoritmo, entonces, selecciona el n�mero de variables
# predictoras que minimiza esta medida.

# Podemos cambiar el n�mero de variables predictoras a probar, por
# ejemplo, probemos con modelos de 2 a 10 variables.
# 
set.seed(13)
mrl1.2 <- rfe(predictoras, salida, rfeControl = control1, sizes = 2:10)
cat("\n\n")
cat("Recursive Feature Elimination con tama�os 2..10\n")
cat("-----------------------------------------------\n")
print(mrl1.2)
prl1.2 <- ggplot(mrl1.2)
print(prl1.2)


# cat("\n\n");stop("*** SIN ERROR ***")


# Es com�n, en t�rminos de b�squeda de modelos, hacer esto: una 
# exploraci�n "gruesa" (potencias de 2) y luego un recorrido m�s fino
# alrededor de buenos valores encontrados en la primera iteraci�n.

# Como antes, poder cambiar esta b�squeda basada en re-muestreo por
# alternativas basadas en validaci�n cruzada.

set.seed(13)
control2 <- rfeControl(functions = lmFuncs, method = "cv", number = 6)
mrl2 <- rfe(predictoras, salida, rfeControl = control2, sizes = 2:10)
prl2 <- ggplot(mrl2)
cat("\n\n")
cat("RFE con tama�os 2..10 y CV\n")
cat("--------------------------\n")
print(mrl2)
print(prl2)

set.seed(13)
control3 <- rfeControl(functions = lmFuncs, method = "LOOCV")
mrl3 <- rfe(predictoras, salida, rfeControl = control3, sizes = 2:10)
prl3 <- ggplot(mrl3)
cat("\n\n")
cat("RFE con tama�os 2..10 y Leave one out\n")
cat("-------------------------------------\n")
print(mrl3)
print(prl3)


# cat("\n\n");stop("*** SIN ERROR ***")


# Pucha, no coinciden en el n� de variables. Tal vez habr�a que
# considerar parsimonia.
# De hecho, el paquete tiene otras funciones que consideran este tipo de
# criterios, pero quedar� para el siguiente curso...

# 
# Pasemos a clasificaci�n.
# =======================

# Aunque no es f�cil de encontrar, *s�* hay un wrapper para la regresi�n
# log�stica que hemos estudiado (la encontr� revisando el c�digo fuente,
# �no los manuales!).

# Preparemos los datos...

nombre.salida <- "Categor�a"
nombres.no.usar <- c("Weight", "Height", "Categor�a")
nombres.predictoras <- colnames(datos.wide)
i <- which(nombres.predictoras %in% nombres.no.usar)
nombres.predictoras <- nombres.predictoras[-i]

predictoras <- datos.wide[, nombres.predictoras]
salida <- datos.wide[, nombre.salida]

# y probemos...

set.seed(13)
rfe.ctrl1 <- rfeControl(functions = lrFuncs, verbose = TRUE)
train.ctrl1 <- trainControl(method = "none")
mrlog1 <- rfe(predictoras, salida,
              rfeControl = rfe.ctrl1, trControl = train.ctrl1)
prlog1 <- ggplot(mrlog1)
cat("\n\n")
cat("RFE con RLog con remuestreo\n")
cat("---------------------------\n")
print(mrlog1)
print(prlog1)


# cat("\n\n");stop("*** SIN ERROR ***")


# tambi�n podemos usar con cv...

set.seed(13)
rfe.ctrl2 <- rfeControl(functions = lrFuncs, method = "cv",
                        number = 6, verbose = TRUE)
train.ctrl2 <- trainControl(method = "none")
mrlog2 <- rfe(predictoras, salida, 
              rfeControl = rfe.ctrl2, trControl = train.ctrl2)
prlog2 <- ggplot(mrlog2)
cat("\n\n")
cat("RFE con RLog con 6-fold CV\n")
cat("--------------------------\n")
print(mrlog2)
print(prlog2)


# cat("\n\n");stop("*** SIN ERROR ***")


# Tambi�n, con alguna dificultad, podemos usar AUC-ROC para la
# selecci�n. Esto porque se requiere modificar el m�todo por defecto:

lrFuncs2 <- lrFuncs
lrFuncs2$summary <- twoClassSummary

set.seed(13)
rfe.ctrl3 <- rfeControl(functions = lrFuncs2, method = "cv",
                        number = 6, verbose = TRUE)
train.ctrl3 <- trainControl(method = "none", classProbs = TRUE,
                            summaryFunction = twoClassSummary)
mrlog3 <- rfe(predictoras, salida, metric = "ROC",
              rfeControl = rfe.ctrl3, trControl = train.ctrl3)
prlog3 <- ggplot(mrlog3)
cat("\n\n")
cat("RFE con RLog con 6-fold CV y usando ROC como medida objetivo\n")
cat("------------------------------------------------------------\n")
print(mrlog3)
print(prlog3)


# cat("\n\n");stop("*** SIN ERROR ***")


# Vemos que nos quedamos con el modelo de 8 variables, que alcanza un
# �rea bajo la curva ROC de 88.19% (84.72% de sensibilidad y 83.33% de
# especificidad).

# Mirando los resultados, hagamos una b�squeda fina:

set.seed(13)
rfe.ctrl4 <- rfeControl(functions = lrFuncs2, method = "cv",
                        number = 6, verbose = TRUE)
train.ctrl4 <- trainControl(method = "none", classProbs = TRUE,
                            summaryFunction = twoClassSummary)
mrlog4 <- rfe(predictoras, salida, metric = "ROC",
              rfeControl = rfe.ctrl4, trControl = train.ctrl4,
              sizes = 5:11)
prlog4 <- ggplot(mrlog4)
cat("\n\n")
cat("RFE con RLog con tama�os 5..11, ")
cat("con 6-fold CV y usando ROC como medida objetivo\n")
cat("--------------------------------")
cat("-----------------------------------------------\n")
print(mrlog4)
print(prlog4)

cat("\n\n")
cat("Modelo obtenido\n")
cat("---------------\n")
print(mrlog4[["fit"]])

# Vemos que el mejor rendimiento se obtiene usando 5 variables.
# Pero esto es el l�mite inferior que probamos, por lo que cabe
# preguntarse si valores menores podr�an llevar a mejores modelos.
# Probemos.

# Mirando los resultados, hagamos una b�squeda fina:

set.seed(13)
rfe.ctrl5 <- rfeControl(functions = lrFuncs2, method = "cv",
                        number = 6, verbose = TRUE)
train.ctrl5 <- trainControl(method = "none", classProbs = TRUE,
                            summaryFunction = twoClassSummary)
mrlog5 <- rfe(predictoras, salida, metric = "ROC",
              rfeControl = rfe.ctrl4, trControl = train.ctrl4,
              sizes = 2:6)
prlog5 <- ggplot(mrlog5)
cat("\n\n")
cat("RFE con RLog con tama�os 2..6, ")
cat("con 6-fold CV y usando ROC como medida objetivo\n")
cat("--------------------------------")
cat("-----------------------------------------------\n")
print(mrlog5)
print(prlog5)

cat("\n\n")
cat("Modelo obtenido\n")
cat("---------------\n")
print(mrlog5[["fit"]])


