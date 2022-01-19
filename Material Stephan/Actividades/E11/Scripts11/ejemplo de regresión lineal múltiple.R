library(ggpubr)
library(scatterplot3d)

# Indicar directorio donde están los datos
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
# print(str(datos))

# Tomamos la misma muestra con igual número de varones y mujeres 
i <- which(datos.todos[["Gender"]] == 0)
j <- which(datos.todos[["Gender"]] == 1)
set.seed(13)
N <- 15
i <- sample(i, N)
j <- sample(j, N)
datos <- datos.todos[c(i, j), ]


# Recordemos nuestras ideas en términos de modelos:
# 
#    valor = (modelo) + error
# 
# Inicialmente, modelo era la media de la población (mu).
# Luego, en ANOVA, eran las medias de varias poblaciones (mu_i)
# Luego, en regresión lineal simple, una media condicional (mu_Y|X)
# Ahora, queremos extender esta última idea a más de una variable
# predictora X1, X2, ..., Xk:
# 
#     Y = (??0 + ??1·mu_Y|X1 + ??2·mu_Y|X2 + ... + ??k·mu_Y|Xk) + error
# 
# Los ??'s son los parámetros de la combinación lineal que relaciona
# los valores de X1, X2, ..., Xk con los valores de Y. Vimos que con
# dos parámetros tenemos una recta; con tres, veríamos un plano; con
# más es un hiperplano, pero no es tan fácil de dibujar (ver el filme
# Interstellar (2014) para un ejemplo).
# 
# A este modelo se le conoce como regresión lineal múltiple.
#
#
# Los principios son los mismos que los vistos con la regresión lineal
# simple y el método de los mínimos cuadrados se puede extender para
# obtener estimaciones de estos parámetros que minimizan la suma
# los cuadrados de las distancias del hiperplano a los datos (en todas
# las dimensiones). 
# Es decir, se obtienen los coeficientes b0, b1, b2, ..., bk que son
# estimaciones de los parámetros ??0, ??1, ??2, ..., ??k, de tal
# forma que si 
# 
#     Yi_est = b0 + b1·X1i + b2·X2i + ... + bk·Xki
# 
# estos coeficientes minimizan sum[(Yi - Yi_est)^2].
# 
# 
# Por ejemplo, si consideramos Género como otra variable predictora del
# peso (además de estatura):

d3 <- datos[, c("Weight", "Height")]
colnames(d3) <- c("Peso", "Estatura")
d3[["Género"]] <- factor(datos[["Gender"]], labels = c("Mujer", "Hombre"))

colors <- c("#E69F00", "#56B4E9")
colors <- colors[as.numeric(d3[[3]])]
scp3d <- scatterplot3d(
  x = d3[[2]],
  y = d3[[3]],
  z = d3[[1]],
  main ="Dispersión 3D para Peso",
  xlab = "Estatura (cm)",
  ylab = "Genero (1=M; 2=H)",
  zlab = "Peso (Kg)",
  pch = 16,
  color = colors,
  angle = 60
)

p3.1 <- ggscatter(
  d3, x = "Estatura", y = "Peso",
  color = "#6D9EC1", fill = "#6D9EC1", shape = 21, size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE, # Add confidence interval
  cor.coef = TRUE, # Add correlation coefficient.
  cor.coeff.args = list(method = "pearson", label.x = 150, label.sep = "\n")
)
print(p3.1)
# cat("\n"); stop("*** NO ERROR ***")

p3.2 <- ggscatter(
  d3, x = "Estatura", y = "Peso",
  color = "Género", fill = "Género", shape = 21, size = 3
)
print(p3.2)
# cat("\n"); stop("*** NO ERROR ***")

p3.3 <- ggscatter(
  d3, x = "Estatura", y = "Peso",
  color = "Género", fill = "Género", palette = "jco",
  shape = "Género", size = 3,
  add = "reg.line",
  conf.int = TRUE
)
p3.3 <- p3.3 + stat_cor(aes(color = Género), label.x = 150) 
print(p3.3)
# cat("\n"); stop("*** NO ERROR ***")

# 
# Las pendientes parecen ser algo diferentes. Para los hombres es menos
# probable que la pendiente que se observa sea producto del azar
#
# Obtengamos la combinación lineal con estas tres variables:

m3 <- lm(Peso ~ Estatura + Género, d3)

# Y comparémoslo con el modelo que teníamos con una variable 
m2 <- lm(Peso ~ Estatura, d3)

cat("\n")
cat("Modelo antiguo\n")
cat("--------------\n")
print(summary(m2))
cat("\n")
cat("Modelo nuevo\n")
cat("------------\n")
print(summary(m3))

# Podemos graficar este plano !!
scp3d$plane3d(m3)

# cat("\n"); stop("*** NO ERROR ***")

# Notemos que los (primeros dos) coeficientes son distintos en ambos
# modelos:
cat("\n")
cat("Coeficientes:\n")
cat("-------------\n")
print(coef(m2))
print(coef(m3))

# Luego, qué predictores se escojen para un modelo tiene gran impacto
# en estos valores y, por lo tanto, en la explicación o capacidad de
# predicción que podamos obtener.

# Hay que notar que los ??'s, y sus estimaciones b's, tienen apellido:
# "parciales".
# ??_i no es lo mismo que el ?? que resulta de la regresión de Y en
# Xi.
# Conceptualmente, un coeficiente de regresión parcial, de una regresión
# múltiple, es una medida del cambio de la media de la variable Y
# asociado a un incremento de una unidad en la variable predictora
# correspondiente, *manteniendo constante* todas las otras variables.
#
# Similarmente, los coeficientes de correlación parciales tienen sus
# complicaciones. Esto hace necesario adaptar R^2.
# Primero, hablaremos de "R^2 múltiple", que se obtiene como el
# cuadrado de la correlación entre los valores de Y observados y los
# valores de Y estimados por el modelo de regresión múltiple.
# Su interpretación es equivalente al R^2 de la regresión lineal: la
# cantidad de variación en la variable dependiente que es explicada por
# el modelo.
#
# Claro que esta medida tiene un problema: cuando se agrega una variable
# a un modelo, R^2 siempre aumenta.
# Si queremos decidir cuál modelo es mejor entre varios modelos, como m2
# y m3 en nuestro ejemplo, R^2 no sería de mucha utilidad: siempre se
# inclinaría por el modelo con más predictores.
# 
# Por eso, R^2 se "ajusta" de acuerdo a los grados de libertad
# del modelo, que se reducen al incluir más variables (más parámetros
# ??). Claro que hay muchas formas de ajustar R^2, y ¡con varios
# nombres distintos cada fórmula! Por ejemplo: 
# [Siguiendo los nombres en P Yin, X Fan (2001). Estimating R^2 shrinkage
# in multiple regression: A comparison of different analytical methods.
# The Journal of Experimental Education 69(2), 203-224.]
#
# Smith: [1-(1-R^2)·N/(N-v-1)]
# Wherry fórmula-1: [1-(1-R^2)·(N-1)/(N-v-1)]
# Wherry fórmula-2: [1-(1-R^2)·(N-1)/(N-v)]
# 
# R reporta R^2 ajustado con la fórmula 1 de Wherry, como medida de la
# pérdida de poder predictivo de un modelo (o "shrinkage" en inglés).
# Así, mientras R^2 nos dice cuánta varianza en Y es explicada por el
# modelo de regresión múltiple construido con nuestra muestra,
# R^2 ajustado nos dice cuánta varianza de Y sería explicada por un
# modelo hecho con la población subyacente usando las mismas variables
# predictoras
# 
# Así, podemos comparar ambos modelos:
cat("\n")
cat("R^2 ajustados:\n")
cat("--------------\n")
cat("Modelo con 2 predictores:", summary(m2)$adj.r.squared, "\n")
cat("Modelo con 3 predictores:", summary(m3)$adj.r.squared, "\n")

# cat("\n"); stop("*** NO ERROR ***")

#
# Sin embargo, este ajuste ha sido criticado porque no da información
# de qué tan bueno sería la predicción del modelo (i.e. del conjunto de
# variables predictivas que se está usando) con una muestra distinta de
# la misma población subyacente.
# Se han propuesto ajustes que intentan responder esto, como por ejemplo:
#
# Lord fórmula-1: [1-(1-R^2)(N+v-1)/(N-v-1)]
# Stein: [1-(1-R^2)((N-1)/(N-v-1))((N-2)/(N-v-2))((N+1)/N)]
#
# que estiman un parámetro conocido como Coeficiente de Validación
# Cruzada.
#
sm2 <- summary(m2)
sm3 <- summary(m3)
N <- nrow(d3)
r2 <- sm2[["r.squared"]]; 
v <- 1
stein.adj.r2.m2 <- 1 - (1 - r2^2) * ((N - 1) / (N - v - 1)) *
  ((N - 2) / (N - v - 2)) * ((N + 1) / N)
r2 <- sm3[["r.squared"]]
v <- 2
stein.adj.r2.m3 <- 1 - (1 - r2^2) * ((N - 1) / (N - v - 1)) *
  ((N - 2) / (N - v - 2)) * ((N + 1) / N)
cat("\n")
cat("R^2 ajustado según la fórmula de Stein (validación cruzada):\n")
cat("------------------------------------------------------------\n")
print(stein.adj.r2.m2)
print(stein.adj.r2.m3)

# cat("\n"); stop("*** NO ERROR ***")

# Pero también existen otras medidas para evaluar modelos que también
# consideran parsimonia. Dos medidas bien populares son:
# - Akaike's Information Criterion (AIC)
# - Schwarz's Bayesian Information Criterion (BIC, a veces SBC)
cat("\n")
cat("AIC:\n")
cat("----\n")
print(AIC(m2, m3))
cat("\n")
cat("BIC:\n")
cat("----\n")
print(BIC(m2, m3))

# ¿Qué significa que estas medidas consideran la parsimonia del modelo?
# ¿Navaja de Ockham? - otro filme: Contact (1997)
# Note: entre más bajo, mejor es el modelo.

# En este caso, parece que agregar la variable "Género" al modelo no
# mejora mucho su nivel predictivo. ¿Deberíamos agregarla? ¿Y si tenemos
# otras variables?

# De hecho, los datos de "body" tiene 24 posibles variables predictivas.
# ¿Hay que hacer un modelo para cada par de dos variables? ¡!
# ¿Y luego probar con todos los tríos?
#
# Un par de décadas atrás era impensado. Hoy en día, no es tan ridículo
# (a menos que hablemos de "Big Data").
#
#
# Pero la recomendación más honesta es: agregar un predictor si hay una
# buena razón (¿teórica?) para hacerlo.
#
# Con los computadores, y R, existe la tentación de incluir cientos de
# predictores y esperar que salga bien. Pero este no es el mejor método.
#
# Además, está el tema del orden en que las variables predictoras se
# incluyen en el modelo. Si estas variables están correlacionadas,
# esto tiene un gran impacto en los resultados.