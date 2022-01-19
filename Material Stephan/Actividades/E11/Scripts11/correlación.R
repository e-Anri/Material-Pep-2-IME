library(ggpubr)

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
# print(str(datos))

# Tomamos una muestra con igual número de varones y mujeres
i <- which(datos.todos[["Gender"]] == 0)
j <- which(datos.todos[["Gender"]] == 1)
set.seed(13)
N <- 15
i <- sample(i, N)
j <- sample(j, N)
datos <- datos.todos[c(i, j), ]

# Veamos la distribución de estatura versus peso
p <- ggscatter(
  datos, x = "Height", y = "Weight",
  color = "#6D9EC1", fill = "#6D9EC1", shape = 21, size = 3#,
)
print(p)


# cat("\n"); stop("*** NO ERROR ***")


# Obtenemos estas dos variables para analizarlas en más detalle
estatura <- datos[["Height"]]
peso <- datos[["Weight"]]
N <- nrow(datos)

# Obtenemos las medias
estatura.media <- mean(estatura)
peso.medio <- mean(peso)

# Covarianza
cov.manual <- (estatura - estatura.media) * (peso - peso.medio)
cov.manual <- sum(cov.manual) / (N - 1)
cat("\n\n")
cat("Covarianza\n")
print(cov.manual)
print(cov(estatura, peso))

# Coeficiente de correlación: covarianza normalizada [-1, 1]
r.manual <- cov.manual / (sd(estatura) * sd(peso))
cat("\n\n")
cat("Coeficiente de correlación\n")
print(r.manual)
print(cor(estatura, peso))

# Ajuste para que coef. de cor. tenga una distrib. muestral aprox. normal
z.r <- (1 / 2) * log( (1 + r.manual) / (1 - r.manual) )
# Esta distrib. tiene error estándar conocido:
z.r.se <- 1 / sqrt(N - 3)
# Luego, el estadístico sería:
z <- z.r / z.r.se
# Calculamos su nivel de significanción (H0 sin dirección):
z.r.p <- 2 * (1 - pnorm(z))
cat("\n\n")
cat("P-valor del coeficiente de correlación (Z)\n")
print(z.r.p)

# También podemos obtener intervalos con 95% confianza
z.r.inf <- z.r + qnorm(2.5 / 100) * z.r.se
z.r.sup <- z.r + qnorm(97.5 / 100) * z.r.se
# Pero los tenemos que llevar al intervalo original [-1, 1]
r.inf <- (exp(2 * z.r.inf) - 1) / (exp(2 * z.r.inf) + 1)
r.sup <- (exp(2 * z.r.sup) - 1) / (exp(2 * z.r.sup) + 1)
cat("\n\n")
cat("Intervalo de 95% confianza para coeficiente de correlación (Z)\n")
cat("[", r.inf, ", ", r.sup, "]\n")


# Pero, en la práctica, se suele usar el estadístico t
t.r <- r.manual * sqrt(N - 2) / sqrt(1 - r.manual ^ 2)
# Calculamos su significanción (H0 sin dirección):
t.r.p <- 2 * (1 - pt(t.r, N - 2))
cat("\n\n")
cat("P-valor del coeficiente de correlación (t)\n")
print(t.r.p)

# Dejando que R lo haga por nosotros
cat("\n\n")
cat("Prueba para correlación de dos variables\n")
cat("----------------------------------------\n")
print(cor.test(estatura, peso))


# Esto es "Pearson's product-moment correlation", que es una medida
# paramétrica, que asume:
# - las variables tienen al menos escala de intervalo
# - las variables siguen aproximadamente una distribución normal
# - existe una relación lineal entre las variables
# - no existe (o se minimiza) presencia de valores atípicos (outliers)
# - hay homocedasticidad en los datos

# Si alguna de estas condiciones no se da, existen métodos no
# paramétricos como alternativa; entre los más usados:
# - Spearman's rho: r con el ranking de los datos
# - kendall's tau: ajueste que mejora problemas con empates
cat("\n\n")
cat("Pruebas no-paramétricas para correlación de dos variables\n")
cat("---------------------------------------------------------\n")
print(cor.test(estatura, peso, method = "spearman"))
print(cor.test(estatura, peso, method = "kendall"))


# Coefficiente de determinación: el cuadrado.
# Mide cuánto de la variación de una variable es compartida por la otra.
# R^2 aún no es "causalidad", pero está más cerca que R.
r.squared <- r.manual ^ 2
# También puede expresarse como porcentaje
cat("\n\n")
cat("Coefficiente de determinación (R^2)\n")
cat(round(r.squared * 100, 2), "%\n")

# R obtiene la recta con menor desviación cuadrada total (least squares)
recta <- lm(Weight ~ Height, datos)

cat("\n\n")
cat("Regresión lineal con mínimos cuadrados\n")
print(formula(recta))
print(coef(recta))

# Este método tiene las mismas suposiciones que el coeficiente de
# correlación de Pearson, aunque con una mirada un poco distinta pues
# lo que analiza son los residuos, y que OpenIntro resume en 7.2.2:
# linearity , nearly normal residuals, constant variability, independent
# observations.


# Aquí aparece  otra interpretación de R^2 con el modelo que hemos
# trabajo, i.e. la media
desviaciones.total <- peso - peso.medio
SS.T <- sum(desviaciones.total ^ 2)
# Con la regresión obtenida
peso.estimado <- coef(recta)[1] + coef(recta)[2] * estatura
desviaciones.recta <- peso - peso.estimado
SS.R <- sum(desviaciones.recta ^ 2)
# ¿Cuánto cambiaron las desviaciones con el nuevo modelo?
SS.M <- SS.T - SS.R
# ¿Y en porcentaje?
cat("\n\n")
cat("(SS.M / SS.T) por ciento\n")
cat(round(SS.M / SS.T * 100, 2), "%\n")
cat("\n\n")
cat("Coeficiente de correlación\n")
print(sqrt(SS.M / SS.T))
# Esto es R^2, la (usualmente) reducción porcentual de la variación de
# los datos al usar la variable independiente para predecir la variable
# dependiente.
# Así R es una medida la bondad del ajuste de la regresión,
# y R^2 una medida del tamaño de esta relación
cat("\n\n")