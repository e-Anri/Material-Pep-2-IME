library(ggpubr)

# Indicar directorio
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

# Obtenemos estas dos variables para analizarlas en más detalle
estatura <- datos$Height
peso <- datos$Weight
N <- nrow(datos)

# Recordemos nuestras ideas en términos de modelos:
#
#    valor = (modelo) + error
#
# Inicialmente, modelo era la media de la población (??).
# Luego, en ANOVA, eran las medias de varias poblaciones (??_i)
# Ahora, tenemos la relación de dos variables: la media de Y dado un
# valor específico de X (??_Y|X, media condicional):
#
#     Y = (??0 + ??1 · ??_Y|X) + error
#
# ??0 y ??1 son los parámetros de una línea recta de regresión
# que relaciona los valores de X e Y (ordenada en el origen y pendiente
# respectivamente).

# Siguiendo el método de los mínimos cuadrados, podemos obtener
# estimaciones de estos parámetros siguiendo la recta que minimiza la
# suma de los cuadrados de las desviaciones de la recta a los datos:
#
#     Yi_est = (b0 + b1 · Xi) + error
#
# Es decir, minimizando: sum[(Yi - Yi_est)^2], se obtiene:
#
# b0 = Y.media - b1 * X_medio
# b1 = sum[(Y - Y_media) * (X - X_medio)] / sum[(X - X_medio)^2]
#
# Nota: Esto es equivalente a b1 = R · s_y / s_x
#
peso.medio <- mean(peso)
errores.peso.medio <- peso - peso.medio
estatura.media <- mean(estatura)
errores.estatura.media <- estatura - estatura.media
SP <- sum(errores.peso.medio * errores.estatura.media)
b1 <- SP / sum(errores.estatura.media ^ 2)
b0 <- peso.medio - b1 * estatura.media
cat("\n\n")
cat("(Intercept)\t\tHeight\n")
cat(b0, "\t\t", b1, "\n")

# Veamos esta línea
p <- ggscatter(
  datos, x = "Height", y = "Weight",
  color = "#6D9EC1", fill = "#6D9EC1", shape = 21, size = 3#,
)
p1 <- p + geom_abline(intercept = b0, slope = b1, color = "blue")
print(p1)

# cat("\n"); stop("*** NO ERROR ***")


# ¿Qué tan seguros estamos de estos parámetros?
# Asumiendo que error sigue una distribución normal con media cero y
# desviación estándar ??_error:
#
# Y ~ N(??_Y|X, ??_error),
#
# con errores estándares:
#
# b0 ~ N(??0, ??_error · sqrt(1 / n + X_medio^2 / SSx),
#
# b1 ~ N(??1, ??_error · sqrt(1 / SSx)
#
# donde SSx = sum[(X - X_medio)^2]
#
# ??1 representa el cambio en el valor Y que resulta por incrementar
# X en una unidad.
# Si las variables no están relacionadas (X no es buen predictor de Y),
# esperaríamos que al cambiar X, Y no cambie (el cambio en Y sea cero).
# Luego podemos probar la hipótesis ??1 = 0 (H0).
# Ya hemos hecho esto con una prueba T de Student. Como siempre:
#
#    estadístico_prueba =   varianza explicada por modelo  = efecto
#                         --------------------------------   ------
#                         varianza no explicada por modelo   error
#
#    estadístico_prueba = (valor_observado - valor_esperado)
#                         ----------------------------------
#                                 error estándar
#
#    estadístico_prueba = (parámetro_estimado - parámetro_modelo)
#                         ---------------------------------------
#                                 error estándar
#
# y en este caso
#
#    t = (b1 - ??1) / (??_error · sqrt(1 / SSx))
#      = b1 / sqrt(??^2_error / SSx)
#      = b1 / sqrt((SS_error / df_error) / SSx))
#      = b1 / sqrt(SS_error / (df_error · SSx))
#
peso.estimado <- b0 + b1 * estatura
SS.error <- sum((peso - peso.estimado) ^ 2)
SS.X <- sum(errores.estatura.media ^ 2)
SE.b1 <- sqrt(SS.error / ((N - 2) * SS.X))
cat("\n\n")
cat("Error estándar para el coeficiente de estatura (b1)\n")
print(SE.b1)
t.b1 <- (b1 - 0) / SE.b1
cat("\n\n")
cat("Estadístico t test para el coeficiente de estatura (b1)\n")
print(t.b1)
# Ahora buscamos el valor p
p.b1 <- 2 * (1 - pt(t.b1, N - 2))
cat("\n\n")
cat("P-valor del coeficiente de estatura (b1)\n")
print(p.b1)

# Lo anterior nos recuerda que la varianza corresponde a la media de los
# cuadrados, usando los grados de libertad adecuados.
#
# Inicialmente, tenemos la variaza total, que no es otra cosa que la
# varianza que observamos en la variable Y
SS.total <- SS.Y <- sum((peso - peso.medio) ^ 2)
DF.total <- DF.Y <- N - 1
MS.total <- VAR.Y <- SS.total / DF.total # = var(peso)

# Como lo hicimos con ANOVA, podemos separar esta varianza total en la
# varianza que explica el modelo y la varianza del término de error
# aleatorio (es decir, SS.error).
#
# SS_total = SS_regresión + SS_residuos
# SS_regresión = SS_total - SS_residuos
# SS_regresión = SS_total - SS_error
#
SS.regresion <- SS.total - SS.error

# Esto mide cuánto disminuye la varianza (en rigor, la suma de cuadrados)
# por usar regresión en vez de la media.
# Si lo expresamos en porcentaje:
cat("\n\n")
cat("Disminución en SS:\n")
cat(round(SS.regresion / SS.total * 100, 2), "%\n")


# Esto no es otra cosa que el coeficiente de determinación R^2.
# Lo que nos permite tener el coeficiente de correlación de Person (R)
R.squared <- SS.regresion / SS.total
R <- sqrt(R.squared)
cat("Coef. determinación: ", R.squared, "\n")
cat("Coef. correlación: ", R, "\n")

# Esta idea también nos permite visualizar una forma de evaluar qué tan
# bueno es el nuevo modelo. Repetimos:
#
#    estadístico_prueba =   varianza explicada por modelo  = efecto
#                         --------------------------------   ------
#                         varianza no explicada por modelo   error
#
#    F = varianza_regresión / varianza_error
#
# Para esto, también necesitamos separar los grados de libertad total
# para poder calcular las media de los cuadrados (varianzas) de la
# regresión y el error.
#
# DF_total = DF_regresión + DF_residuos
# DF_regresión = DF_total - DF_residuos
#
DF.error <- N - 2
DF.regresion <- DF.total - DF.error

MS.error <- SS.error / DF.error
MS.regresion <- SS.regresion / DF.regresion

F <- MS.regresion / MS.error

cat("\n\n")
cat("Estadístico de prueba F\n")
print(F)
# Para el cual podemos estimar su nivel significación estadística
# (y compararlo con un alpha prederminado, como .05)
cat("P-valor para este F\n")
print(1 - pf(F, df1 = DF.regresion, df2 = DF.error))

# cat("\n"); stop("*** NO ERROR ***")


# Claro que esto, y más, se obtiene en R fácilmente
#
# la función lm() (y otras más) de R obtiene la regresión lineal con
# el método de los mínimos cuadrados.
recta <- lm(Weight ~ Height, datos)
cat("\n\n")
cat("Regresión lineal con mínimos cuadrados\n")
cat("--------------------------------------\n")
print(recta)

# Y podemos obtener las estadísticas de esta regresión
cat("\n\n")
cat("Resumen de la regresión\n")
cat("-----------------------\n")
print(summary(recta))


# También es simple obtener el gráfico
# plm <- ggplot(datos, aes(x = Height, y = Weight))
# p <- p + geom_point()
p2 <- p + geom_smooth(method = lm, se = FALSE)
print(p2)

# Incluso se puede obtener una visión del intervalo de confianza
# (95% by default) de la pendiente
p3 <- p + geom_smooth(method = lm, se = TRUE)
print(p3)

# Claro que todo esto puede obtenerse con una sola llamada a la
# biblioteca ggpubr (además de los coeficientes:
p4 <- ggscatter(
  datos, x = "Height", y = "Weight",
  color = "#6D9EC1", fill = "#6D9EC1", shape = 21, size = 3,
  add = "reg.line",
  add.params = list(color = "blue", fill = "lightgray"),
  conf.int = TRUE, # Add confidence interval
  cor.coef = TRUE, # Add correlation coefficient.
  cor.coeff.args = list(method = "pearson", label.x = 150, label.sep = "\n")
)
print(p4)

