library(ggpubr)

# Indicar directorio
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
# print(str(datos))

# Tomamos la misma muestra con igual n�mero de varones y mujeres
i <- which(datos.todos[["Gender"]] == 0)
j <- which(datos.todos[["Gender"]] == 1)
set.seed(13)
N <- 15
i <- sample(i, N)
j <- sample(j, N)
datos <- datos.todos[c(i, j), ]

# Obtenemos estas dos variables para analizarlas en m�s detalle
estatura <- datos$Height
peso <- datos$Weight
N <- nrow(datos)

# Recordemos nuestras ideas en t�rminos de modelos:
#
#    valor = (modelo) + error
#
# Inicialmente, modelo era la media de la poblaci�n (??).
# Luego, en ANOVA, eran las medias de varias poblaciones (??_i)
# Ahora, tenemos la relaci�n de dos variables: la media de Y dado un
# valor espec�fico de X (??_Y|X, media condicional):
#
#     Y = (??0 + ??1 � ??_Y|X) + error
#
# ??0 y ??1 son los par�metros de una l�nea recta de regresi�n
# que relaciona los valores de X e Y (ordenada en el origen y pendiente
# respectivamente).

# Siguiendo el m�todo de los m�nimos cuadrados, podemos obtener
# estimaciones de estos par�metros siguiendo la recta que minimiza la
# suma de los cuadrados de las desviaciones de la recta a los datos:
#
#     Yi_est = (b0 + b1 � Xi) + error
#
# Es decir, minimizando: sum[(Yi - Yi_est)^2], se obtiene:
#
# b0 = Y.media - b1 * X_medio
# b1 = sum[(Y - Y_media) * (X - X_medio)] / sum[(X - X_medio)^2]
#
# Nota: Esto es equivalente a b1 = R � s_y / s_x
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

# Veamos esta l�nea
p <- ggscatter(
  datos, x = "Height", y = "Weight",
  color = "#6D9EC1", fill = "#6D9EC1", shape = 21, size = 3#,
)
p1 <- p + geom_abline(intercept = b0, slope = b1, color = "blue")
print(p1)

# cat("\n"); stop("*** NO ERROR ***")


# �Qu� tan seguros estamos de estos par�metros?
# Asumiendo que error sigue una distribuci�n normal con media cero y
# desviaci�n est�ndar ??_error:
#
# Y ~ N(??_Y|X, ??_error),
#
# con errores est�ndares:
#
# b0 ~ N(??0, ??_error � sqrt(1 / n + X_medio^2 / SSx),
#
# b1 ~ N(??1, ??_error � sqrt(1 / SSx)
#
# donde SSx = sum[(X - X_medio)^2]
#
# ??1 representa el cambio en el valor Y que resulta por incrementar
# X en una unidad.
# Si las variables no est�n relacionadas (X no es buen predictor de Y),
# esperar�amos que al cambiar X, Y no cambie (el cambio en Y sea cero).
# Luego podemos probar la hip�tesis ??1 = 0 (H0).
# Ya hemos hecho esto con una prueba T de Student. Como siempre:
#
#    estad�stico_prueba =   varianza explicada por modelo  = efecto
#                         --------------------------------   ------
#                         varianza no explicada por modelo   error
#
#    estad�stico_prueba = (valor_observado - valor_esperado)
#                         ----------------------------------
#                                 error est�ndar
#
#    estad�stico_prueba = (par�metro_estimado - par�metro_modelo)
#                         ---------------------------------------
#                                 error est�ndar
#
# y en este caso
#
#    t = (b1 - ??1) / (??_error � sqrt(1 / SSx))
#      = b1 / sqrt(??^2_error / SSx)
#      = b1 / sqrt((SS_error / df_error) / SSx))
#      = b1 / sqrt(SS_error / (df_error � SSx))
#
peso.estimado <- b0 + b1 * estatura
SS.error <- sum((peso - peso.estimado) ^ 2)
SS.X <- sum(errores.estatura.media ^ 2)
SE.b1 <- sqrt(SS.error / ((N - 2) * SS.X))
cat("\n\n")
cat("Error est�ndar para el coeficiente de estatura (b1)\n")
print(SE.b1)
t.b1 <- (b1 - 0) / SE.b1
cat("\n\n")
cat("Estad�stico t test para el coeficiente de estatura (b1)\n")
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
# varianza que explica el modelo y la varianza del t�rmino de error
# aleatorio (es decir, SS.error).
#
# SS_total = SS_regresi�n + SS_residuos
# SS_regresi�n = SS_total - SS_residuos
# SS_regresi�n = SS_total - SS_error
#
SS.regresion <- SS.total - SS.error

# Esto mide cu�nto disminuye la varianza (en rigor, la suma de cuadrados)
# por usar regresi�n en vez de la media.
# Si lo expresamos en porcentaje:
cat("\n\n")
cat("Disminuci�n en SS:\n")
cat(round(SS.regresion / SS.total * 100, 2), "%\n")


# Esto no es otra cosa que el coeficiente de determinaci�n R^2.
# Lo que nos permite tener el coeficiente de correlaci�n de Person (R)
R.squared <- SS.regresion / SS.total
R <- sqrt(R.squared)
cat("Coef. determinaci�n: ", R.squared, "\n")
cat("Coef. correlaci�n: ", R, "\n")

# Esta idea tambi�n nos permite visualizar una forma de evaluar qu� tan
# bueno es el nuevo modelo. Repetimos:
#
#    estad�stico_prueba =   varianza explicada por modelo  = efecto
#                         --------------------------------   ------
#                         varianza no explicada por modelo   error
#
#    F = varianza_regresi�n / varianza_error
#
# Para esto, tambi�n necesitamos separar los grados de libertad total
# para poder calcular las media de los cuadrados (varianzas) de la
# regresi�n y el error.
#
# DF_total = DF_regresi�n + DF_residuos
# DF_regresi�n = DF_total - DF_residuos
#
DF.error <- N - 2
DF.regresion <- DF.total - DF.error

MS.error <- SS.error / DF.error
MS.regresion <- SS.regresion / DF.regresion

F <- MS.regresion / MS.error

cat("\n\n")
cat("Estad�stico de prueba F\n")
print(F)
# Para el cual podemos estimar su nivel significaci�n estad�stica
# (y compararlo con un alpha prederminado, como .05)
cat("P-valor para este F\n")
print(1 - pf(F, df1 = DF.regresion, df2 = DF.error))

# cat("\n"); stop("*** NO ERROR ***")


# Claro que esto, y m�s, se obtiene en R f�cilmente
#
# la funci�n lm() (y otras m�s) de R obtiene la regresi�n lineal con
# el m�todo de los m�nimos cuadrados.
recta <- lm(Weight ~ Height, datos)
cat("\n\n")
cat("Regresi�n lineal con m�nimos cuadrados\n")
cat("--------------------------------------\n")
print(recta)

# Y podemos obtener las estad�sticas de esta regresi�n
cat("\n\n")
cat("Resumen de la regresi�n\n")
cat("-----------------------\n")
print(summary(recta))


# Tambi�n es simple obtener el gr�fico
# plm <- ggplot(datos, aes(x = Height, y = Weight))
# p <- p + geom_point()
p2 <- p + geom_smooth(method = lm, se = FALSE)
print(p2)

# Incluso se puede obtener una visi�n del intervalo de confianza
# (95% by default) de la pendiente
p3 <- p + geom_smooth(method = lm, se = TRUE)
print(p3)

# Claro que todo esto puede obtenerse con una sola llamada a la
# biblioteca ggpubr (adem�s de los coeficientes:
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

