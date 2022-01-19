library(ggplot2)
library(ggpubr)

# Se propone o una hipótesis nula y una hipótesis alternativa 
# H0: media =  170
# H1: media != 170

#pnorm ( q_critico_inferior,mean = media_efecto, sd = SE, lower.tail = TRUE)
#pnorm (q=172,              mean = 171,          sd = 5, lower.tail = TRUE)

#qnorm ( alfa / 2, mean = media_nula, sd = SE, lower.tail = FALSE )
#qnorm ( 0.05 / 2, mean = 173       , sd = 7)


#Parametros conocidos.
sigma <- 10
alfa <- 0.05
n <- 25

# Cálculo del error estandar.
SE <- sigma / sqrt( n )

# Grafico de la distribucion de la diferencia de medias si la diferencia
# fuera nula , con regiones de rechazo de la hipotesis nula .
media_nula <- 0
Z_critico <- qnorm ( alfa / 2, mean = media_nula, sd = SE, lower.tail = FALSE )
q_critico_inferior <- media_nula - Z_critico
q_critico_superior <- media_nula + Z_critico

x <- seq ( -6 * SE, 4 * SE, 0.01)
y <- dnorm (x, mean = media_nula, sd = SE)
df <- data.frame (x, y)

g <- ggplot ( data = df, aes(x))

g <- g + stat_function(
  fun = dnorm ,
  args = list ( mean = media_nula, sd = SE),
  colour = " red ", size = 1)

g <- g + ylab ( " " )
g <- g + scale_y_continuous ( breaks = NULL )
g <- g + scale_x_continuous ( name = "Diferencia en tiempos de ejecución [ ms ]",
                              breaks = seq (-6, 4, 2))

g <- g + theme_pubr ()

g <- g + geom_area(data = subset(df, x < q_critico_inferior),
                    aes(y = y),
                    colour = "red",
                    fill = "red",
                    alpha = 0.5)

g <- g + geom_area(data = subset(df, x > q_critico_superior),
                    aes(y = y),
                    colour = "red",
                    fill = "red",
                    alpha = 0.5)

print (g) # Figura 7.5

# Gráfico de la distribución de la diferencia de medias si la diferencia
# fuera -4.
media_efecto <- -4

x1 <- seq(1 * SE, 200 * SE, 0.01)
y1 <- dnorm(x, mean = media_efecto, sd = SE)
df1 <- data.frame(x1, y1 )

g <- g + stat_function (
  fun = dnorm ,
  args = list (mean = media_efecto, sd = SE),
  colour = "blue", size = 1)
  
g <- g + ylab ( " " )

g <- g + geom_area (data = subset ( df1 , x < q_critico_inferior),
                    aes ( x = x1, y = y1),
                    colour = "blue",
                    fill = "blue",
                    alpha = 0.05)


g <- g + geom_area (data = subset ( df1 , x > q_critico_superior),
                    aes ( x = x1, y = y1),
                    colour = "blue",
                    fill = "blue",
                    alpha = 0.05)

print ( g ) # Figura 7.6

# Cálculo del poder de acuerdo al análisis teórico .
poder <- pnorm ( q_critico_inferior,
                 mean = media_efecto,
                 sd = SE ,
                 lower.tail = TRUE)
+ pnorm (q_critico_superior ,
         mean = media_efecto ,
         sd = SE ,
         lower.tail = FALSE )

print ( poder )

# Cálculo del poder usando la función power.t.test().
poder_1 <- power.t.test ( n = 25 ,
                          delta = 4,
                          sd = 10,
                          sig.level = 0.05 ,
                          type = "paired" ,
                          alternative = "two.sided")

print(poder_1)

# Cálculo del poder usando la función pwr.t.test().
library(pwr)

d_cohen <- (4 / sigma) * ((n - 2) / (n - 1.25))

poder_2 <- pwr.t.test(n = 25,
                      sig.level = alfa ,
                      type = "paired",
                      alternative = "two.sided",
                      d = d_cohen)

print(poder_2)

