#Nombres:
#Estefanía Álvarez 20.371.287-1
#Felipe Cornejo 20.427.782-6
#David Morales 19.881.480-6
#Claudio Muñoz 20.003.395-7

library(tidyr)
library (ggpubr)
library(ggplot2)
library (dplyr)
library(pwr)
library (tidyverse)

# ////////////////////////// PREGUNTA 1 //////////////////////////

# Estudios previos habían determinado que la incidencia de cáncer oral en la población general que no fuma
# era de 20%. ¿Respaldan estos datos tal estimación?

# Formulación de Hipotesis:
# H0 : p = 20%
# HA : p <> 20%

# Fijar valores conocidos
n <- 85 # controles de gente no fumadora
p_exito <- 26/n
alfa <- 0.05
valor_nulo <- 0.2

# Construcción del intervalo de confianza
error_est <- sqrt((p_exito * (1 - p_exito)) / n)
Z_critico <- qnorml(alfa / 2, lower.tail = FALSE)
inferior <- p_exito - Z_critico * error_est
superior <- p_exito + Z_critico * error_est
cat (" Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")

# Prueba de hipótesis.
error_est_hip <- sqrt((valor_nulo * (1 - valor_nulo)) / n)
Z <- (p_exito - valor_nulo) / error_est_hip
p <- pnorm(Z, lower.tail = FALSE)
cat ("Hipó tesis alternativa unilateral \n")
cat ("Z =", Z, "\n")
cat ("p =", p)

# Por lo tanto, a partir del p obtenido [0,007] se puede ver que es mucho menor al valor de significación [0,05], entonces
# se puede rechazar la hipotesis nula a favor de la alternativa. Dando como resultado que estos 


# ////////////////////////// PREGUNTA 2 //////////////////////////

# Según estos datos, ¿da lo mismo no fumar que hacerlo diariamente consumiendo entre 1 y 19 cigarrillos? 

# Hipótesis
# H0 : pNo_fumador - pFumador[1-19] = 0
# HA : pNo_fumador - pFumador[1-19] <> 0

# Fijar valores conocidos NO Fumadores
n_NoF <- 85 # controles de gente no fumadora
n_F <- 97 # controles de gente fumadora [1 - 19 cigarros / día]
exito_NoF <- 26
exito_F <- 66
alfa <- 0.05
valor_nulo <- 0

# Probabilidades
p_exitoNoF <- exito_NoF / n_NoF
p_exitoF <- exito_F / n_F

# Estimar diferencia
diferencia <- abs(p_exitoF - p_exitoNoF)

# Construcción de intervalo de Confianza
error_NoF <- (p_exitoNoF * (1- p_exitoNoF)) / n_NoF
error_F <- (p_exitoF * (1- p_exitoF)) / n_F
error_est <- sqrt(error_NoF + error_F)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
inferior <- diferencia - Z_critico * error_est
superior <- diferencia + Z_critico * error_est
cat (" Intervalo de confianza = [", inferior , ", ", superior , "]\n", sep = "")

# Prueba de Hipótesis

p_agrupada <- (exito_NoF + exito_F) / (n_NoF + n_F)
error_NoF <- (p_agrupada * (1 - p_agrupada)) / n_NoF
error_F <- (p_agrupada * (1 - p_agrupada)) / n_F
error_est_hip <- sqrt( error_NoF + errorF )
Z <- (diferencia - valor_nulo) / error_est_hip
p <- 2 * pnorm(Z, lower.tail = FALSE)
cat ("Hipó tesis alternativa bilateral \n")
cat ("Z =", Z, "\n")
cat ("p =", p)

#Como el valor p [6,0030e-18] obtenido es significativamente menor al nivel de 
#significación [0.005], se rechaza la hipotesis nula en favor de la hipotesis 
#alternativa, concluyendo así que, según los datos no da lo mismo no fumar que 
#fumar entre 1 a 19 cigarrillos

# ////////////////////////// PREGUNTA 3 //////////////////////////

#Suponiendo que la diferencia en la proporción de personas que desarrollan la 
#enfermedad entre quienes no fuman y aquellos que fuman de 1 a 19 cigarrillos al
#día es de 0.2. ¿Cuánta gente deberíamos monitorear para obtener un intervalo de
#confianza del 95% y poder estadístico de 80%? si se intente #mantener 
#aproximadamente la misma proporción de gente estudiada en cada caso.

diferencia <- 0.2
int_conf <- 0.95
poder <- 0.8
n_NoF <- 85 # controles de gente no fumadora
n_F <- 97 # controles de gente fumadora [1 - 19 cigarros / día]
exito_NoF <- 26
exito_F <- 66

# Probabilidades
p_exitoNoF <- exito_NoF / n_NoF
p_exitoF <- exito_F / n_F

tam_efecto <- ES.h(p_exitoNoF,p_exitoF)

poder_estadistico <- pwr.2p.test(h = tam_efecto,
                                 n = NULL,
                                 sig.level = 1-int_conf,
                                 power = poder,
                                 alternative = "two.sided")

print(poder_estadistico)

#Suponiendo que la diferencia es de 20% entre ambas muestras, manteniendo las mismas proporciones
#se estima que es necesario monitoriar 27 personas para obtener un intervalo de confianza de 
# 95% y un poder de 80%.


