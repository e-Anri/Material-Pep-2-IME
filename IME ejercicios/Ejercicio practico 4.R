require(ggplot2)
require(ggpubr)
"
Ejercicio practico 4
"

"
ENUNCIADO

Se sabe que el proceso de fabricacion de barras de acero para concreto reforzado 
producen barras con medidas de dureza que siguen una distribucion normal con 
desviacion estandar de 10 kilogramos de fuerza por milimetro cuadrado. 
Usando una muestra aleatoria de tamanio 25, un ingeniero quiere averiguar si una 
linea de produccion esta generando barras con dureza media de 170 [kgf mm^-2].
"

"PROBLEMA C

Datos generales

n = 25
sd = 10 [kgf mm^-2]

Hipotesis
- H0: Dureza media de las barras de acero U = 170 [kgf mm^-2]
- H1: Dureza media de las barras de acero U != 170 [kgf mm^-2]

Delta = 2
Delta siendo la diferencia entre la media (170) con los parametros que el ingeniero 
tomara para rechazar la hipotesis nula

"

"
1. Si el ingeniero piensa rechazar la hipotesis nula si la muestra presenta una 
media menor a 168 [kgf mm^-2] o mayor a 172 [kgf mm^-2], pero la verdadera dureza 
media de la linea de produccion fuera 173 [kgf mm^-2], 
¿cual seria la probabilidad de que el ingeniero, que obviamente no conoce este dato, 
tome la decision correcta de rechazar la hipotesis nula en favor de la hipotesis alternativa?
  
"

poder1 <- power.t.test(n = 25, 
                       sd = 10, 
                       delta = 2, 
                       sig.level = 0.05, 
                       power = NULL, 
                       alternative = "two.sided", 
                       strict = TRUE, 
                       type = "one.sample")
print(poder1)

"
El ingeniero tiene alrededor de un 84% de probabilidad de no cometer 
Error tipo 1 (no rechace H0 en favor de H1)
"

"
2. ¿A cuánto cambiaria esta probabilidad si se pudiera tomar una muestra de 64 barras?
"
poder2 <- power.t.test(n = 64, 
                       sd = 10, 
                       delta = 2, 
                       sig.level = 0.05, 
                       power = NULL, 
                       alternative = "two.sided", 
                       strict = TRUE, 
                       type = "one.sample")
print(poder2)

"
La probabilidad cambiaria a un 65% de no cometer error tipo 1 ( 1 - 0.35)
"

"
3. ¿Cuantas barras deberian revisarse para conseguir un poder estadistico de 0,90 
y un nivel de significacion de 0,05?
"

poder3 <- power.t.test(n = NULL, 
                       sd = 10, 
                       delta = 2, 
                       sig.level = 0.05, 
                       power = 0.9, 
                       alternative = "two.sided", 
                       strict = TRUE, 
                       type = "one.sample")
print(poder3)

"
Para conseguir un poder estadistico de 0,90 y un nivel de significacion 0,05 se 
necesitan 264.6137 muestras, como son barras de acero y no existen porciones de 
tal, n deberia ser 265.
"

"
4. ¿Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error 
de tipo 1 a un 1% solamente?
" 

poder4 <- power.t.test(n = 25, 
                       sd = 10, 
                       delta = 2, 
                       sig.level = 0.01, 
                       power = NULL, 
                       alternative = "two.sided", 
                       strict = TRUE, 
                       type = "one.sample")
print(poder4)

"
Al bajar la probabilidad de cometer un error tipo 1 a un 1% solamente,existe 
un 95% de probabilidades de cometer error tipo 2.
"

n0 <- 25
u0 <- 170
sigma0 <- 10

x <- rnorm(n0, u0, sigma0)
y <- dnorm(x, mean = u0, sd= sigma0)
d <- data.frame(x,y)

p1 <- ggplot(data = d, aes(x))
p1 <- p1 + stat_function(fun = dnorm, 
                         n = 100, 
                         args = list(mean = u0, sd = sigma0), 
                         colour = "blue", 
                         size = 1)
print(p1)

p2 <- p1 + geom_area(data = subset(d, x > 172), 
                     aes(y = y), 
                     colour = "red", 
                     fill = "red", 
                     alpha = 0.5) + geom_area(data = subset(d, x < 168), 
                                              aes(y = y), 
                                              colour = "red", 
                                              fill = "red", 
                                              alpha = 0.5)
print(p2)

