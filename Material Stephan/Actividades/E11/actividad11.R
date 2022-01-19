
# Actividad: practicando regresión lineal múltiple

library(ggplot2)
library(ggpubr)
library(car)
library (scatterplot3d)
# Indicar directorio
dir <- "/Users/Desktop/"

basename <- "body.csv"
file <- file.path(dir, basename)
datos <- read.csv(file = file, sep = "")

mujeres <- which(datos[["Gender"]] == 0)
hombres <- which(datos[["Gender"]] == 1)

# Usando como semilla el a?o y mes de nacimiento de un/a miembro del equipo
set.seed(200003)

# Se selecciona una muestra de 50 hombres porque la semilla es impar
tamaño.muestra <- 50

muestra_i <- sample(hombres, tamaño.muestra)
muestra <- datos[muestra_i,]

#ESTE FUE NUESTRO MODELO DE PARTIDA FAVOR LLEGAR HASTA EL FINAL DEL CÃ“DIGO PARA
#VER LAS PRUEBAS APLICADAS DE CONFIANZA Y LLEGAR AL MODELO QUE FINALMENTE SE ESTABLECIÃ“

# Regresi?n lineal para predecir la altura de un hombre a partir 
# de dos variables.

# Una RLM con dos predictores para la altura de un hombre: la curvatura del 
# hombro (columna Shoulder.Girth) y la curvatura de la cintura 
# (columna Waist.Girth)

# Ajustar modelo usando validacion cruzada de 5 pliegues.
modelo <- lm ( Height ~ Shoulder.Girth + Waist.Girth, data = muestra )
print (summary(modelo))


# Graficar modelo ajustado .
g <- scatterplot3d( datos$Shoulder.Girth , datos$Waist.Girth , datos$Height , type = "p" ,
                        highlight.3d = TRUE ,
                        pch = 20 ,
                        xlab = "Height" ,
                        ylab = "Shoulder.Girth",
                        zlab = "Waist.Girth" )

g$plane3d( modelo , draw_polygon = TRUE , draw_lines = TRUE )
print ( g )

#Ya que creamos un modelo que nos pareciera correcto debemos analizar si las variables
#que elegimos son significativas o no para poder llegar a un modelo confiable
#Para esto nos ayudaremos de las funciones de R

#Creamos un modelo nulo
m_0 <- lm( Weight ~ 1, data = muestra)

#Creamos un modelo completo (full) para utilizar la función add1 en la consola y saber
# que variables son mas significativas

m_f <- lm(Weight ~ ., data = muestra)

#Usamos la funcion add1 con la prueba chi cuadrado en la consola
# add1(m_0, m_f, test = "Chisq")

#Ahora creamos m_1 con un "update" de m_0, le agregamos la variable Shoulder.Girth
m_1 <- update(m_0, ~ .+ Shoulder.Girth)

#add1(m_1, m_f, test = "Chisq") en consola

#Ahora, Para poder obtener claramente las variables mas significativas y que nuestro
#modelo sea confiable, utilizaremos un modelo escalondado, como vimos en el script en 
#Perusall, utilizando la funciÃ³n step para obtener iteraciones de add1 y asi encontrar
# las variables que mas nos sirven

m_a <- step(m_0, list(lower = m_0, upper = m_f), data = muestra)

#ploteamos este modelo
plot(m_a)

#Luego, este será nuestro nuevo modelo candidato de acuerdo a todas las iteraciones 
# de la función add1 

#Ahora haremos las pruebas correspondientes para ver si es confiable o no 
#Comenzamos con la prueba DurbinWatson

durbinWatsonTest(m_a)

#Ahora utilizamos la función summary para este modelo y obtenemos un p-value = 0,06
# Estamos al límite por lo que creemos que si es confiable, de todas maneras quitaremos
# un par de variables mas para ver si el p-valor aumenta

m_a2 <- update(m_a, ~ .- Calf.Maximum.Girth)

durbinWatsonTest(m_a2)
# p-valor = 0,07, es decir aumentÃ³
m_a3 <- update(m_a2, ~ .- Navel.Girth)

durbinWatsonTest(m_a3)
#p-valor = 0,356, aquí se disparó, por lo que nuestro modelo m_a si es confiable
# además eliminamos harta correlación en la última prueba
plot(m_a3)

#De igual forma ploteamos.

#Pero para comprobar que el modelo es confiable
# utilizaremos las siguientes condiciones

# Comprobar independencia de los residuos nuevamente mediante la prueba DurbinWatson.
#Obtenemos el p-valor = 0.048
cat("Prueba de Durbin -Watson para autocorrelaciones ")
cat("entre errores:\n")
print(durbinWatsonTest(m_a))

# Comprobar normalidad de los residuo mediante la prueba Shapiro
#Obtenemos el p-valor = 0.1846
cat("\nPrueba de normalidad para los residuos:\n")
print(shapiro.test(modelo$residuals))


# Comprobar homocedasticidad de los residuos con la prueba de la varianza no constante
cat("Prueba de homocedasticidad para los residuos:\n")
print(ncvTest(modelo))
#Obtenemos el p-valor = 0.91659

#Ahora analizamos el factor de inflación de la varianza
vif(m_a3)
#Vemos que no hay ninguna variable cercana a 10 
print(1/vif(m_a3))
#Ahora no hay nunguna variable cercana a 0,2 

#Tras utilizar VIF y ver que ninguna variable se acerca al umbral 10
#Y en el recÃ­proce de VIF se ve que ninguna variable es inferior al umbral 0,2
#POR LO QUE SI ES CONFIABLE EL MODELO m_a 

#Entonces:
#El modelo m_a predice el peso de un hombre de acuerdo al diÃ¡metro biliaco, curvatura
# de rodilla, diametro de muÃ±ecas, profundidad del pecho, diametro del pecho,
#curvatura de antebrazo, curvatura minima del tobillo, curvatura minima de la muÃ±eca
#diametro de los hombros, DiÃ¡metro bitrocantÃ©rico, curvatura del pecho, edad,
#diametro de tobillos, curvatura de cadera, curvatura de bicep, DiÃ¡metro biacromial.
