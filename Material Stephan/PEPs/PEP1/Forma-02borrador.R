#Prueba escrita N� 1
#Forma 02

library(ggplot2)
library(ggpubr)

# Indicar directorio
#dir <- "~/../Downloads"
dir <- "/Users/javie/Downloads"
basename <- "cardiacos.csv"
file <- file.path(dir, basename)
poblaci�n <- read.csv(file = file, encoding = "UTF-8")

tama�o.poblaci�n <- nrow(poblaci�n)

# Primero se elige una muestra de forma aleatoria, pero fijando una
# semilla para el generador de secuencias pseudoaleatorias para poder
# repetir los resultados
semilla <- 203
tama�o.muestra <- 40

set.seed(semilla)
muestra <- poblaci�n[sample(1:tama�o.poblaci�n, tama�o.muestra), ]

datos <- muestra
datos$ingreso <- datos$ytot/1000

# H1: La edad promedio de los pacientes diagnosticados como enfermos es igual a 60,5 a�os.
# H0: La edad promedio de los pacientes diagnosticados como enfermos es menor a 60,5 a�os.

edadmuestra <- muestra$edad
diagnosticomuestra <- muestra$diagnostico


datos = data.frame(edadmuestra, diagnosticomuestra="Enfermo")

# EL valor p obtenido por la prueba T de StudentDe fue mayor a 0.05, por 
# lo que se rechaza la hipotesis nula a favor de la hipotesis alternativa y
# de acuerdo a la pregunta: �respaldan los datos obtenidos la hip�tesis de
# un nuevo equipo de investigadores, quienes sugieren que la edad promedio 
# de lospacientes enfermos es menor a 60,5 a�os? Se puede asegurar con un 
# 95% de confianza que la edad promedio de los pacientes diagnosticados 
# como enfermos es menor a 60,5 a�os.
# El poder estad�stico de la prueba realizada resulto igual a .
