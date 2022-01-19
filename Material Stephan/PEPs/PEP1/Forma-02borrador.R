#Prueba escrita Nº 1
#Forma 02

library(ggplot2)
library(ggpubr)

# Indicar directorio
#dir <- "~/../Downloads"
dir <- "/Users/javie/Downloads"
basename <- "cardiacos.csv"
file <- file.path(dir, basename)
población <- read.csv(file = file, encoding = "UTF-8")

tamaño.población <- nrow(población)

# Primero se elige una muestra de forma aleatoria, pero fijando una
# semilla para el generador de secuencias pseudoaleatorias para poder
# repetir los resultados
semilla <- 203
tamaño.muestra <- 40

set.seed(semilla)
muestra <- población[sample(1:tamaño.población, tamaño.muestra), ]

datos <- muestra
datos$ingreso <- datos$ytot/1000

# H1: La edad promedio de los pacientes diagnosticados como enfermos es igual a 60,5 años.
# H0: La edad promedio de los pacientes diagnosticados como enfermos es menor a 60,5 años.

edadmuestra <- muestra$edad
diagnosticomuestra <- muestra$diagnostico


datos = data.frame(edadmuestra, diagnosticomuestra="Enfermo")

# EL valor p obtenido por la prueba T de StudentDe fue mayor a 0.05, por 
# lo que se rechaza la hipotesis nula a favor de la hipotesis alternativa y
# de acuerdo a la pregunta: ¿respaldan los datos obtenidos la hipótesis de
# un nuevo equipo de investigadores, quienes sugieren que la edad promedio 
# de lospacientes enfermos es menor a 60,5 años? Se puede asegurar con un 
# 95% de confianza que la edad promedio de los pacientes diagnosticados 
# como enfermos es menor a 60,5 años.
# El poder estadístico de la prueba realizada resulto igual a .
