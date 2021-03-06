# PEP2 IME

# Indicar directorio
dir <- "/Users/javie/Downloads"

basename <- "datos-Forma03.csv"
file <- file.path(dir, basename)
datos <- read.csv(file = file, sep = ",")

set.seed(200003)
tama�o.muestra <- 500

#Fijamos el tama�o de la poblaci�n en estudio, contando la cantidad de columnas
tama�o.poblacion <- nrow(datos)

#Fijamos una semilla para randomizar la selecci�n de nuestra muestra y su tama�o
semilla <- 113
tama�o.muestra <- 300
set.seed(semilla)

#Fijamos la muestra
muestra <- datos[sample(1:tama�o.poblacion,tama�o.muestra),]