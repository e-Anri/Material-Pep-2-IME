# PEP2 IME

# Indicar directorio
dir <- "/Users/javie/Downloads"

basename <- "datos-Forma03.csv"
file <- file.path(dir, basename)
datos <- read.csv(file = file, sep = ",")

set.seed(200003)
tamaño.muestra <- 500

#Fijamos el tamaño de la población en estudio, contando la cantidad de columnas
tamaño.poblacion <- nrow(datos)

#Fijamos una semilla para randomizar la selección de nuestra muestra y su tamaño
semilla <- 113
tamaño.muestra <- 300
set.seed(semilla)

#Fijamos la muestra
muestra <- datos[sample(1:tamaño.poblacion,tamaño.muestra),]