
# Pregunta de investigaci�n para la comparaci�n de las medias de dos grupos 
# independientes:
#   La media de los ingresos de las mujeres solteras es el mismo tanto para la 
# zona norte como para la zona sur.


library(readxl)
datos <- read_excel("C:/Users/Downloads/Datos-Casen-v2.xls")
#datos <- read.csv("C:/Users/javie/Downloads/Datos-Casen-v2.csv")


#Fijamos el tama�o de la poblacion en estudio, contando la cantidad de columnas
tama�o.poblacion <- nrow(datos)

#Fijamos una semilla para randomizar la seleccion de nuestra muestra y su tama�o
semilla <- 111
tama�o.muestra <- 300
set.seed(semilla)

#Fijamos la muestra
muestra <- datos[sample(1:tama�o.poblacion,tama�o.muestra),]

#zonaNorte <- c(muestra$region <- "Regi�n de Tarapac�",
#               muestra$region <- "Regi�n de Atacama")

muestraFinal <- data.frame(muestra$sexo <- "Mujer", muestra$region)

###############

muestraFinal <- muestraFinal[muestraFinal$region == "Regi�n de Valpara�so", muestraFinal$region == "Regi�n de Tarapac "]

muestraFinal <- data.frame(muestraFinal$sexo == "Mujer", 
                           muestraFinal$ecivil == "Soltero(a)",
                           muestraFinal$region == "Regi�n de Valpara�so | Regi�n de Tarapac") 

restaurant[restaurant[,target]==label,atributo]
muestra[muestra[,"sexo"]== "Mujer", "sexo"]
muestra[muestra[,"ecivil"]== "Soltero(a)r", "ecivil"]

#muestraFinal <- data.frame(split(muestra, f = muestra$sexo))


#muestraFinal <- subset(muestraFinal, region == "Regi�n de Valpara�so" | region == "Regi�n de Tarapac ")
#muestraFinal <- subset(muestraFinal, region == c("Regi�n de Valpara�so", "Regi�n de Tarapac "))


muestraFinal <- muestraFinal[muestraFinal$region == "Regi�n de Valpara�so" | muestraFinal$region == "Regi�n del Biob�o", ]

