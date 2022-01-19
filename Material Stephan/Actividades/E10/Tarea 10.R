
# Utilizando metodos de remuestreo y metodos robustos para abordar datos problematicos.
# Se proponen preguntas de investigacion originales que involucren:

# 1) La comparacion de las medias de dos grupos independientes.
# 2) La comparacion de mas de dos medias independientes.

# Nuestra preguntas son las siguientes:

# P1.- La media de los ingresos de las mujeres solteras es el mismo tanto 
# para la region de Valparaiso como para la region del Biobio.

# P2.- La media de las edades de las personas casadas es igual en las comunas de
# Maipu, Macul y Ñuñoa.

# Cargamos el arvhico csv para guardarlo en la variable "datos"
# Entre parentesis poner el directorio del archivo.csv
datos <- read.csv2("/Users/Downloads/Datos-Casen-v2.csv", stringsAsFactors = F)

# Fijamos el tamaño de la poblacion en estudio, contando la cantidad de columnas
tamaño.poblacion <- nrow(datos)

# Fijamos una semilla para randomizar la seleccion de nuestra muestra y su tamaño
semilla <- 113
tamaño.muestra <- 300
set.seed(semilla)

# Fijamos la muestra
muestra <- datos[sample(1:tamaño.poblacion,tamaño.muestra),]


# Para comparar medias de dos grupos independientes

# Es decir, para responder a la P1:

muestraP1 <- data.frame(muestra$sexo,muestra$ecivil,muestra$region)

colnames(muestraP1)[1] <- "sexo"
colnames(muestraP1)[2] <- "estado_civil"
colnames(muestraP1)[3] <- "region"

muestraP1 <- subset(muestraP1, sexo == "Mujer" & estado_civil == "Soltero(a)")
muestraP1 <- subset(muestraP1, region == "Regi¢n de Valpara¡so" | region == "Regi¢n del Biob¡o")






# Para comparar mas de dos medias independientes

# Es decir, para responder a la P2:

muestraP2 <- data.frame(muestra$edad, )

colnames(muestraP1)[1] <- "edad"
colnames(muestraP1)[2] <- ""



