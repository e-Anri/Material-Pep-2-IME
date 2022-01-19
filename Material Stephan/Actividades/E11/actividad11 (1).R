
# Actividad: practicando regresión lineal múltiple

library(ggplot2)
library(ggpubr)

# Indicar directorio
dir <- "/Users/Downloads"

basename <- "body.csv"
file <- file.path(dir, basename)
datos <- read.csv(file = file, sep = "")

mujeres <- which(datos[["Gender"]] == 0)
hombres <- which(datos[["Gender"]] == 1)

# Usando como semilla el año y mes de nacimiento de un/a miembro del equipo
set.seed(200003)

# Se selecciona una muestra de 50 hombres porque la semilla es impar
tamaño.muestra <- 50

muestra_i <- sample(hombres, tamaño.muestra)
muestra <- datos[muestra_i, ]

# Regresión lineal para predecir la velocidad al correr de un hombre a partir 
# de dos variables.

# Una RLM con dos predictores para la velocidad al correr: el diametro de la 
# rodilla (columna Knees.diameter) y la circunferencia minima del tobillo 
# (columna Ankle.Minimun.Girth)

# Ajustar modelo usando validacion cruzada de 5 pliegues.
modelo <- lm ( Height ~ Shoulder.Girth + Waist.Girth, data = muestra )
print (summary(modelo))


library ( scatterplot3d)

# Graficar modelo ajustado .
g <- scatterplot3d( datos$Shoulder.Girth , datos$Waist.Girth , datos$Height , type = "p" ,
                        highlight.3d = TRUE ,
                        pch = 20 ,
                        xlab = " Peso [ lb x 1 0 0 0 ] " ,
                        ylab = " Rendimiento [ millas / gal ó n ] " ,
                        zlab = " 1 / 4 de milla [ s ] " )

g$plane3d( modelo , draw_polygon = TRUE , draw_lines = TRUE )
print ( g )


modelo <- lm ( Height ~ Shoulder.Girth, data = muestra )
print (summary(modelo))

g <- ggscatter(muestra,
          x = "Shoulder.Girth",
          y = "Height",
          add = "reg.line")
print(g)

modelo <- lm ( Shoulder.Girth ~ Height, data = muestra )
print (summary(modelo))

g <- ggscatter(muestra,
               x = "Height",
               y = "Shoulder.Girth",
               add = "reg.line")
print(g)

intercept <- mean(muestra[["Height"]])

g <- g + geom_abline(intercept = intercept, slope = 0)
print(g)

