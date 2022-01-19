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


# peso <- muestra[["Weight"]]
modelo_estatura <- lm(Weight ~ Height, muestra)

summary(modelo_estatura)

g <- ggscatter(muestra,
               x = "Height",
               y = "Weight",
               add = "reg.line")
print(g)

#plot(modelo_estatura)

intercept <- mean(muestra[["Weight"]])

g <- g + geom_abline(intercept = intercept, slope = 0)
print(g)

modelo_nulo <- lm(Weight ~ 1, muestra)

print(anova(modelo_nulo, modelo_estatura))

modelo_estatura_edad <- lm(Weight ~ Height + Age, muestra)
print(anova(modelo_estatura_edad, modelo_estatura))

modelo_full <- lm(Weight ~ ., muestra)
summary(modelo_full) # Relevantes para este modelo con *

add1(modelo_nulo, modelo_full)

drop1(modelo_full)

add1(modelo_estatura, modelo_full, test = "Chisq")
add1(modelo_estatura, modelo_full, test = "F")
