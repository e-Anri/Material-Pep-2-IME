
# Actividad: ANOVA para muestras independientes

# Problema B
# El conjunto de datos birthwt del paquete MASS contiene registros de 189 
# nacimientos en un centro médico de Springfield, Massachusetts, en 1986. 
# Estos registros incluyen varias variables interesantes, entre ellas la 
# raza de la madre y el peso de los bebés (en libras).

# H0: El peso de los bebes promedio es igual para las tres razas de sus madres.
# H1: El peso de los bebes promedio es diferente para al menos una raza de sus madres.

library (ggplot2)
library (ggpubr)
library (ez)

# Se importa el conjunto de datos birthwt del paquete MASS 
registroNacimintos = data.frame(MASS::birthwt)
str(registroNacimintos)

Raza_Madre <- registroNacimintos$race
Peso_Bebe <- registroNacimintos$bwt

Raza_Madre <- factor(Raza_Madre)
instancia <- factor(seq(1, 189, by=1))

datos = data.frame(instancia, Raza_Madre, Peso_Bebe)

# Comprobcion de normalidad.
g <- ggqqplot ( datos ,
                x = "Peso_Bebe" ,
                y = "Raza_Madre" ,
                color = "Raza_Madre")

g <- g + facet_wrap (~ Raza_Madre)
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print (g) 
# 1 = white, 2 = black, 3 = other

# Procedimiento ANOVA con aov().
prueba <- aov(Peso_Bebe ~ Raza_Madre , data = datos)
print (summary (prueba))

# Procedimiento ANOVA con ezANOVA().
prueba2 <- ezANOVA (data = datos,
                    dv = Peso_Bebe,
                    wid = instancia,
                    between = Raza_Madre,
                    return_aov = TRUE )
print (prueba2)
summary(prueba2[["aov"]])

# Grafico del tamaño del efecto.
g2 <- ezPlot(data = datos ,
             dv = Peso_Bebe ,
             wid = instancia ,
             between = Raza_Madre ,
             x_lab = "Raza de la madre",
             y_lab = "Peso bebes al nacer [gr]" ,
             x = Raza_Madre)
print (g2) 

# Debido a que el valor p = 0.00834 es menor al nivel de significacion igual
# a 0.05, se rechaza la hipotesis nula a favor de hipotesis alternativa por 
# lo que se puede decir con un 95% de confianza que el peso de los bebes 
# promedio es diferente para al menos una raza de sus madres.


# Establecer nivel de significacion.
alfa <- 0.05

# Prueba HSD de Tukey para analisis post hoc.
post_hoc <- TukeyHSD(prueba ,
                     "Raza_Madre" ,
                     ordered = TRUE ,
                     conf.level = 1 - alfa)

print (post_hoc)

# En conclusión, el único valor p menor al nivel de significacion 0,025 
# corresponde a la diferencia 1-3 (white-other), siendo esta última la 
# única diferencia significativa.
