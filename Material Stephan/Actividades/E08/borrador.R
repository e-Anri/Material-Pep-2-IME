#gender <- rep(datos2$gender, 2)
#gender <- factor(gender)
#Estudio <- rep(c("words","colors"), each=22)
#Estudio <- factor(Estudio)
#Tiempo <- c((datos2$words-datos2$interfer)/datos$interfer,
#            (datos2$colors-datos2$interfer)/datos2$interfer)
#datosFinal2 <- data.frame(gender, Estudio, Tiempo)

gender <- rep(datos2$gender, 3)
Tareas <- rep(c("words","colors","interfer"), each=22)
Tiempo <- c(datos2$words, datos2$colors, datos2$interfer)

gender <- factor(gender)
Tareas <- factor(Tareas)

instancia <- factor(seq(1, 66, by=1))

datosFinal2 = data.frame(instancia, gender, Tareas, Tiempo)

library(tidyr)
datos2$gender <- factor(datos2$gender)
datosFinal2 <- pivot_longer(datos2,
                            cols = c("words","colors","interfer"),
                            names_to = "Tareas",
                            values_to = "Tiempo")


# Comprobción de normalidad .
g3 <- ggqqplot(datosFinal2 ,
               x = "Tiempo" ,
               #y = "Tareas" ,
               color = "Tareas")
g3 <- ggqqplot(datosFinal2 ,
               x = "Tiempo" ,
               y = "gender" ,
               color = "gender")

#g3 <- g3 + facet_wrap(~ datosFinal2$Tareas)
g3 <- g3 + facet_wrap(~ gender)
g3 <- g3 + rremove("x.ticks") + rremove("x.text")
g3 <- g3 + rremove("y.ticks") + rremove("y.text")
g3 <- g3 + rremove ("axis.title")
print(g3) 

# Procedimiento ANOVA con aov .
prueba3 <- aov ( Tiempo ~ Tareas + Error (gender/(Tareas)),
                 data = datosFinal2)
prueba3 <- aov ( Tiempo ~ gender + Error (instancia/(gender)),
                 data = datosFinal2)