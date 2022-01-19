
# Actividad: practicando ANOVA con muestras correlacionadas

# Actividad 1
# El siguiente código R define los datos que aparecen en una tabla que 
# compara las mejores soluciones encontradas por cuatro algoritmos para 
# instancias del problema del vendedor viajero con solución óptima conocida, 
# tomados desde una memoria del DIINF. Con estos datos responda la pregunta
# de investigación: ¿Hay algoritmos mejores que otros?

# H0: La eficiencia promedio es igual para los 18 algoritmos.
# HA: La eficiencia promedio es diferente para los 18 algoritmos.

library ( ggpubr )
library ( ez )

# Crear el data frame.  
texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 29 16.6 19.3 20.2 22
'brock400_4' 33 16.8 19.3 20.4 22
'C2000.9' 80 51.2 59.6 62.4 66
'c-fat500-10' 126 125 125 125 126
'hamming10-2' 512 243.2 419.1 422.4 512
'johnson32-2-4' 16 16 16 16 16
'keller6' 59 34.5 43 45.5 48.2
'MANN_a81' 1100 1082.2 1082.2 1082.2 1095.9
'p-hat1500-1' 12 6.9 8.1 8.9 10
'p-hat1500-3' 94 42.8 77.7 84.6 86
'san1000' 15 7.6 7.6 7.7 10
'san400_0.7_1' 40 19.6 20.5 20.5 21
'san400_0.9_1' 100 44.1 54.4 56.4 92
'frb100-40' 100 66.4 76.7 80.5 82
'frb59-26-1' 59 39.2 45.9 48.3 48
'1et.2048' 316 232.4 268.4 280.9 292.4
'1zc.4096' 379 253.8 293.2 307.4 328.5
'2dc.2048' 24 15.6 18.7 19.9 21
")
datos <- read.table(textConnection(texto), header = TRUE)


datos$Instancia <- factor(datos$Instancia)

Instancia <- rep(datos$Instancia, 4)
Algoritmos <- rep(c("R","R2","R3","G"), each=18)
Algoritmos <- factor(Algoritmos)
Eficiencia <- c((datos$R-datos$Optimo)/datos$Optimo,
                (datos$R2-datos$Optimo)/datos$Optimo, 
                (datos$R3-datos$Optimo)/datos$Optimo,
                (datos$G-datos$Optimo)/datos$Optimo)
datosFinal <- data.frame(Instancia , Algoritmos, Eficiencia)

# Comprobción de normalidad .
g <- ggqqplot(datosFinal ,
              x = "Eficiencia" ,
              y = "Algoritmos" ,
              color = "Algoritmos")

g <- g + facet_wrap(~ Algoritmos)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove ("axis.title")
print(g) 

# Procedimiento ANOVA con aov .
prueba <- aov ( Eficiencia ~ Algoritmos + Error (Instancia/(Algoritmos)),
                data = datosFinal)

cat ("\nResultado de la prueba ANOVA para muestras correlacionadas con aov\n")
print(summary(prueba))

# Procedimiento ANOVA con ezANOVA () .
prueba2 <- ezANOVA (data = datosFinal ,
                    dv = Eficiencia ,
                    within = Algoritmos ,
                    wid = Instancia ,
                    return_aov = TRUE )

cat ( "\n\nResultado de la prueba ANOVA para muestras correlacionadas con ezANOVA\n" )
# print(prueba2)
print (summary(prueba2$aov))

# Grafico del tamaño del efecto.
g2 <- ezPlot(data = datosFinal ,
             dv = Eficiencia ,
             wid = Instancia ,
             between = Algoritmos ,
             y_lab = "Eficiencia" ,
             x = Algoritmos)
print (g2) 

# Dado que el valor p obtenido como resultado del procedimiento ANOVA es
# igual a 1,01*10^9 es significativamente menor al nivel de significacion 
# igual a 0,01, por lo que se se podría rechazar la hipotesis nula a favor 
# de la hipesis alternativa. Pero primero hay que verificar que se cumpla
# la condicion de esfericidad.

cat ( "\n\nPero ezANOVA entrega más informació n.\n")
cat ( "El resultado de la prueba de esfericidad de Mauchly :\n\n")
print ( prueba2$`Mauchly's Test for Sphericity`)

# Al realizar la prueba de esfericidad de Mauchly se obtiene un valor p 
# muy pequeño p=3,879745*10^9, por lo que no se cumple con la condicion 
# de esfericidad y se debe realizar una correccion.

cat ("\n\nFactores de corrección dado que no se cumple la ")
cat (" condición de esfericidad :\n\n")
print (prueba2$`Sphericity Corrections`)

# De la correción de esfericidad se obtiene un nuevo valor p igual a 
# 2,09807*10^-6, el cual continua siendo significativamente menor al nivel 
# de significacion, por lo que se rechaza la hipotesis nula a favor de la
# hipotesis alternativa. De esta forma se concluye con un 99% de confianza
# que existen diferencias significativas en las eficiencias promedio entre 
# al menos 2 de los 4 algoritmos comparados.

# Respondiendo a la pregunta propuesta ¿Hay algoritmos mejores que otros?
# Si, existen mejores algoritmos que otros en terminos de eficiencia. 

# Análisis post-hoc:

# Procedimiento post-hoc de Bonferroni.
bonferroni <- pairwise.t.test(Eficiencia,
                              Algoritmos,
                              p.adj = "bonferroni", 
                              paired = TRUE)
cat("Corrección de Bonferroni:\n")
print(bonferroni)

# Según la correccion de Bonferroni al comparar directamente con el nivel 
# de significación original igual a 0.025, se puede concluir que con un
# 97,5% de confianza que todos los algoritmos tienen eficiencias promedio
# distintas, con la excepción del par R3/G que tienen la misma eficiencia
# promedio (cerca del borde).


# Actividad 2
# El siguiente es (un resumen de) la descripción de un famoso experimento:
# Naming the ink color of color words can be difficult. For example, if 
# asked to name the color of the word "blue" is difficult because the 
# answer (red) conflicts with the word "blue." This interference is called
# "Stroop Interference" after the researcher who first discovered the 
# phenomenon. This case study is a classroom demonstration. Students in an
# introductory statistics class were each given three tasks. In the "words" 
# task, students read the names of 60 color words written in black ink; in the "color" task, students named the
# colors of 60 rectangles; in the "interference" task, students named the 
# ink color of 60 conflicting color words. The times to read the stimuli 
# were recorded. There were 31 female and 16 male students.
# El siguiente código R define los datos que se obtuvieron en este estudio. 
# Con estos datos, responda la siguiente pregunta de investigación: 
# ¿Hay diferencias en los tiempos entre tareas y género?

texto2 <- ("
gender words colors interfer
1 19 15 31
1 21 20 38
1 9 25 38
1 21 19 32
1 16 15 29
1 16 16 36
1 17 23 34
1 21 19 44
1 9 14 42
1 23 17 37
1 18 19 31
2 26 24 33
2 18 19 44
2 17 21 31
2 12 5 44
2 21 17 35
2 19 21 34
2 16 15 32
2 13 22 47
2 13 24 29
2 15 19 38
2 15 26 42
")
datos2 <- read.table(textConnection(texto2), header = TRUE)
  
gender <- rep(datos2$gender, 3)

Tareas <- rep(c("words","colors","interfer"), each=22)

# Se crea variable Estudio ya que para anova se necesitan más de 3 grupos,
# y si se pensaba en los dos generos solo ingresarian 2 grupos al anova.
Estudio <- rep(c("1-words","2-words",
                 "1-colors","2-colors",
                 "1-interfer","2-interfer"), each=11)


Tiempo <- c(datos2$words, datos2$colors, datos2$interfer)


Estudio <- factor(Estudio)
Tareas <- factor(Tareas)

datosFinal2 = data.frame(Estudio, Tareas, Tiempo)

# Se realizan cambios, obteniendo las medias de los tiempos de cada tarea,
# para poder realizar anova correctamente, sin repeticiones de la instancia, 
# que en este caso es la variable Estudio.
Estudio <- rep(c("1-words","2-words",
                 "1-colors","2-colors",
                 "1-interfer","2-interfer"))

Tareas <- rep(c("words","colors","interfer"), each=2)

media_1_words <- mean(datosFinal2$Tiempo[seq(1, 11, by=1)])
media_1_colors <- mean(datosFinal2$Tiempo[seq(12, 22, by=1)])
media_1_interfer <- mean(datosFinal2$Tiempo[seq(23, 33, by=1)])

media_2_words <- mean(datosFinal2$Tiempo[seq(34, 44, by=1)])
media_2_colors <- mean(datosFinal2$Tiempo[seq(45, 55, by=1)])
media_2_interfer <- mean(datosFinal2$Tiempo[seq(56, 66, by=1)])

TiempoMedio <- c(media_1_words, media_2_words,
                 media_1_colors, media_2_colors,
                 media_1_interfer, media_2_interfer)

Estudio <- factor(Estudio)
Tareas <- factor(Tareas)

datosFinales = data.frame(Estudio, Tareas, TiempoMedio)

# Comprobción de normalidad .
g3 <- ggqqplot(datosFinal2 ,
               x = "Tiempo" ,
               y = "Estudio" ,
               color = "Estudio")

g3 <- g3 + facet_wrap(~ Estudio)
g3 <- g3 + rremove("x.ticks") + rremove("x.text")
g3 <- g3 + rremove("y.ticks") + rremove("y.text")
g3 <- g3 + rremove ("axis.title")
print(g3) 

# Procedimiento ANOVA con aov .
prueba3 <- aov ( TiempoMedio ~ Tareas + Error (Estudio/(Tareas)),
                data = datosFinales)
cat ("\nResultado de la prueba ANOVA para muestras correlacionadas con aov\n")
print(summary(prueba3))

# Procedimiento ANOVA con ezANOVA () .
prueba4 <- ezANOVA (data = datosFinales ,
                    dv = TiempoMedio ,
                    within = Tareas ,
                    wid = Estudio ,
                    return_aov = TRUE )

cat ( "\n\nResultado de la prueba ANOVA para muestras correlacionadas con ezANOVA\n" )
print (summary(prueba4$aov))

# Por problemas de tiempo no se puso llevar a cabo el anova y por consiguiente 
# no se logró dar respuesta a la pregunta planteada. De todas formas se 
# comprendió el inconveniente y se podrá llevar a cabo fuera de la entrega.

# Grafico del tamaño del efecto.
g2 <- ezPlot(data = datosFinal2 ,
             dv = Tiempo ,
             wid = Estudio ,
             between = Tareas ,
             y_lab = "Tiempo" ,
             x = Tareas)
print (g2) 

