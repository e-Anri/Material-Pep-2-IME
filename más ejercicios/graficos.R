library(ggpubr)

# Cargar conjunto de datos .
datos <- mtcars


# Histograma para la variable Rendimiento 
g1 <- gghistogram (datos,
                   x = "Rendimiento",
                   bins = 10,
                   add = "mean",
                   xlab = "Rendimiento [Millas/galón]",
                   ylab = "Frecuencia",
                   color = "blue",
                   fill = "blue")

print (g1)



# Histograma para la variable Potencia .
g2 <- gghistogram (datos,
                   x = "Potencia",
                   bins = 10,
                   add = "mean",
                   xlab = "Potencia [hp]",
                   ylab = "Frecuencia",
                   color = "red",
                   fill = "yellow")

print (g2)



g <- ggboxplot(datos[["Potencia"]],
                 color = "red",
                 fill = "pink",
                 ylab = "Potencia [hp]")

g <- g + rremove ("x.ticks")
g <- g + rremove ("x.text")
g <- g + rremove ("x.title")

print (g)




# Crear la tabla de frecuencias para la variable Cambios y convertirla a
# data frame .
contingencia <- as.data.frame ( xtabs (~ Cambios, data = datos ))

# Crear el gráico de barras .
g <- ggbarplot ( contingencia,
                 x = "Cambios",y = "Freq",
                 fill = c("brown", "purple", "orange"),
                 title = "Cantidad de cambios de los automóviles",
                 xlab = "Cantidad de cambios",
                 ylab = "Frecuencia")

print (g)



# Crear gráfico de torta 
g <- ggpie ( contingencia ,
             x = "Freq",
             label = "Cambios",
             fill = c("red", "yellow", "green"),
             title = "Cantidad de cambios de los automóviles",
             lab.pos = "in")

print (g)



# Crear gráfico de dispersión.
g <- ggscatter (datos ,
                x = "Rendimiento",
                y = "Peso",
                color = "red",
                title = "Rendimiento v/s peso",
                xlab = "Rendimiento [millas/galón]",
                ylab = "Peso [1000 lb]")

print (g)





# Gráfico para variables independientes .
g1 <- ggscatter (datos ,
                 x = "Peso",
                 y = "Cuarto_milla",
                 color = "blue",
                 title = "Independientes",
                 xlab = "Peso [1000 lb]",
                 ylab = "Tiempo para recorrer un cuarto de milla [s]")


# Gráfico para variables con asociaci ón positiva .
g2 <- ggscatter (datos ,
                 x = "Peso",
                 y = "Potencia",
                 color = "orange",
                 title = "Asociación positiva",
                 xlab = "Peso [1000 lb]",
                 ylab = "Potencia [hp]")


# Gráfico para variables con asociaci ón negativa .
g3 <- ggscatter (datos ,
                 x = "Peso",
                 y = "Rendimiento",
                 color = "black",
                 title = "Asociación negativa",
                 xlab = "Peso [1000 lb]",
                 ylab = "Rendimiento [millas/galón]")


# Crear figura con tres gráficos .
g <- ggarrange (g1 ,g2 ,g3 , ncol = 3, nrow = 1, common.legend = TRUE )

print (g)






# Crear tabla de contingencia para las variables Motor y Cambios ,
# y guardarla como data frame .
tabla <- xtabs (~ Motor + Cambios, data = datos ) 
contingencia <- as.data.frame ( tabla )



# Crear tabla de proporciones por columnas y guardarla como
# data frame .
proporciones <- as.data.frame ( prop.table ( tabla, margin = 2))





