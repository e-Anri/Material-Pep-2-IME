#cargando el conjunto de datos nativo R.ChickWeight
chickens <- data.frame(ChickWeight)

#modificando nombres de las columnas de ingles a español
chickens <- chickens %>% rename(Peso = weight, Tiempo = Time, 
                              Pollo = Chick, Alimentación = Diet)


#dando formato categórico a la variable alimentación
chickens[["Alimentación"]] <- factor(chickens[["Alimentación"]], 
                                     levels = c(1, 2),
                                     labels = c("Natural", "Procesada"))

#linea para guardar el archivo en 
write.csv(chickens, "capitulo1.pregunta6.csv")