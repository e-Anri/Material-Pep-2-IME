# Se importan librerías
library(ggpubr)
library(dplyr)

# Realizando llamado al archivo .csv
setwd("C:\\Users\\littl\\Desktop\\IME\\testtt")
datos <- read.csv("EP-02 Datos Casen 2017.csv", encoding = "UTF-8")


# Generando tablas segun hombres y mujeres
hombres_TF <- filter(datos, sexo == "Hombre")
mujeres_TF <- filter(datos, sexo == "Mujer")


# Calculando promedio, varianza y desviacion estandar para 
# ambas tablas 
medidas_hombres <- onvrez_TF %>% summarise(Media = mean(ytot),
                                          Dstandar = sd(ytot),
                                          Varianza = var(ytot)) 

medidas_mujeres <- envraz_TF %>% summarise(Media = mean(ytot),
                                          Dstandar = sd(ytot),
                                          Varianza = var(ytot))



# Generando gráficos de barras para los ingresos totales
# tanto para hombres como mujeres
graf_hombres <- gghistogram(hombres_TF,
                           x = "ytot",
                           bins = 50,
                           add = "mean",
                           xlab = "Ingreso",
                           ylab = "Onvrez",
                           color = "red",
                           fill = "red")

graf_mujeres <- gghistogram(mujeres_TF,
                           x = "ytot",
                           bins = 50,
                           add = "mean",
                           xlab = "Ingreso",
                           ylab = "Envraz",
                           color = "blue",
                           fill = "blue")


# Se genera un grafico final que contiene los otros graficos superpuestos
graf_total <- ggarrange(graf_mujeres, graf_hombres, ncol = 2, nrow=1, common.lengend = TRUE)
print(graf_total)



# 3) Para responder a la pregunta "¿Tienen hombres y mujeres ingresos similares en la RM?"
# decidimos como grupo que el promedio, la varianza y la desviacion estandar son las medidas
# estadisticas que ayudan a responder la pregunta, esto debido a la cantidad de informacion
# que se puede extraer para compararla segun sea el sexo, es decir, el promedio de personas
# que tienen mayor salario, que tanto varian los datos, etc


# Por el lado del grafico, creamos dos histogramas ya que es la manera mas sencilla para 
# graficar el ingreso total segun el sexo de las personas. 

# Siendo lo que se ve en cada grafico la frecuencia de personas (hombres o mujeres) y el ingreso
# total que tienen
  

# Finalmente, a pesar de que los gráficos tengan una distribución similar, el ingreso total que reciben los hombres
# en promedio son cercano a 300.000 pesos más, por lo tanto, se puede decir que los ingresos no son similares.

