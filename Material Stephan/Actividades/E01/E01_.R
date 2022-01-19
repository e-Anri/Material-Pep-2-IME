# E01
# Grupo: 5 (e y f)
# Fecha: 19 - 04 - 2021

library(ggplot2)
library(ggpubr)

# Indicar directorio
#dir <- "~/../Downloads"

# Una selección de columnas (hecha por el profesor) de los resultados
# públicos de la encuesta Casen 2017 en la Región Metropolitana.
# Las siguientes son las columnas que se eligieron:
#  - id.reg: nº secuencial del registro
#  - folio: identificador del hogar (comp: comuna área seg viv hogar)
#  - o: número de orden de la persona dentro del hogar
#  - id.vivienda: identificador de la vivienda (comp: comuna área seg viv)
#  - hogar: identificación del hogar en la vivienda
#  - region: región
#  - provincia: provincia
#  - comuna: comuna
#  - ing.comuna: posición en el ranking histórico del ingreso de la
#                comuna (de menor a mayor ingreso)
#  - zona: área geográfica (Urbano, Rural)
#  - sexo: sexo de la persona registrada
#  - edad: edad de la persona registrada
#  - ecivil: estado civil de la persona registrada
#  - ch1: situación ocupacional de la persona registrada
#  - ytot: ingreso total

#basename <- "Casen 2017.csv"
#file <- file.path(dir, basename)
file <- file.choose()

# Esta función lee un CSV en inglés: campos separados por comas y
# números con "punto decimal".
# Existen otras funciones para leer otros formatos de texto. Ver help.
# Si se quiere leer una planilla MS Excel, se puede usar las funciones
# del paquete "xlsx".
población <- read.csv(file = file, encoding = "UTF-8")

# Se devuelve un "marco de datos" o data.frame
print(class(población))
print(str(población))

# Podemos obtener el número de filas (o de columnas) que corresponde al número de
# "casos" en la muestra/población (o de variables).
tamaño.población <- nrow(población)

# Primero se elige una muestra de forma aleatoria, pero fijando una
# semilla para el generador de secuencias pseudoaleatorias para poder
# repetir los resultados

semilla <- 101
tamaño.muestra <- 100

set.seed(semilla)
muestra <- población[sample(1:tamaño.población, tamaño.muestra), ]

datos <- muestra
datos$ingreso <- datos$ytot/1000

#Preguntas asignadas:

# Ejercicio e:
#¿Son similares los ingresos registrados en las diferentes provincias de la RM?
  
# Se decidió usar gráfico de cajas debido a que se necesitaba una variable 
# categórica en el eje x y una variable numérica en el eje y. Además, en cada 
# provincia podían existir distintos ingresos y este gráfico tiene distintas 
# partes interesantes para analizar, como la caja, los bigotes, el punto rojo al 
# interior de la caja(mediana) y las observaciones atípicas que corresponden a 
# los puntos alejados de la caja.
p <- ggboxplot(
  datos,
  x = "provincia", y = "ingreso",
  add = "mean", add.params = list(color = "#FC4E07"),
  color = "provincia", fill = "provincia",
  title = "Muestra del ingreso de cada provincia de la RM",
  ylab = "Ingreso total (miles de pesos)",
  xlab = "Provincias de la RM"
)
print(p)

# Con solo observar el gráfico se puede notar que no son similares los ingresos 
# registrados en las diferentes provincias de la RM de la muestra. También, se 
# puede apreciar esta diferencia en relación a la media y la desviación estandar, 
# cuyo cálculo se muestra a continuación.
print(by(datos[["ytot"]], datos[["provincia"]], mean))
print(by(datos[["ytot"]], datos[["provincia"]], sd))

# Ejercicio f
#¿Van los ingresos de los chilenos incrementándose con la experiencia y de forma 
#similar entre hombres y mujeres?

g <- ggplot(data=datos, aes(edad,ingreso,color=sexo)) +
    geom_point() + # los puntos
    stat_smooth(se=FALSE) +
    scale_x_continuous("Edad") + 
    scale_y_continuous("Ingresos totales") + 
    scale_color_discrete("Género")
print(g)
# Utilizamos un gráfico de puntos debido a que estos son útiles al momento de 
# analizar los datos de variables cuantitativas.
# Fuente utilizada para generar el gráfico: https://rua.ua.es/dspace/bitstream/10045/69767/1/Modulo_4_-_Graficos_avanzados_con_ggplot2.pdf
# Respondiendo a la pregunta, los ingresos no tienen relación directa con la 
# experiencia, y según el gráfico generado, es posible ver de mejor manera, que 
# los ingresos de los hombres son mayores que los de las mujeres a partir de los 
# 25 años aproximadamente.
  