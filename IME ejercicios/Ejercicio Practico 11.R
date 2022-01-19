library(ggpubr)
library(ggplot2)


"Ejercicio Practico 11
"

"1. Usando como semilla el año y mes de nacimiento de un/a miembro del equipo
(ej.: 198012), seleccionen una muestra de 50 mujeres (si la semilla es un 
número par) o 50 hombres (si la semilla es impar) de los datos body.csv.
"
# Indicar directorio
dir <- ""

#Se obtienen los datos
basename <- "body.csv"
file <- file.path(dir, basename)

datos.todos <- read.csv2(
  file = "/Users/DanielaOrellanaCruz/Desktop/body.csv"
)

#Se utiliza una semilla cualquiera

set.seed(12000)

# Tomando una muestra de 50 mujeres dentro de la poblacion
mujeres <- which(datos.todos[["Gender"]] == 0)
set.seed(13)
N <- 50
mujeres <- sample(mujeres, N)
datos <- datos.todos[c(mujeres),]

"
2. selecciona un conjunto reducido de variables para construir un modelo RLM, 
evaluando que este sea confiable.
"
# Obtenemos estas 2 variables para analizarlas en más detalle
estatura <- datos$Height
peso <- datos$Weight
edad <- datos$Age
N <- nrow(datos)
"
3. evalúa el poder predictivo del modelo en datos no utilizados para construirlo
o utilizando validación cruzada.
"

#Se evaluan los predictores y se construye un gráfico 3d
modelo <- lm(edad ~ estatura + peso, data=datos)

print(summary(modelo))

grafico <- scatterplot3d(estatura,peso,edad,type="p",
                   highlight.3d=TRUE,
                   
                   pch=20,
                   xlab="Estatura en cm",
                   ylab="Peso en kg",
                   zlab="Edad en anos",
)

# Por último, se obtiene un plano a partir del gráfico que representa el 
# cambio esperado de una respuesta (Edad) cuando varían los predictores
g$plane3d(modelo,draw_polygon=TRUE,draw_lines=TRUE)

print(grafico)




