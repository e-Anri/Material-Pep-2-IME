library(ggplot2)
library(ggpubr)

# Indicar directorio
#dir <- "~/../Downloads"
dir <- "C:/Users/Downloads/"

# Una selecci�n de columnas (hecha por el profesor) de los resultados
# p�blicos de la encuesta Casen 2017 en la Regi�n Metropolitana.
# Las siguientes son las columnas que se eligieron:
#  - id.reg: n� secuencial del registro
#  - folio: identificador del hogar (comp: comuna �rea seg viv hogar)
#  - o: n�mero de orden de la persona dentro del hogar
#  - id.vivienda: identificador de la vivienda (comp: comuna �rea seg viv)
#  - hogar: identificaci�n del hogar en la vivienda
#  - region: regi�n
#  - provincia: provincia
#  - comuna: comuna
#  - ing.comuna: posici�n en el ranking hist�rico del ingreso de la
#                comuna (de menor a mayor ingreso)
#  - zona: �rea geogr�fica (Urbano, Rural)
#  - sexo: sexo de la persona registrada
#  - edad: edad de la persona registrada
#  - ecivil: estado civil de la persona registrada
#  - ch1: situaci�n ocupacional de la persona registrada
#  - ytot: ingreso total

basename <- "Casen 2017.csv"
file <- file.path(dir, basename)

# Esta funci�n lee un CSV en ingl�s: campos separados por comas y
# n�meros con "punto decimal".
# Existen otras funciones para leer otros formatos de texto. Ver help.
# Si se quiere leer una planilla MS Excel, se puede usar las funciones
# del paquete "xlsx".
poblaci�n <- read.csv(file = file, encoding = "UTF-8")

# Se devuelve un "marco de datos" o data.frame
print(class(poblaci�n))
print(str(poblaci�n))

# Podemos obtener el n�mero de filas (o de columnas) que corresponde al n�mero de
# "casos" en la muestra/poblaci�n (o de variables).
tama�o.poblaci�n <- nrow(poblaci�n)

# Primero se elige una muestra de forma aleatoria, pero fijando una
# semilla para el generador de secuencias pseudoaleatorias para poder
# repetir los resultados

semilla <- 113
tama�o.muestra <- 100

set.seed(semilla)
muestra <- poblaci�n[sample(1:tama�o.poblaci�n, tama�o.muestra), ]

q <- quantile(muestra$ytot, seq(.2, 1, .2))

quintil <- ifelse(muestra$ytot <= q[1], 1, 0)
quintil[muestra$ytot > q[1] & muestra$ytot <= q[2]] <- 2
quintil[muestra$ytot > q[2] & muestra$ytot <= q[3]] <- 3
quintil[muestra$ytot > q[3] & muestra$ytot <= q[4]] <- 4
quintil[muestra$ytot > q[4]] <- 5

datos <- muestra
datos$quintil <- quintil
datos$ingreso <- datos$ytot / 1000
p <- ggboxplot(
  datos,
  x = "quintil", y = "ingreso",
  add = "mean", add.params = list(color = "#FC4E07"),
  color = "quintil", fill = "quintil",
  title = "Muestra del ingreso en la Regi�n Metropolitana",
  ylab = "Ingreso total (miles de pesos)"
)
print(p)

cat("\n")
cat("L�mites de los quintiles\n")
cat("------------------------\n")
print(c("0%" = 0, q))

cat("\n")
cat("Ingreso medio de cada quintil\n")
cat("-----------------------------\n")
print(by(datos[["ytot"]], datos[["quintil"]], mean))
