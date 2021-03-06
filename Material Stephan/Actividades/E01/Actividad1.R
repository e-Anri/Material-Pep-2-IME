#Room 5
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

semilla <- 101
tama�o.muestra <- 100

set.seed(semilla)
muestra <- poblaci�n[sample(1:tama�o.poblaci�n, tama�o.muestra), ]

datos <- muestra
datos$ingreso <- datos$ytot/1000


#Preguntas asignadas:
#e.�Son similares los ingresos registrados en las diferentes provincias de la RM?

#Se decidi� usar gr�fico de cajas debido a que se necesitaba una variable 
#categ�rica en el eje x y una variable num�rica en el eje y. Adem�s, en cada 
#provincia pod�an existir distintos ingresos y este gr�fico tiene distintas 
#partes interesantes para analizar, como la caja, los bigotes, el punto rojo al 
#interior de la caja(mediana) y las observaciones at�picas que corresponden a los 
#puntos alejados de la caja.
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


#Con solo observar el gr�fico se puede notar que no son similares los ingresos 
#registrados en las diferentes provincias de la RM de la muestra. Tambien, se 
#puede apreciar esta diferencia en relaci�n a la media y la desviaci�n estandar, 
#cuyo c�lculo se muestra a continuaci�n.
print(by(datos[["ytot"]], datos[["provincia"]], mean))
print(by(datos[["ytot"]], datos[["provincia"]], sd))


#f.�Van los ingresos de los chilenos increment�ndose con la experiencia y de forma 
#similar entre hombres y mujeres?
