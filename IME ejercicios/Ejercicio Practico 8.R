library(ggpubr)
library(ggplot2)
library(ez)
library(knitr)
library(tidyr)
"
Ejercicio practico 8
"
# RESOLUCION EJERCICIO A

'

El siguiente código R define los datos que aparecen en una tabla que compara las
mejores soluciones encontradas por cuatro algoritmos para instancias del problema 
del vendedor viajero con solución óptima conocida, tomados desde una memoria del
DIINF. Con estos datos responda la pregunta de investigación: ¿Hay algoritmos mejores que otros?

'
cat("Ejercicio A \n")

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
#OBTENER LOS DATOS EN FORMATO ANCHO
datos <- read.table(
  file= textConnection(texto), 
  header = TRUE,
  stringsAsFactors = FALSE
  )

names(datos)<- c("Instancia", "Optimo", "R", "R2", "R3", "G")
#OBTENER LOS DATOS EN FORMATO LARGO
dl <- gather(
  data = datos,
  key = "Algoritmo",
  value = "Tiempo",
  -"Instancia"
)

dl[["Instancia"]] <- factor(dl[["Instancia"]])
dl[["Algoritmo"]] <- factor(dl[["Algoritmo"]])

# Como aproximación se comparan los algoritmos con una gráfico de
# cajas.
p1 <- ggboxplot(
  dl,
  x = "Algoritmo", y = "Tiempo",
  xlab = "Algoritmos en instancias", ylab = "Tiempo",
  color = "Algoritmo",
  add = "jitter",
  add.params = list(color = "Algoritmo", fill = "Algoritmo")
)
print(p1)


# ----------------------------------------------------
# Verificaciones
# ----------------------------------------------------

# Estas se explican en el capítulo 14 de VarssarStats:
# 1. La variable dependiente tiene escala de intervalo
# 2. Las muestras son independientes y obtenidas aleatoriamente
# 3. Se puede asumir que las poblaciones son aproximadamente normales
# 4. Las muestras tienen varianzas similares

# Para la condición 2 obtenida según el enunciado



# Para la condición 3, podemos usar un gráfico QQ
p2 <- ggqqplot(
  dl,
  x = "Tiempo",
  color = "Algoritmo"
)
p2 <- p2 + facet_wrap(~ Algoritmo)
print(p2)

# Podemos ver que, si vien hay un par de puntos más o menos 
# problemáticos por aquí por allá, no hay desviaciones importantes en
# los datos. 

# Para confirmar, podríamos aplicar alguna prueba de normalidad.
# Con muestras tan pequeñas, podría convenir usar la prueba de 
# Shapiro-Wilk.





