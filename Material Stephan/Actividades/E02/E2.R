

library(ggpubr)
basename <- "Casen 2017.csv"
dir <- "~/../Downloads"
file <- file.path(dir, basename)
población <- read.csv(file = file)
#tamaño total de la población
tamaño <- nrow(población)
#filtrar ingreso de encuestados
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.3
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

#Distribuciones Continuas

#1.Se crean 9 distrubuciones normales para luego crear 9 respectivas distribuciones z.
set.seed(101)
ingreso.normal1 <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
ingreso.normal2 <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
ingreso.normal3 <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
ingreso.normal4 <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
ingreso.normal5 <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
ingreso.normal6 <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
ingreso.normal7 <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
ingreso.normal8 <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
ingreso.normal9 <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#2.Se crean 9 distrubuciones z para luego crear las distibuciones xi2 con grados de libertad.
z1 <- (ingreso.normal1 - media.ingreso) / sd.ingreso
z2 <- (ingreso.normal2 - media.ingreso) / sd.ingreso
z3 <- (ingreso.normal3 - media.ingreso) / sd.ingreso
z4 <- (ingreso.normal4 - media.ingreso) / sd.ingreso
z5 <- (ingreso.normal5 - media.ingreso) / sd.ingreso
z6 <- (ingreso.normal6 - media.ingreso) / sd.ingreso
z7 <- (ingreso.normal7 - media.ingreso) / sd.ingreso
z8 <- (ingreso.normal8 - media.ingreso) / sd.ingreso
z9 <- (ingreso.normal9 - media.ingreso) / sd.ingreso


#3.Distribuciones Xi2

# La primera con 5 grados de libertad:
grado1 <- 5
x2.1 <- z1^2 + z2^2 + z3^2 + z4^2 + z5^2

# La segunda con 9 grados de libertad:
grado2 <- 9
x2.2 <- z1^2 + z2^2 + z3^2 + z4^2 + z5^2 + z6^2 + z7^2 + z8^2 + z9^2

#4.Distribución F con las distribuciones Xi2 anteriores
f <- (x2.1/grado1)/(x2.2/grado2)

#5.Gráfico Distribuciones
#Distribucion Z:
z <- c(z1, z2, z3, z4, z5, z6, z7, z8, z9)
hist(z, plot=TRUE, 
     main = "Distribucion Z", 
     col = "purple",
     border = "blue", 
     xlab = "Distribucion Z", 
     ylab = "Frecuencia")

#Distribuciones Xi2:
#con 5 grados de libertad
hist(x2.1, plot=TRUE, 
     main = "Distribucion Xi2", 
     col = "yellow",
     border = "orange", 
     xlab = "Distribucion Xi2.1", 
     ylab = "Frecuencia")
#con 9 grados de libertad
hist(x2.2, plot=TRUE, 
     main = "Distribucion Xi2", 
     col = "yellow",
     border = "orange", 
     xlab = "Distribucion Xi2.2", 
     ylab = "Frecuencia")

#Distribucion F:
hist(f, plot=TRUE, 
     main = "Distribucion F", 
     col = "pink",
     border = "red", 
     xlab = "Distribucion F", 
     ylab = "Frecuencia")


#Distribuciones Discretas

#6. Se define la semilla y la cantidad de repeticiones.
set.seed(101)
n.repeticiones <- 100
ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)

cien.repeticiones <- sapply(1:n.repeticiones, ensayo)

#7. Se genera la distribucion binomial a partir de la distribucion de bernoulli.
binomial <- function(x){
  return( sum(  sapply(1:100, ensayo)  )  )
}
dis.binomial <- sapply(1:1000,binomial)

#8. Se crea la distribucion geometrica a partir de la distribucion de bernoulli.
geometrica <- function(x){
  rep <- sapply(1:100, ensayo)
  return( which(rep == 1)[1]  )
}
dis.geometrica <- sapply(1:1000, geometrica)

#9. Se crea la distribucion binomial negativa a partir de la distribucion de bernoulli.
binomialnegativa <- function(x){
  rep <- sapply(1:100, ensayo)
  return( which(rep == 1)[5]  )
}
dis.binomialnegativa <- sapply(1:1000, binomialnegativa)

#10.Graficos

#Distribucion binomial
hist(dis.binomial, plot=TRUE, 
     main = "Distribucion binomial", 
     col = "purple",
     border = "blue", 
     xlab = "Distribucion binomial", 
     ylab = "Frecuencia")

#Distribucion geometrica
hist(dis.geometrica, plot=TRUE, 
     main = "Distribucion geometrica", 
     col = "yellow",
     border = "orange", 
     xlab = "Distribucion geometrica", 
     ylab = "Frecuencia")

#Distribucion binomial negativa
hist(dis.binomialnegativa, plot=TRUE, 
     main = "Distribucion binomial negativa", 
     col = "pink",
     border = "red", 
     xlab = "Distribucion binomial negativa", 
     ylab = "Frecuencia")


