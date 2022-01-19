library(ggpubr)
basename <- "Casen 2017.csv"
#file <- file.path(dir, basename)
file <-choose.files()
población <- read.csv(file = file)
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.3
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

#Distribuciones Continuas

#1.
set.seed(101)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#2.Distribución Z
z <- (ingreso.normal - media.ingreso) / sd.ingreso
print(z)


#3.Distribuciones ??2

# La primera con 10 grados de libertad:
grado1 <- 10
x2.1 <- z^((grado1/2)-1)*exp(1)^(-z/2)/2^(z/2)*gamma(grado1/2)
print (x2.1)

# La segunda con 9 grados de libertad:
grado2 <- 9
x2.2 <- z^((grado2/2)-1)*exp(1)^(-z/2)/2^(z/2)*gamma(grado2/2)
print (x2.2)

#4.Distribución F
f <- (x2.1/grado1)/(x2.2/grado2)
print (f)

#5.Gráfico Distribuciones
#Distribucion Z:
hist(z, plot=TRUE, 
     main = "Distribucion Z", 
     col = "purple",
     border = "blue", 
     xlab = "Distribucion Z", 
     ylab = "Frecuencia")

#Distribuciones ??2:
hist(x2.1, plot=TRUE, 
     main = "Distribucion ??2", 
     col = "yellow",
     border = "orange", 
     xlab = "Distribucion ??2.1", 
     ylab = "Frecuencia")

hist(x2.2, plot=TRUE, 
     main = "Distribucion ??2", 
     col = "yellow",
     border = "orange", 
     xlab = "Distribucion ??2.2", 
     ylab = "Frecuencia")

#Distribucion F:
hist(f, plot=TRUE, 
     main = "Distribucion F", 
     col = "pink",
     border = "red", 
     xlab = "Distribucion F", 
     ylab = "Frecuencia")


#Distribuciones Discretas

#6.
set.seed(113)
n.repeticiones <- 12

ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)

doce.repeticiones <- sapply(1:n.repeticiones, ensayo)

combinatoria <- combinations(n.repeticiones, doce.repeticiones, ensayo)

