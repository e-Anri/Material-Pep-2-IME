poblacion <- read.csv("C:\\Users\\Estefania\\Desktop\\inferencia\\datos.csv", encoding = "UTF-8")
tamaño <- nrow(poblacion)
ingreso <- as.numeric(poblacion[["ytot"]])
poda <- 0.2

q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)

ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )

set.seed(69420)

ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

ingreso.z <- (ingreso.normal - media.ingreso) / sd.ingreso


plot(ingreso.z, type="o", col="green")


chi <- (((ingreso.z - 14)^2) / 14)
chis <- rchisq(5000, 14)


chi2 <- (((ingreso.z - 4)^2) / 4)
chis2 <- rchisq(5000, 4)


# gráfico distribución F
x <- rf(5000, df1 = 4, df2 = 14)

hist(x, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,5), 
     ylim = c(0,1),
     xlab = '',
     ylab = '',
     main = "Distribución F",
     cex.main=0.9)

curve(df(x, df1 = 4, df2 = 14), col= 'blue', lwd=2, add = T)


hist(chi)
hist(chi2)






