library(ggpubr)

texto <- "4.62 4.43 5.18 4.89 4.89 5.41 4.87 5.07 5.30 4.98 4.54 5.21 4.60
          4.71 4.58 4.99 5.05 4.70 4.63 4.95 4.85 4.19 5.25 4.69 5.03 4.74
          4.67 4.85 4.45 4.93 4.42 4.40 5.59 4.69 5.42 5.19 4.99 4.88 4.03
          5.51 4.90 4.43 4.93 4.84 4.73 4.89 4.53 4.97 5.10 5.95 4.95 4.18
          4.91 4.87 5.38 5.49 4.96 4.76 4.76 4.63 5.10 4.84 4.87 4.39 4.99
          5.03 4.31 5.05 4.71 4.78 4.90 5.02 4.84 5.18 4.79 4.99 4.55 4.70
          4.74 4.60 4.94 5.25 5.01 4.95 4.19 5.27 5.00 5.15 5.12 4.34 4.27
          4.92 4.98 4.91 5.05 5.28 4.29 5.58 5.55 4.60"
file <- textConnection(texto)
datos <- scan(file)

dataframe <- data.frame(datos)

ggqqplot(data = dataframe, 
         x = "datos", 
         color ="red", 
         xlab = "teorico",
         ylab = "muestra",
         title = "grafico Q-Q distribucion normal")

mu_0 = 1
alfa <- 0.005

t.test(datos, alternative="less", mu = mu_0, conf.level = 1-alfa)
