
# Enunciado
# Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) 
# (Journal of chronic diseases, 25(12), 711-716) sobre la incidencia de la 
# cantidad de alcohol y de tabaco que se consume en el riesgo de padecer 
# cáncer oral.

# Preguntas B

# 1. Suponiendo que la diferencia en la proporción de personas que desarrollan 
# la enfermedad entre quienes beben de 1 a 9 ml de alcohol por día y aquellos 
# que beben de 10 a 44 ml al día es de 0.10. ¿Cuánta gente deberíamos entrevistar 
# para obtener un intervalo de confianza del 95% y poder estadístico de 80%?

# Tamaños de las muestras.
n1 <- 230
n2 <- 200

# Cantidad de éxitos en las muestras.
exitos1 <- 89
exitos2 <- 109

# Valor nulo
nulo <- 0.10

# Probabilidades de éxito estimadas.
p_est1 <- exitos1 / n1
p_est2 <- exitos2 / n2

# Diferencia de las proporciones.
dif <- p_est1 - p_est2

# Error estándar.
err <- sqrt ( p_est1 * (1 - p_est1) / n1 + p_est2 * (1 - p_est2) / n2)

# Estadístico de prueba.
Z <- ( dif - nulo ) / err

# Valor p.
p <- pnorm (Z , lower.tail = FALSE )
print ( p )


# Se busca obtener el n con intervalo de confianza del 95% y poder estadístico 
# de 80%
power.prop.test(n = NULL, 
                p1 = p_est1, 
                p2 = p_est2, 
                sig.level = 0.05,
                power = 0.8,
                alternative = "two.sided")

# Respuesta B-1
# Deberiamos entrevistar 155 personas para obtener un intervalo de confianza
# de 95% y un poder estadistico de un 80%, Suponiendo que la diferencia en la 
# proporcio??n de personas que desarrollan la enfermedad entre quienes beben de
# 1 a 9 ml de alcohol por di??a y aquellos que beben de 10 a 44 ml al di??a es de 
# 0.10.


# 2. Estudios previos habían determinado que la incidencia de cáncer oral en 
# la población que bebe regularmente entre 1 y 9 ml de alcohol era de 25%. 
# ¿Respaldan estos datos tal estimación?

#H0: La población que bebe regularmente entre 1 y 9 ml de alcohol es de 25%
#H1: La población que bebe regularmente entre 1 y 9 ml de alcohol supera el 25%

# Tamaño, proporción de éxito y cantidad de éxitos de la muestra.
n <- 230
p_muestra <- 89/230
exitos <- p_muestra * n

# Valor nulo y nivel de significación.
p0 <- 0.25
alfa <- 0.05

# Prueba de hipótesis.
prueba <- prop.test ( exitos,
                      n = n,
                      p = p0,
                      alternative = "greate",
                      conf.level = 1 - alfa)

print(prueba)

# Respuesta B-2
# Debido a que el valor p = 0.3869565 es mayor que 0.25, no hay evidencia 
# suficiente para rechazar la hipotesis nula, por lo que se respalda la
# estimación que la incidencia de cáncer oral en la población que bebe 
# regularmente entre 1 y 9 ml de alcohol era de 25%.


# 3. Según estos datos, ¿da lo mismo beber entre 1 y 9 ml de alcohol diariamente 
# que hacerlo de 10 a 44 ml?

#H0: La proporcion entre los que beben de 1-9 ml menos proporcion de los que
#    beben 10-44 ml es igual a 0.
#H1: La proporcion entre los que beben de 1-9 ml menos proporcion de los que
#    beben 10-44 ml es distinto a 0.

# Cantidad de éxitos
exitos <- c ( 89 , 109 )

# Tamaño de la muestra
n <- c( 230 , 200 )

# Nivel de significación.
alfa <- 0.05

# Prueba de la hipótesis.
prueba <- prop.test ( exitos,
                      n = n,
                      alternative = "two.sided",
                      conf.level = 1 - alfa )

print ( prueba )

# Respuesta B-3
# Como se obtuvo un valor p de 0.00146 que es menor a 0.05 (nivel significacion),
# se rechaza la hipotesis H_0 a favor de H_A por lo tanto: No da lo mismo 
# beber 1-9 ml diarios que beber 10-44 ml diarios de alcohol.

