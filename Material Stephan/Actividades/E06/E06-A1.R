
# Actividad: recordando a Fisher y conociendo a McNemar

# Problema 1:
# Una de las primeras preguntas a responder por el último estudio nacional de 
# obesidad infantil fue si existían diferencias en la prevalencia de la obesidad
# entre niños y niñas o si, por el contrario, el porcentaje de obesos no varía 
# entre sexos. Se le colicita responder esta pregunta, contando con las primeras 
# observaciones obtenidas en el estudio sobre una muestra de 14 menores.

# Se propone una hipótesis nula y una hipótesis alternativa 
# H0: Las variables Obesidad y Sexo son independientes
# H1: Las variables Obesidad y Sexo no son independientes

# Construir la tabla de contingencia
sexo <- c( rep("Niña", 4),
           rep("Niño", 2),
           "Niña",
           rep("Niño", 7))
resultado <- c ( rep("Sano", 6), rep("Obeso", 8))
datos <- data.frame ( resultado , sexo )
tabla <- xtabs (~. , datos )

# Aplicar prueba exacta de Fisher
alfa <- 0.05
prueba <- fisher.test ( tabla , 1 - alfa )
print ( prueba )

#Respuesta 1
# Considerando un nivel de confianza igual a 0.05, mayor al valor p = 0.002997,
# se rechaza la hipotesis nula a favor de la hipotesis alternativa. 
# En consecuencia, se concluye que, con un 95% de confianza que existe una 
# asociacion entre la cantidad de niñ@s obesos y su sexo.



# Problema 2:
# En el artículo de García y colegas (2010) se describe un estudio en que 
# compararon diferentes versiones de algoritmos evolutivos para resolver 
# variadas instancias de problemas de calificacion tomadas desde el UCI 
# Machine Learning Repository. Suponga que las siguientes son los reultados 
# de la calificacion hecha por dos versiones de un algoritmo genetico evaluado 
# en el estudio para el problema Breast Cancer. ¿consigue uno de los algoritmos 
# mejor desempeño?

# Se formulan las hipotesis
# H0: La diferencia de desempeño de los algoritmos es igual a 0
# H1: La diferencia de desempeño de los algoritmos es distinta a 0

# Se propone una hipótesis nula y una hipótesis alternativa 
# H0: desempeño AGv1 - desempeño AGv2 = 0
# H1: desempeño AGv1 - desempeño AGv2 != 0

# Construir la tabla de contingencia
alumno <- seq ( 1 : 12 )
agv_1 <- c ( "Incorrecta",
             "Correcta",
             "Incorrecta",
             "Correcta",
             rep ( "Incorrecta", 2 ),
             rep ( "Correcta", 3 ),
             rep ( "Incorrecta", 3 ))

agv_2 <- c ( rep ( "Correcta", 4 ) , 
             "Incorrecta",
             rep ( "Correcta", 2 ) , 
             rep ( "Incorrecta", 2 ),
             rep ( "Correcta", 3 ) )

datos <- data.frame ( alumno , agv_2 , agv_1 )
tabla <- table ( agv_2 , agv_1 )
addmargins ( tabla )

# Aplicar prueba de McNemar
mcnemar.test ( tabla )

# Respuesta 2
# Obtenindose que p = 0.2888 mayor al nivel de significacion igual a 0. 05,
# se falla al rechazar la hipotesis nula y se concluye que no hay diferencia
# en el desempeño de ambos algoritmos.
