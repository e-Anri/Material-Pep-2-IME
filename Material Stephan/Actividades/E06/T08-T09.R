

# Actividad: recordando a Fisher y conociendo a McNemar

# Problema 1:
# Una de las primeras preguntas a responder por el ?ltimo estudio nacional de 
# obesidad infantil fue si exist?an diferencias en la prevalencia de la obesidad
# entre ni?os y ni?as o si, por el contrario, el porcentaje de obesos no var?a 
# entre sexos. Se le colicita responder esta pregunta, contando con las primeras 
# observaciones obtenidas en el estudio sobre una muestra de 14 menores.

# Se propone una hip?tesis nula y una hip?tesis alternativa 
# H0: Las variables Obesidad y Sexo son independientes
# H1: Las variables Obesidad y Sexo no son independientes

# Construir la tabla de contingencia
sexo <- c( rep("Ni?a", 4),
           rep("Ni?o", 2),
           "Ni?a",
           rep("Ni?o", 7))

resultado <- c ( rep("Sano", 6), rep("Obeso", 8))
datos <- data.frame ( resultado , sexo )
tabla <- xtabs (~. , datos )

# Aplicar prueba exacta de Fisher
alfa <- 0.05
prueba <- fisher.test ( tabla , 1 - alfa )
print ( prueba )

#Respuesta P1
# Considerando un nivel de confianza igual a 0.05, menor al valor p = 0.09091,
# se falla en rechazar la hipotesis nula. En consecuencia, se concluye que, 
# con un 95% de confianza que no hay una asociacion entre la cantidad de 
# ni?@s obesos y su sexo.


# Problema 2:
# En el art?culo de Garc?a y colegas (2010) se describe un estudio en que 
# compararon diferentes versiones de algoritmos evolutivos para resolver 
# variadas instancias de problemas de calificacion tomadas desde el UCI 
# Machine Learning Repository. Suponga que las siguientes son los reultados 
# de la calificacion hecha por dos versiones de un algoritmo genetico evaluado 
# en el estudio para el problema Breast Cancer. ?consigue uno de los algoritmos 
# mejor desempe?o?

# Se formulan las hipotesis
# H0: La diferencia de desempe?o de los algoritmos es igual a 0
# H1: La diferencia de desempe?o de los algoritmos es distinta a 0

# Se propone una hip?tesis nula y una hip?tesis alternativa 
# H0: desempe?o AGv1 - desempe?o AGv2 = 0
# H1: desempe?o AGv1 - desempe?o AGv2 != 0

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

# Respuesta P2
# Obtenindose que p = 0.2888 mayor al nivel de significacion igual a 0. 05,
# se falla al rechazar la hipotesis nula y se concluye que no hay diferencia
# en el desempe?o de ambos algoritmos.

#____________________________________________________________________________

# Actividad: inferencias con frecencias

#Pregunta B
# Una investigacion monitoreo a mas de 50 mil mujeres adultas durante 10 a?os
# (Archives of Internal Medicine, 171(17), 1571-1578) con la intencion de 
# identificar factores de riesgo de desarrollar un cuadro de depresion. Entre 
# otras cosas, se registro el consumo de cafeina, cuyos datos se resumen en la
# siguiente tabla. ?Existe alguna asociacion entre la cantidad de cafe que se 
# bebe y la depresion?

# Se propone una hip?tesis nula y una hip?tesis alternativa 
# H0: Las variables Depresion y Consumo de cafe no descafeinado son independientes.
# H1: Las variables Depresion y Consumo de cafe no descafeinado son dependientes.

# Crear tabla de contingencia.
tabla <- cbind ( c ( 670, 11545) ,
                 c ( 373, 6244) , 
                 c ( 905, 16329) , 
                 c ( 564, 11726) , 
                 c ( 95, 2288) )

colnames(tabla) <- c ("<=1" , 
                      "2-6" , 
                      "1" , 
                      "2-3" , 
                      ">=4" )

rownames(tabla) <- c("Si","No")

tabla <- as.table( tabla )
print ( tabla )

# # Prueba chi - cuadrado de independencia .
prueba <- chisq.test ( tabla , correct = FALSE )
print ( prueba )

#Respuesta PB
# El valor p obtenido es muy peque?o p = 0.0003267, menor que el nivel de 
# significacion igual a 0,01, por lo que se rechaza la hipotesis nula a favor
# de la hipotesis alternativa, por lo que se concluye con 99% de cofianza
# que las variables Depresion y Consumo de cafe no descafeinado est?n 
# relacionadas.

# ?Por qu? la familia de pruebas X2 son no parametricas?
# Esta prueba es NO PARAMETRICA porque no menciona alg?n par?metro. Es mas, 
# no se hace ninguna suposicion sobre la distribucion de la poblacion desde
# donde proviene la muestra analizada.
