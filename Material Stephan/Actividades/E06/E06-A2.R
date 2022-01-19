
# Actividad: inferencias con frecencias

#Pregunta B
# Una investigacion monitoreo a mas de 50 mil mujeres adultas durante 10 años
# (Archives of Internal Medicine, 171(17), 1571-1578) con la intencion de 
# identificar factores de riesgo de desarrollar un cuadro de depresion. Entre 
# otras cosas, se registro el consumo de cafeina, cuyos datos se resumen en la
# siguiente tabla. ¿Existe alguna asociacion entre la cantidad de cafe que se 
# bebe y la depresion?

# Se propone una hipótesis nula y una hipótesis alternativa 
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

# # Prueba chi - cuadrado de independencia .
prueba <- chisq.test ( tabla , correct = FALSE )
print ( prueba )

#Respuesta B
# El valor p obtenido es muy pequeño p = 0.0003267, menor que el nivel de 
# significacion igual a 0,01, por lo que se rechaza la hipotesis nula a favor
# de la hipotesis alternativa, por lo que se concluye con 99% de cofianza
# que las variables Depresion y Consumo de cafe no descafeinado están 
# relacionadas.

# ¿Por qué la familia de pruebas X2 son no parametricas?
# Esta prueba es NO PARAMETRICA porque no menciona algún parámetro. Es mas, 
# no se hace ninguna suposicion sobre la distribucion de la poblacion desde
# donde proviene la muestra analizada.