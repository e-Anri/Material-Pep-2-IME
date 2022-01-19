library(pwr)
"
Ejercicio practico 5
"

"
ENUNCIADO

Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) 
(Journal of chronic diseases, 25(12), 711-716) sobre la incidencia de la 
cantidad de alcohol y de tabaco que se consume en el riesgo de padecer cáncer oral.
"
# Datos de tablas

alcohol <- data.frame(
  consumo = factor(c("0", "1-9", "10-44", "45+")),
  casos = c(43,89,109,242),
  controles = c(108, 141, 91, 107)
)

tabaco <- data.frame(
  consumo = factor(c("0", "1-19", "20-39", "40+")),
  casos = c(26, 66, 248, 143),
  controles = c(85, 97, 197, 68)
)


#PROBLEMA C

"1. Suponiendo que la diferencia en la proporción de personas que desarrollan
la enfermedad entre quienes beben de 1 a 9 ml de alcohol por día y aquellos que
beben de 10 a 44 ml al día es de 0.10. ¿Cuánta gente deberíamos entrevistar
para obtener un intervalo de confianza del 95% y poder estadístico de 80%?
"

# Se trata de una diferencia de proporciones entre 2 grupos de personas
# Siendo la H0 de quienes beben de 10 a 44 ml al dia es de un 10% de cancer oral 
# que quines beben de 1 a 9 de alcohol por dia

#Datos
cantidad <- pwr.2p.test(h=0.1,
                        sig.level = 0.05,
                        power = 0.8,
                        alternative = "two.sided")$n
                        
print(cantidad)

"
La cantidad de gente que se debería entrevistar para obtener un intervalo de 
confianza del 95% y poder estadístico de 80%, sabiendo que la diferencia
de proporción de personas quienes beben de 1 a 9 ml de alcohol por día y
aquellos que beben de 10 a 44 ml es de 1570 aproximadamente
#es aproximadamente de 392.443026235748

"


"
2. Estudios previos habían determinado que la incidencia de cáncer oral en la
población que bebe regularmente entre 1 y 9 ml de alcohol era de 25%.
¿Respaldan estos datos tal estimación?
"
# una prueba con una proporcion
"
Hipotesis
- H0: Incidencia cáncer oral en la población que bebe regularmente entre 1 y 9 ml
de alcohol = 25%
- H1: Incidencia cáncer oral en la población que bebe regularmente entre 1 y 9 ml
de alcohol != 25%
"
alpha <- 0.05 
casos <- alcohol[["casos"]][2]
controles <- alcohol[["controles"]][2]
n <- casos + controles

ptest1 <- prop.test(x = casos,
                    n = n,
                    p = 0.25,
                    alternative = "two.sided",
                    conf.level = 1- alpha,
                    correct = FALSE)
print(ptest1)

"La probabilidad es menor al nivel de significancia considerado de 0,05, por lo
que es improbable observar esos numeros en una muestra, siendo no respaldad"




"
3. Según estos datos, ¿da lo mismo beber entre 1 y 9 ml de alcohol diariamente
que hacerlo de 10 a 44 ml?
"

casos <- alcohol[["casos"]][2]
controles <- alcohol[["controles"]][2]
n1 <- casos + controles

proporcion_1a9 <- casos / n1

casos <- alcohol[["casos"]][3]
controles <- alcohol[["controles"]][3]
n2 <- casos + controles
proporcion_10a44 <- casos / n2

"
incidientemente un 55% y un 38% respectivamente de beber entre 1 y 9 ml de 
alcohol diariamente que hacerlo de 10 a 44 ml, por lo tanto, no da lo mismo

"
