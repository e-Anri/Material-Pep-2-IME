#Pregunta 1
#Planteamos las hipostesis de acuerdo a lo especificado en el enunciado
# H0: La edad promedio de los pacientes diagnosticados como enfermos es igual a 60,5 años.
# H1: La edad promedio de los pacientes diagnosticados como enfermos es menor a 60,5 años.


#Cargamos el arvhico csv para guardarlo en la variable "poblacion"
#Entre par?ntesis poner el directorio del archivo.csv
poblacion <- read.csv("/Users/Desktop/cardiacos.csv", stringsAsFactors = F)

#Fijamos el tama?o de la poblaci?n en estudio, contando la cantidad de columnas
tamaño.poblacion <- nrow(poblacion)

#Fijamos una semilla para randomizar la selecci?n de nuestra muestra y su tama?o
semilla <- 203
tamaño.muestra <- 40
set.seed(semilla)

muestra <- poblacion[sample(1:tamaño.poblacion,tamaño.muestra),]
edadmuestra <-muestra$edad 
diagnosticomuestra <- muestra$diagnostico

#Creamos un dataframe con las variables edad y diagnostico (para los enfermos)
datos = data.frame(edadmuestra, diagnosticomuestra = "Enfermo" )


#El comando summary resume datos de buena manera igualmente. y tambien obtenemos la
#desviacion estandar
summary(datos$edadmuestra)
sd <- sd(datos$edadmuestra)


# Calcular estimadores puntuales y estadísticos útiles.
mu_0=60.5
alfa <- 0.05

# Aplicar la prueba t de Student.
t.test(datos$edadmuestra,
       alternative = "two.sided",
       mu = mu_0,
       conf.level = 1 - alfa)

# Calculo del poder usando la funcion power.t.test().
poder <- power.t.test(n = 40, 
                        delta=0.5,
                        sd=sd,
                        power = NULL,
                        sig.level = 0.05, 
                        type="one.sample",
                        alternative = "two.sided")

print(poder)

# EL valor p obtenido por la prueba T de Student fue mayor a 0.05, por 
# lo que se rechaza la hipotesis nula a favor de la hipotesis alternativa y
# de acuerdo a la pregunta: ¿respaldan los datos obtenidos la hipótesis de
# un nuevo equipo de investigadores, quienes sugieren que la edad promedio 
# de los pacientes enfermos es menor a 60,5 años? Se puede asegurar con un 
# 95% de confianza que la edad promedio de los pacientes diagnosticados 
# como enfermos es menor a 60,5 años.

# El poder estadístico de la prueba realizada resulto igual a  0.0564305.

#Pregunta 2 


# En Chile la poblcación ha comenzado a creer que no hay diferencias entre la
#efectividad de proteger contra el COVID-19 una vacuna creada en Chile 
# y otra creada en Rusia

# Se formulan las hipotesis
# H0: La diferencia de efectividad de las vacunas contra el COVID es igual a 0
# H1: La diferencia de efectividad de las vacunas contra el COVID es distinta a 0

# Se propone una hip?tesis nula y una hip?tesis alternativa 
# H0: efectividadVacunaChile - efectividadVacunaRusia  = 0
# H1: efectividadVacunaChile - efectividadVacunaRusia != 0

# Construir la tabla de contingencia
chileno <- seq ( 1 : 12 )
vacunaChilena <- c ( "Cree",
                     "No cree",
                     "Cree",
                     "No cree",
                     rep ( "Cree", 2 ),
                     rep ( "No cree", 3 ),
                     rep ( "No cree", 3 ))

vacunaRusa <- c ( rep ( "Cree", 4 ) , 
                  rep ( "No cree", 8 ) )

datos <- data.frame ( chileno , vacunaChilena , vacunaRusa )
tabla <- table ( vacunaRusa , vacunaChilena )
addmargins ( tabla )

# Aplicar prueba de McNemar
mcnemar.test ( tabla )

# Como se obtiene un valor p = 1, que es mayor al nivel de significacion 0.05
# Se falla al rechazar la hipotesis nula y se concluye que los chilenos
# no hayan una diferencia de efectividad de vacunas creadas en Chile y en Rusia

#Pregunta 3

#En el estudio se entrevisto al doble de hombres (masculino) que de mujeres (femenino)

#La proporcion de gente con hipertrofia del ventriculo izquierdo
#es la misma entre hombres y mujeres.

#La proporcion de gente con normalidad es la misma entre hombres y mujeres.

#la anormalidad de onda st-t es un poco mas frecuente en mujeres que en hombres.

