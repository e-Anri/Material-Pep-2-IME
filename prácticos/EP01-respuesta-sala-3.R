###########
#Preguntas#
###########
#�qu� variables se han cargado?
#R: Las varaibles que se cargaron fueron las siguientes: 
# Region: que corresponde al nombre de la region,
# Casos: corresponde a la cantidad de contagiados con sintomas
# Fecha: es la fecha donde se registr� el numeros de Casos en el dia

#�qu� tipo tiene cada una de estas variables? 
#R: La variable Region es del tipo Categ�rica nominal,
# La variable Casos es del tipo Num�rica discreta y
# La variable Fecha es del tipo Categ�rica ordinal

#�qu� escala parecen tener estas variables?
#R: Region tiene una escala nominal, Casos tiene una escala de raz�n y Fecha tiene una escala ordinal


library(tidyr)
library(dplyr)

datos <- read.csv("casos.csv", encoding = "UTF-8")

#Se han cargado un conjunto de variables respecto a las fechas desde 03/03/2020 hasta 23/09/2021 cada dia es una variable
#Cada variable es de tipo num�rico
#Escala de Raz�n

biobio <- filter(datos,Region == "Biob�o")

headd <- names(biobio)[-1]

datos_largos <- biobio %>% pivot_longer(headd,names_to = "Fechas", values_to = "Casos")

fechas_X <- "X2021.01.01" <= headd & "X2021.06.30" >= headd

final.fechas <- headd[fechas_X]
final.casos <- datos_largos[fechas_X, "Casos"]
diaMax <- which.max(final.casos[["Casos"]])

print("el dia con m�s casos es el d�a: ")
print(final.fechas[diaMax])




# //////// Ejercicio 2 ////////

# Creamos una tabla por cada mes, con fechas y casos de contagios
enero <- datos_largos %>% filter("X2021.01.01" <= headd & "X2021.01.31" >= headd)
febrero <- datos_largos %>% filter("X2021.02.01" <= headd & "X2021.02.28" >= headd)
marzo <- datos_largos %>% filter("X2021.03.01" <= headd & "X2021.03.31" >= headd)
abril <- datos_largos %>% filter("X2021.04.01" <= headd & "X2021.04.30" >= headd)
mayo <- datos_largos %>% filter("X2021.05.01" <= headd & "X2021.05.31" >= headd)
junio <- datos_largos %>% filter("X2021.06.01" <= headd & "X2021.06.30" >= headd)


# Calculamos el total de contagiados seg�n el mes
casos.enero = colSums (select (enero, contains ("Casos")))
casos.febrero = colSums (select (febrero, contains ("Casos")))
casos.marzo = colSums (select (marzo, contains ("Casos")))
casos.abril = colSums (select (abril, contains ("Casos")))
casos.mayo = colSums (select (mayo, contains ("Casos")))
casos.junio = colSums (select (junio, contains ("Casos")))


# Creamos una tabla con el total de contagios segun el mes
Total_mensual <- data.frame("Enero" = casos.enero, "Febrero" = casos.febrero, 
                            "Marzo" = casos.marzo, "Abril" = casos.abril, 
                            "Mayo" = casos.mayo, "Junio" = casos.junio)















