"
Ejercicio practico 6
"
# RESOLUCION EJERCICIOS A, B Y C

"
ENUNCIADO EJERCICIO A

Una de las primeras preguntas a responder por el último estudio nacional de
obesidad infantil fue si existían diferencias en la prevalencia de la
obesidad entre niños y niñas o si, por el contrario, el porcentaje de obeso
s no varía entre sexos. Se les solicita responder esta pregunta, contando con
las primeras observaciones obtenidas en el estudio sobre una muestra de 
14 menores:

          Obesidad
Sexo      Sí        No
Niña       1         4        
Niño       7         2

"
cat("Ejercicio A de Obesidad Infantil\n")

# en este ejercicio se puede apreciar que es una tabla de muestras pequeñas y 
# al estar en presencia de datos independientes no pareados se debe trabajar con
# la prueba de Fisher.


# Hipotesis
cat("\n")
cat("H0: Siendo la obesidad infantil independiente del sexo.\n") 
cat("HA: La obesidad infantil dependiente del sexo.\n")

# Datos de la tabla
Niña <- c(1,4)
Niño <- c(7,2)
Tabla <- rbind(Niña,Niño)
colnames(Tabla) <- c("Si","No")

cat("\n")
print(Tabla)

# Nivel de significación
alpha <- 0.05

# Aplicando la funcion de prueba de Fisher

fisher <- fisher.test(Tabla, conf.int = 1-alpha)
cat("El resultado de la prueba con Fisher es: \n")
print(fisher)

cat(" Como se obtiene un valor aproximado de p = 0.09091 siendo mayor al nivel
    nivel de significacion alpha = 0,05, se falla al rechazar H0, por lo tanto,
    la obesidad infantil es independiente del sexo")


#######################################

"
ENUNCIADO EJERCICIO B

En un artículo de García y colaboradores (2010) se describe un estudio en qu
e se compararon diferentes versiones de algoritmos evolutivos para resolver 
variadas instancias de problemas de clasificación tomadas desde el repositorio
UCI Machine Learning. Suponga que la siguiente tabla muestra los resultados de 
la clasificación hecha por dos versiones de un algoritmo genético evaluado en
el estudio para el problema Breast Cancer. ¿Consigue uno de los algoritmos 
mejor desempeño?

            AG v1     AG v2
        Incorrecta    Correcta 
        Correcta      Correcta 
        Incorrecta    Correcta 
        Correcta      Correcta 
        Incorrecta    Incorrecta 
        Incorrecta    Correcta
        Correcta      Correcta 
        Correcta      Incorrecta 
        Correcta      Incorrecta 
        Incorrecta    Correcta   
        Incorrecta    Correcta 
        Incorrecta    Correcta

"
cat("Ejercicio B de Algoritmos\n")

# En este ejercicio se puede observar un caso contrario al ejercicio anterior,
# es decir, muestras pareadas pequeñas con 2 tratamientos distintos por lo que 
# en esta ocacion se trabajara con la prueba de McNemar.

# Hipotesis
cat("\n")
cat("H0: Siendo que los algortimos tienen igual desempeño.\n") 
cat("HA: Los algoritmos tienen distinto desempeño.\n")

# Datos de la tabla
AG_v1 <- c("Incorrecta", "Correcta", "Incorrecta", "Correcta", "Incorrecta",
          "Incorrecta", "Correcta", "Correcta", "Correcta", "Incorrecta", 
          "Incorrecta", "Incorrecta")

AG_v2 <- c("Correcta", "Correcta", "Correcta", "Correcta", "Incorrecta", 
           "Correcta", "Correcta", "Incorrecta", "Incorrecta", "Correcta", 
           "Correcta", "Correcta")
Tabla <- table(data.frame(AG_v1,AG_v2))
print(Tabla)

# Nivel de significación
alpha <- 0.05

# Aplicando la funcion de prueba de McNemar

mcnemar <- mcnemar.test(Tabla)
print(mcnemar)

cat(" Como se obtiene un valor aproximado de p = 0.2888 siendo mayor al nivel
    nivel de significacion alpha = 0,05, se falla al rechazar H0, por lo tanto,
    los dos algoritmos tienen igual desempeño")



#######################################

"
ENUNCIADO EJERCICIO C

Un memorista que está trabajando con el grupo de investigación de algoritmos
para la Web del DIINF-USACH ha diseñado dos algoritmos de búsqueda que
intentan considerar el estado de ánimo del usuario en los parámetros de la
búsqueda. Por supuesto, necesita evaluar estas propuestas y para eso ha
realizado un experimento computacional que mide las veces que el usuario 
necesita hacer solo una búsqueda para encontrar información relevante.
La siguiente tabla muestra los resultados de este experimento, que también 
considera el algoritmo que actualmente utiliza uno de los motores de búsqueda 
más usados. ¿Existe alguna diferencia entre el rendimiento de los
algoritmos probados?



"
cat("Ejercicio C de Memorista\n")

# En este ejercicio se puede observar que se trata de una homogeneidad, al 
# representar varias muestras .... 

# Hipotesis
cat("\n")
cat("H0: Siendo que el rendimiento de los algoritmos es el mismo.\n") 
cat("HA: El rendimiento de al menos un algoritmo no es el mismo.\n")

# Datos de la tabla

una_busqueda <- c(3511, 1749, 1818)
mas_de_una_busqueda <- c(1489, 751, 682)
Tabla <- rbind(una_busqueda, mas_de_una_busqueda)
# Nivel de significación
alpha <- 0.05

# Aplicando la funcion chisq.test de R

chisq <- chisq.test(Tabla)
print(chisq)

cat(" Como se obtiene un valor aproximado de p = 0.04688, se acepta al rechazar 
      H0, por lo tanto, el rendimiento entre los algoritmos con nivel de 
      confianza 0,05 tendria diferencias")