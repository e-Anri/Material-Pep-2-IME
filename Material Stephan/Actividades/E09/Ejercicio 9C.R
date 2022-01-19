# Problema C
# En trabajo de título de un estudiante del DIINF, se reportan los siguientes 
# tiempos de ejecución ('Tpo' en milisegundos) medidos para dos versiones de un
# algoritmo genético (A6 y B12) para resolver instancias del problema del 
# vendedor viajero disponibles en repositorios públicos. 
# ¿Es uno de los algoritmos más rápido que el otro?
   
# H0: No hay diferencias significativas en los tiempos de ejecución de las dos 
#     versiones de un algoritmo genético.
# HA: La version A6 tiene tiempos de ejecucion menores a la version B12.

# En resumen:

# H0: Tpo_A6 - Tpo_B12 = 0 
# HA: Tpo_A6 - Tpo_B12 < 0 


texto <-("
      Instancia 'Tpo_A6' 'Tpo_B12'
      'rat575' 33349 32444
      'u724' 55026 64019
      'd657' 43352 52696
      'rat783' 65076 76857
      'u574' 112326 123456
      'pr1002' 136262 162808
      'fl1577' 3234574 3192222
      'nrw1379' 335608 393213
      'd1291' 268964 335566
      'u1432' 398653 472597
      'pcb1173' 303634 234658
      'fl1400' 337977 430748
      'u2152' 3073534 3253423
      'rl1323' 243679 132654
      'rl1304' 342321 231254
      'u1817' 876432 672542
      'vm1084' 413672 543215
      'rl1889' 1876432 854213
      'pr2392' 6764986 8765321
      'u1060' 3453176 432876
      ")

datos <- read.table(textConnection(texto), header = TRUE)

# Establecer nivel de significacion.
alfa <- 0.05

# Hacer la prueba de rangos con signo de Wilcoxon.
prueba <- wilcox.test(datos$Tpo_A6, 
                      datos$Tpo_B12,
                      alternative = "less",
                      paired = TRUE ,
                      conf.level = 1 - alfa)
print(prueba)

# Respuesta Problema C
# Dados los resultados obtenidos con la prueba de de rangos con signo de 
# Wilcoxon, donde se obtiene p = 0.4347, mayor al nivel de significación 0.05
# por lo que se falla en rechachar la hipotesis nula y se concluye que con un 
# 95% de confianza que noo hay diferencias significativas en los tiempos de 
# ejecución de las dos versiones de un algoritmo genético.

# Respondiendo a la interrogante ¿Es uno de los algoritmos más rápido que el 
# otro? No, se podría decir que no hay un algoritmo más rápido que otro, debido
# a que ambos tienen tiempos de ejecución similares.
