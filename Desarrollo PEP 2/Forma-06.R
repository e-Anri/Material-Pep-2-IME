# PEP 2 IME
# Estefan�a Alvarez (20.371.287-1)
# Stephan Silva (20.298.778-8)



# Pregunta 1

# (23 puntos) Lord Vader desea saber si los niveles de exigencia con que los
# distintos oficiales evaluadores (instructor, capitán, comandante y general) 
# califican a los flametroopers son similares, por lo que le ha solicitado 
# estudiar si existen diferencias significativas en el promedio de la 
# evaluación realizada por cada uno de los oficiales. El Lord Sith ha sido muy 
# claro al solicitar un reporte de aquellos oficiales cuyas evaluaciones
# presenten diferencias.

# Desarrollo:


# Paquetes a utilizar:

if (!require(ggplot2)){
  install.packages("ggplot2", dependencies = TRUE )
  require (ggplot2)
}
if (!require(ez)){
  install.packages("ez", dependencies = TRUE )
  require (ez)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

library(tidyr)
library(dplyr)


# Realizando llamado al archivo.csv
datos <- read.csv(file.choose(), encoding = "UTF-8",)

# Nivel de significación:

alpha <- 0.01

# Hipótesis a contrastar:

# H0: Las evaluaciones promedio de los flametrooper es igual para uno de los 
# oficiales evaluadores.

# HA: Existe al menos una evaluación diferente que presenta diferencia frente 
# a las demás.

# Se comienza por definir un nuevo data.frame con los datos acomodados 

datosFlame <- datos %>% filter(datos[["division"]] == "Flametrooper")

# names(datosFlame)<- c("division", "eval_instructor", "eval_capitan", "eval_comandante", "eval_general")

instructor <- datosFlame %>% select(eval_instructor)
capitan <- datosFlame %>% select(eval_capitan)
comandante <- datosFlame %>% select(eval_comandante)
general <- datosFlame %>% select(eval_general)

#instructor <- as.integer(instructor$eval_instructor)
#capitan <- as.integer(capitan$eval_capitan)
#comandante <- as.integer(comandante$eval_comandante)
#general <- as.integer(general$eval_general)

datos2 <- data.frame(instructor, capitan, comandante, general)

class(datos2$eval_instructor)



# Se intenta cambiar el tipo de dato que proviene desde el archivo,
# pues los datos son de tipo character, pero para operar con ellos se
# necesita que sean de tipo numerico o entero

# Cuando tratamos de convertir los valores a entero
# nos tira error:
# Warning message:
# NAs introduced by coercion 


# es por esto que no logramos avanzar más allá en este ejercicio



# DATOS EN FORMATO LARGO
dl <- gather(
  data = datos2,
  key = "Evaluador",
  value = "Resultado",
)
strtoi(dl$Resultado)

dl[["Resultado"]] <- factor(dl[["Resultado"]])

dl$instancia <- factor(1:nrow(dl))



# Comprobación de normalidad a través de un gráfico QQ
# Por alguna razón desconocida, el gráfico sale de una forma no adecuada :(
g1 <- ggqqplot(dl,
               x = "Resultado",
               y = "Evaluador",
               color = "Evaluador")

g1 <- g1 + facet_wrap(~Evaluador)
g1 <- g1 + rremove("x.ticks") + rremove("x.text")
g1 <- g1 + rremove("y.ticks") + rremove("y.text")
g1 <- g1 + rremove("axis.title")
print(g1)

# Es por esto que decidimos utilizar la prueba de 

class(dl$Resultado)

x.test <- shapiro.test(as.numeric(dl$Resultado))

# Por efectos del ejercicio, el método a elegir para esta pregunta es el de 
# ANOVA con muestras correlacionadas, si se hubiera podido transformar los 
# caracteres a entero, no hubiera habido problema para seguir avanzando :(

# Pregunta 2

# (24 puntos) A fin de determinar si es necesario establecer programas de 
# entrenamiento diferenciados para clones y reclutas, Lord Vader quiere saber 
# si es posible distinguir entre ambas clases de soldados con los datos 
# actuales. Para ello, ha solicitado evaluar un modelo clasificador que 
# contemple entre 2 y 5 variables predictoras. Considere que, para ser 
# aceptable, el modelo:

# • Debe lograr una exactitud (accuracy) de al menos 0,8 en datos de prueba
# • No puede considerar casos con demasiada influencia (considerando la 
#   distancia de Cook)

# • No debe presentar autocorrelación (usando la prueba de Durbin-Watson para
#   un retardo y un nivel de significación α = .01)

# • No debe presentar multicolinealidad severa (considerando el factor de 
#   inflación de la varianza, con un VIF promedio inferior a 1,03).

# Considere la semilla 1030 para obtener una muestra de 400 datos, 80% de los
# cuales serán empleados para ajustar el modelo y el 20% restante, para 
# evaluarlo.


# Realizando llamado al archivo.csv
datos <- read.csv(file.choose(), encoding = "UTF-8")


# Considerar semilla

set.seed(1030)
datos <- sample_n(datos, 400)


# Con respecto a las clases de soldados, interpretamos que se refiere a si los soldados
# son clones o no (variable categ�rica)



clones <- datos %>% filter(datos[["es_clon"]] == 1)
no_clones <- datos %>% filter(datos[["es_clon"]] == 0)




# Pregunta 3

# (9 puntos) Proponga un ejemplo novedoso (no mencionado en clase ni que 
# aparezca en las lecturas dadas) en donde un estudio o experimento, 
# relacionado con el sentir de los santiaguinos ante el aumento de la 
# violencia de la delincuencia, necesite utilizar una prueba de suma de 
# rangos de Wilcoxon (también llamada prueba de Mann–Whitney–Wilcoxon o 
# prueba U de Mann-Whitney) debido a problemas con la escala de la variable 
# dependiente en estudio. Indique cuáles serían las variables involucradas 
# en su ejemplo (con sus respectivos niveles) y las hipótesis nula y 
# alternativa a contrastar.

# Respuesta:

# El ministerio de seguridad desea evaluar la efectividad de dos 
# planes alternativos para combatir la delincuencia desatada en Santiago, A y B,
# para decidir cual será implementado en las distintas comunas.
# Con este fin, el ministerio ha seleccionado al azar a 27 voluntarias y 
# voluntarios, quienes son asignados de manera aleatoria a dos grupos, cada uno
# de los cuales debe leer el plan de acción (n_A = 15, n_B = 14). Cada
# participante debe evaluar 7 aspectos de efectividad del plan, cada 
# uno de los cuales se mide con una escala Likert de 7 puntos, donde 1 significa 
# “muy malo” y 7, “muy bueno”. La valoración que cada participante 
# da al plan evaluado corresponde al promedio simple de las puntuaciones 
# de los 7 aspectos evaluados.

# Las variables son:

# Plan A, Plan B y la Valoración  de efectividad (con niveles desde "muy malo" a "muy bueno")

# Hipótesis a contrastar:

# H0: No hay diferencia en la efectividad de ambos planes (se distribuyen de igual forma).

# HA: Sí hay diferencia en la efectividad de ambos planes (distribuciones distintas).






