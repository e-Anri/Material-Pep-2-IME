#A
library ( ggpubr )
library ( ez )
texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 29 16.6 19.3 20.2 22
'brock400_4' 33 16.8 19.3 20.4 22
'C2000.9' 80 51.2 59.6 62.4 66
'c-fat500-10' 126 125 125 125 126
'hamming10-2' 512 243.2 419.1 422.4 512
'johnson32-2-4' 16 16 16 16 16
'keller6' 59 34.5 43 45.5 48.2
'MANN_a81' 1100 1082.2 1082.2 1082.2 1095.9
'p-hat1500-1' 12 6.9 8.1 8.9 10
'p-hat1500-3' 94 42.8 77.7 84.6 86
'san1000' 15 7.6 7.6 7.7 10
'san400_0.7_1' 40 19.6 20.5 20.5 21
'san400_0.9_1' 100 44.1 54.4 56.4 92
'frb100-40' 100 66.4 76.7 80.5 82
'frb59-26-1' 59 39.2 45.9 48.3 48
'1et.2048' 316 232.4 268.4 280.9 292.4
'1zc.4096' 379 253.8 293.2 307.4 328.5
'2dc.2048' 24 15.6 18.7 19.9 21
")
datos <- read.table(textConnection(texto), header = TRUE)

#B
texto1 <- ("
gender words colors interfer
1 19 15 31
1 21 20 38
1 9 25 38
1 21 19 32
1 16 15 29
1 16 16 36
1 17 23 34
1 21 19 44
1 9 14 42
1 23 17 37
1 18 19 31
2 26 24 33
2 18 19 44
2 17 21 31
2 12 5 44
2 21 17 35
2 19 21 34
2 16 15 32
2 13 22 47
2 13 24 29
2 15 19 38
2 15 26 42
")
datos1 <- read.table(textConnection(texto1), header = TRUE)

#1
#hipotesis nula: no existen diferencias significativas entre los algortimos
#hipotesis alternativa: existe al menos 1 algortimos con diferencia significativa
a <- datos$R
b <- datos$R2
c <- datos$R3
d <- datos$G

Tiempo <- c (a , b , c , d)
Algoritmo <- c ( rep ( " R " , length ( a ) ) ,
             rep ( "R2 " , length ( b ) ) ,
             rep ( "R3" , length ( c ) ) ,
             rep ( "G" , length ( d ) ))

Algoritmo <- factor ( Algoritmo )
instancia <- factor ( seq ( 1 , 72 , by = 1 ) )
datos2 <- data.frame ( instancia , Algoritmo , Tiempo )
alfa = 0.01
# Comprobci ó n de normalidad .
g <- ggqqplot ( datos2 ,
                x = "Tiempo" ,
                y = "Algoritmo" ,
                color = "Algoritmo" )

g <- g + facet_wrap (~ Algoritmo )
g <- g + rremove ( "x.ticks" ) + rremove ("x.text")
g <- g + rremove ( "y.ticks" ) + rremove ("y.text")
g <- g + rremove ( "axis.title" )
print ( g ) # es normal

# Procedimiento ANOVA con aov .
prueba <- aov (Tiempo~Algoritmo,
               data = datos2 )
cat ( " \ nResultado de la prueba ANOVA para muestras correlacionadas con aov \ n " )
print ( summary ( prueba ) )


# Procedimiento post - hoc de Bonferroni .
bonferroni <- pairwise.t.test ( Tiempo ,
                                Algoritmo ,
                                p.adj = "bonferroni" ,
                                paired = TRUE)

cat ( " Correcci ó n de Bonferroni \ n " )
print ( bonferroni )


#dado los resultados obtenidos y ajustados para comparar con el p valor, 
#se obtiene que,con un nivel de confianza de 0.01, no existe diferencia significativa 
#entre ninguno de los algoritmos


#2
#hipotesis nula: no existen diferencias significativas entre las tareas
#hipotesis alternativa: existe al menos 1 tarea con diferencia significativa
a1 <- datos1$words
b1 <- datos1$colors
c1 <- datos1$interfer


Cantidad <- c (a1 , b1 , c1 )
Tarea <- c ( rep ( " words " , length ( a1 ) ) ,
                 rep ( "colors " , length ( b1 ) ) ,
                 rep ( "interfer" , length ( c1 ) ) )

Tarea <- factor ( Tarea )
instancia1 <- factor ( seq ( 1 , 66 , by = 1 ) )
datos3 <- data.frame ( instancia1 , Tarea , Cantidad )
alfa = 0.025
# Comprobci ó n de normalidad .
g <- ggqqplot ( datos3 ,
                   x = "Cantidad" ,
                   y = "Tarea" ,
                   color = "Tarea" )

g <- g + facet_wrap (~ Tarea )
g <- g + rremove ( "x.ticks" ) + rremove ( "x.text" )
g <- g + rremove ( "y.ticks" ) + rremove ( "y.text" )
g <- g + rremove ( "axis.title" )
print ( g ) #es normal

# Procedimiento ANOVA con aov .
prueba1 <- aov (Cantidad~Tarea,
              data = datos3 )
cat ( " \ nResultado de la prueba ANOVA para muestras correlacionadas con aov \ n " )
print ( summary ( prueba1 ) )


# Procedimiento post - hoc de holm .
holm <- pairwise.t.test ( Cantidad ,
                                Tarea ,
                                       p.adj = "holm" ,
                                       paired = TRUE )

 cat ( " Correccion de holm \ n " )
print ( holm )


#dado los resultados obtenidos y ajustados para comparar con el p valor, 
#se obtiene que, con un nivel de confianza del 0.01, existe una diferencia significativa,
#entre las tareas de colors e interfer y entre words e interfer