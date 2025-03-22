#Librerias usadas
library(tidyverse)
library(readxl)
library(GGally)
library(ggplot2)
library(MVN)#Nomalidad Multi

#Ejercicio 10 - Tarea 2

#Equipo 6:
#-
#-
#-
# Cargamos el archivo y lo leemos, considerando que la primera columna es el identificador de los árboles
cork <- read.table("cork.txt", header = TRUE, row.names = 1)
# Verificamos la estructura de los datos en el archivo
dim(cork)  # Debería mostrarnos 28 filas y 4 columnas (N, E, S, W)
head(cork) # Inspeccionamos las primeras filas
# Realizamos la prueba de normalidad multivariada y lo guardamos en 'resultado'
resultado <- mvn(data = cork, mvnTest = "mardia", multivariatePlot = "qq")
# Para interpretar los resultados, la prueba de Mardia evalúa asimetría y curtosis multivariadas. 
# Si los valores p > 0.05, no se rechaza la normalidad multivariada.
print(resultado$multivariateNormality)
# Dado que los valores p de las pruebas de asimetría y curtosis de Mardia son mayores que 0.05, 
# no hay evidencia suficiente para rechazar la normalidad multivariada. Por lo tanto, sí se puede
# asumir que los datos siguen una distribución normal multivariada.
# Ambos valores-p (0.404 y 0.690) son mayores que el nivel de significancia común de 0.05. 
# Esto significa que no se rechaza la hipótesis nula de normalidad multivariada. Por lo tanto, sí 
# se puede asumir que los datos siguen una distribución normal multivariada, según las pruebas aplicadas.

# Ahora podriamos complementar a estas pruebas numericas el evaluar la normalidad multivariada
# mediante gráficos Q-Q multivariados en R
# Gráfico Q-Q multivariado con el paquete MVN
resultado <- mvn(data = cork, 
                 mvnTest = "mardia", 
                 multivariatePlot = "qq",
                 multivariateOutlierMethod = "quan") 

# Personalizar el gráfico
plot(resultado$multivariateNormality$qqplot, 
     main = "Q-Q Multivariado para Cork",
     xlab = "Cuantiles teóricos χ²", 
     ylab = "Distancias de Mahalanobis²")
abline(a = 0, b = 1, col = "red", lwd = 2)  # Línea de referencia y=x
# Para interpretar el resultado de la grafica: Si los puntos se alinean cerca de la línea roja entonces
# Los datos siguen una distribución normal multivariada. Y ademas las desviaciones en la cola superior/inferior son
# evidencia de no normalidad.
# resultado y conclusión: Si los puntos en el Q-Q multivariado se ajustan razonablemente a la línea de referencia
# , no hay evidencia para rechazar la normalidad multivariada.
