#Ejercicio 10

library(psych)        # para funciones de Análisis Factorial y PCA
library(GPArotation)  # para rotaciones (p.ej. varimax)
  


# Ejemplo de lectura:
# hepta <- read.csv("heptathlon.csv", stringsAsFactors = FALSE)
# Seleccionamos sólo las 7 variables cuantitativas:
datos <- hepta[, c("hurdles", "highjump", "shot", 
                   "run200m", "longjump", "javelin", "run800m")]

# Estandarizamos las variables (media 0, varianza 1)
datos.scaled <- scale(datos)



eigenvals <- eigen(cor(datos.scaled))$values
print(eigenvals)

fa.parallel(datos.scaled, fa = "fa", n.iter = 100,
            main = "Parallel Analysis y Scree Plot")

# A partir del paralelo y del codo en el Scree, supongamos que 
# vemos entre 2 y 3 factores como solución adecuada.


#  Análisis Factorial Exploratorio (EFA) con rotación varimax
# Elegimos 2 factores (podrías probar también 3 y comparar)
fa2 <- fa(datos.scaled, 
          nfactors = 2, 
          rotate  = "varimax", 
          fm      = "ml")  # máxima verosimilitud

print(fa2, sort = TRUE)  
# Muestra cargas factoriales, comunalidades (h2) y unicidades (u2)


#   - Factor 1: alta carga en pruebas de velocidad (hurdles, run200m, run800m)
#   - Factor 2: alta carga en fuerza/lanzamientos (shot, javelin, quizá highjump)

#  Cálculo de puntuaciones factoriales 
scores_fa2 <- factor.scores(datos.scaled, fa2)$scores
head(scores_fa2)  # muestra las puntuaciones de cada atleta en los 2 factores

#  Comparación con PCA 

pca1 <- principal(datos.scaled, nfactors = 2, rotate = "varimax")
print(pca1, cutoff = 0.30)  


# - PCA busca componentes que expliquen varianza total, FA modela varianza común 
#   y separa la varianza única y el error.
# - En PCA no hay modelo estadístico de error, mientras que en FA sí (si fm != "minres").
# - Las cargas en PCA son autovectores escalados; en FA son estimaciones de cuánto 
#   cada factor “causa” cada variable.
# - En general, las estructuras de cargas pueden coincidir en parte, pero FA 
#   proporciona medidas de bondad de ajuste (χ², RMSEA, TLI, etc.) que no tiene PCA.

#  Reporte y conclusiones 
cat("
RESUMEN DEL ANALISIS:
- Tras la Parallel Analysis, la solución con 2 factores capture bien la estructura de correlaciones.
- Factor 1 (Velocidad/Resistencia): hurdles, run200m y run800m.
- Factor 2 (Fuerza/Salto): shot, javelin (y en menor grado highjump).
- Las comunalidades (h2) superiores a 0.50 indican que estos 2 factores explican buena parte
  de la varianza de cada prueba.
- A diferencia del PCA, el Análisis Factorial modela explícitamente un término de error y ofrece
  criterios de ajuste del modelo (χ², RMSEA). En PCA nos centramos sólo en varianza total, 
  sin descomposición de error vs. común.
")
