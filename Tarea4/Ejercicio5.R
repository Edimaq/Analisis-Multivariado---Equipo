# -------------------------------------------------------------------------
# Análisis de Correspondencias y Comparación de Grupos
# -------------------------------------------------------------------------

# Cargamos los paquetes necesarios
library(FactoMineR)  # Para el análisis de correspondencias
library(factoextra)   # Para la visualización de resultados
library(ggplot2)     

#Ejercicio 5 - Tarea 4

#Equipo 6:
#-Reyes Maqueda Edgar
#-Reynoso Torres Ari Genaro
#-Villarreal Maldonado Andre

# -------------------------------------------------------------------------
# i. Análisis de Correspondencias
# -------------------------------------------------------------------------

# Cargamos los datos desde el archivo (simulación)
# Cargamos tambien el archivo de texto disciplinas_recursos.txt 
disciplinas <- c("Geología", "Bioquímica", "Química", "Zoología", "Física",
                 "Ingeniería", "Microbiología", "Botánica", "Estadística", "Matemáticas")

datos <- matrix(c(
  3, 19, 39, 14, 10,
  1, 2, 13, 1, 12,
  6, 25, 49, 21, 29,
  3, 15, 41, 35, 26,
  10, 22, 47, 9, 26,
  3, 11, 25, 15, 34,
  1, 6, 14, 5, 11,
  0, 12, 34, 17, 23,
  2, 5, 11, 4, 7,
  2, 11, 37, 8, 20
), nrow = 10, byrow = TRUE, dimnames = list(disciplinas, LETTERS[1:5]))

# Verificamos la estructura de los datos
print("Tabla de contingencia original:")
print(datos)

# Hacemos el Análisis de Correspondencias (CA)
ca_result <- CA(datos, graph = FALSE)  # graph = FALSE para personalizar visualización

# Vamos a interpretar los resultados
# a. Estadísticos básicos
print("Inercia total y valores propios:")
print(ca_result$eig)

# b. Contribuciones de filas y columnas
print("Contribuciones de las disciplinas:")
print(round(ca_result$row$contrib, 2))

print("Contribuciones de los recursos:")
print(round(ca_result$col$contrib, 2))

# c. Prueba Chi-cuadrado
chi2_test <- chisq.test(datos)
print("Resultado de la prueba Chi-cuadrado:")
print(chi2_test)

# Hacemos una visualización
# Gráfico de correspondencias
fviz_ca_biplot(ca_result,
               repel = TRUE, 
               title = "Mapa de Correspondencias",
               xlab = "Dimensión 1 (58.5% de inercia)",
               ylab = "Dimensión 2 (28.3% de inercia)") +
  theme_minimal()

# -------------------------------------------------------------------------
# ii. Comparación de investigadores de museos
# -------------------------------------------------------------------------

# Vemos los datos proporcionados
observados_museos <- c(A = 4, B = 12, C = 11, D = 19, E = 7)

# Calculamos las proporciones esperadas basadas en el total
total_general <- colSums(datos)
proporciones_esperadas <- total_general / sum(total_general)

# Calculamos ahora los valores esperados para 53 investigadores
esperados_museos <- 53 * proporciones_esperadas

# Vamos a hacer el calculo de los residuales estandarizados
residuales <- (observados_museos - esperados_museos) / sqrt(esperados_museos)

# Creamos la tabla comparativa
tabla_comparativa <- data.frame(
  Recurso = names(observados_museos),
  Observado = observados_museos,
  Esperado = round(esperados_museos, 1),
  Residual = round(residuales, 2)
)

print("Comparación investigadores de museos vs esperado:")
print(tabla_comparativa)

# Hacemos una prueba Chi-cuadrado de bondad de ajuste
chi2_museos <- chisq.test(observados_museos, p = proporciones_esperadas)

print("Resultado prueba Chi-cuadrado para museos:")
print(chi2_museos)

# -------------------------------------------------------------------------
# Visualización adicional para la comparación
# -------------------------------------------------------------------------

# Gráfico de barras comparativo
ggplot(tabla_comparativa, aes(x = Recurso, y = Residual)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", color = "red") +
  labs(title = "Residuales Estandarizados: Museos vs Esperado",
       y = "Residual Estandarizado",
       x = "Recurso") +
  theme_minimal()
