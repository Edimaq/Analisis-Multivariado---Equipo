#Librerias usadas
library(psych)
library(GPArotation)
library(corrplot)

#Ejercicio 9 - Tarea 3

#Equipo 6:
#-Reyes Maqueda Edgar
#-Reynoso Torres Ari Genaro
#-Villarreal Maldonado Andre
# Leemos los datos como vector y convertimos a matriz
cor_data <- scan("R_FA.txt", sep = ",")
cor_matrix <- matrix(NA, nrow = 9, ncol = 9)
cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- cor_data
cor_matrix[upper.tri(cor_matrix)] <- t(cor_matrix)[upper.tri(cor_matrix)]
colnames(cor_matrix) <- rownames(cor_matrix) <- letters[1:9]

# Visualizamos la matriz de correlación
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")

# 2. Verificamos la adecuación del análisis factorial ---------------------------------------
# Test de esfericidad de Bartlett
cortest.bartlett(cor_matrix, n = 123) # p < 0.05 indica adecuación

# Medida de Adecuación Muestral (KMO)
KMO(cor_matrix)$MSA # Valores > 0.6 aceptables

# 3. Determinamos los número de factores ----------------------------------------------------
# Gráfico de sedimentación (scree plot)
scree(cor_matrix, factors = FALSE, main = "Scree Plot")

# Criterio de Kaiser (valores propios > 1)
eigen_values <- eigen(cor_matrix)$values
cat("Valores propios:\n", eigen_values, "\nFactores sugeridos:", sum(eigen_values > 1))

# 4. Ejecutamos el análisis factorial con rotación Varimax ---------------------------------
fa_result <- fa(cor_matrix, nfactors = 2, rotate = "varimax", fm = "ml", n.obs = 123)

# Interpretación de resultados -----------------------------------------------------
# Cargas factoriales (|>0.4| se consideran significativas)
print(fa_result$loadings, cutoff = 0.4, sort = TRUE)

# Visualizar cargas factoriales
fa.diagram(fa_result, main = "Cargas Factoriales")

# Comunalidades
cat("\nComunalidades:\n")
print(fa_result$communalities)

# Varianza explicada
cat("\nVarianza explicada por cada factor:", fa_result$Vaccounted[2, ])
