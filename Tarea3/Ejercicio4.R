#Librerias usadas
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(psych)

#Ejercicio 4 - Tarea 3

#Equipo 6:
#-Reyes Maqueda Edgar
#-Reynoso Torres Ari Genaro
#-Villarreal Maldonado Andre

# Cargamos y preparamos los datos ------------------------------------------------------
# Leer el archivo de datos
oecd_data <- read.delim("oecd.txt", na.strings = c("NA", "NaN", ""), check.names = FALSE)

# Inspecciónamos 
str(oecd_data)
summary(oecd_data)

# Eliminamos las columnas no numéricas (países y códigos)
data_numeric <- oecd_data %>% 
  select(-c("countries", "countries_short"))

# Realizamos el manejo de valores faltantes:
# Eliminar variables con más del 20% de datos faltantes
data_clean <- data_numeric %>% 
  select(where(~sum(is.na(.x))/length(.x) < 0.2))

# Eliminanos las filas con NAs restantes 
data_clean <- na.omit(data_clean)

# Estandarización de variables (PCA es sensible a escalas)
scaled_data <- scale(data_clean)

# Continuamos con el análisis de Adecuación -----------------------------------------------------------
# Test de esfericidad de Bartlett
cortest.bartlett(scaled_data)  # p < 0.05 indica adecuación

# Medida de Adecuación Muestral de Kaiser (KMO)
KMO(scaled_data)$MSA  # Valores > 0.5 aceptables

# Ejecutamos el PCA -----------------------------------------------------------------
pca_result <- PCA(scaled_data, graph = FALSE)

# Análisis de Resultados -----------------------------------------------------------
# Varianza explicada
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))
cat("Varianza explicada por los primeros 5 componentes:\n")
print(get_eig(pca_result)[1:5,])

# Cargas factoriales (contribución de variables)
var <- get_pca_var(pca_result)
fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Interpretación de componentes (primeros 2)
cat("\nVariables más relevantes en PC1 y PC2:\n")
print(head(var$contrib[, 1:2], 10))

# Biplot de países y variables
fviz_pca_biplot(pca_result, 
                col.ind = "cos2", col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE)

# Interpretación Final -------------------------------------------------------------
# Identificamos los países extremos en componentes
countries <- oecd_data$countries[as.numeric(rownames(data_clean))]
ind_coord <- as.data.frame(pca_result$ind$coord)
rownames(ind_coord) <- countries

cat("\nPaíses con valores extremos en PC1:\n")
print(ind_coord[order(ind_coord$Dim.1, decreasing = TRUE)[1:5],])

cat("\nPaíses con valores extremos en PC2:\n")
print(ind_coord[order(ind_coord$Dim.2, decreasing = TRUE)[1:5],])

