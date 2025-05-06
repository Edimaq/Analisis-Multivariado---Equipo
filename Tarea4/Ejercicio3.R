# -------------------------------------------------------------------------
# Cargamos los paquetes necesarios
# -------------------------------------------------------------------------
library(ade4)     # Para verificar euclideanidad y ajuste con constante
library(vegan)    # Para NMDS
library(ggplot2)  # Para visualización

# -------------------------------------------------------------------------
# Cargamos la matriz de distancias
# -------------------------------------------------------------------------
# Leemos el archivo
distancias <- as.matrix(read.table("dist_ciudades_RU.txt"))
nombres_ciudades <- c("Ciudad1", "Ciudad2", "Ciudad3", "Ciudad4", "Ciudad5",
                      "Ciudad6", "Ciudad7", "Ciudad8", "Ciudad9", "Ciudad10",
                      "Ciudad11", "Ciudad12", "Ciudad13", "Ciudad14")

# Verificamos la estructura
print("Matriz de distancias cargada:")
print(distancias[1:3, 1:3]) # Muestra una submatriz 3x3

# -------------------------------------------------------------------------
# i. Escalamiento Multidimensional Clásico (MDS)
# -------------------------------------------------------------------------
mds_classic <- cmdscale(distancias, k = 2, eig = TRUE)

# Graficamos resultados
df_mds <- data.frame(
  D1 = mds_classic$points[,1],
  D2 = mds_classic$points[,2],
  Ciudad = nombres_ciudades
)

ggplot(df_mds, aes(x = D1, y = D2, label = Ciudad)) +
  geom_point(color = "blue") +
  geom_text(vjust = 1.5, size = 3) +
  ggtitle("MDS Clásico - Reconstrucción del Mapa del Reino Unido") +
  theme_minimal()

# -------------------------------------------------------------------------
# ii. Verificamos la Euclideanidad y ajustar con constante aditiva
# -------------------------------------------------------------------------
# Verificamos si la matriz es euclidiana
is_euclid <- is.euclid(distancias)
print(paste("¿La matriz es euclidiana?", is_euclid))

# Si no es euclidiana, calcular constante aditiva (método de Cailliez)
if (!is_euclid) {
  ajuste <- cailliez(distancias)
  dist_ajustada <- ajuste$adjusted
  print(paste("Constante aditiva:", ajuste$constant))
  
  # Aplicamos MDS a la matriz ajustada
  mds_ajustado <- cmdscale(dist_ajustada, k = 2, eig = TRUE)
  
  # Graficamos el resultado ajustado
  df_ajustado <- data.frame(
    D1 = mds_ajustado$points[,1],
    D2 = mds_ajustado$points[,2],
    Ciudad = nombres_ciudades
  )
  
  ggplot(df_ajustado, aes(x = D1, y = D2, label = Ciudad)) +
    geom_point(color = "red") +
    geom_text(vjust = 1.5, size = 3) +
    ggtitle("MDS con Constante Aditiva - Mapa Ajustado") +
    theme_minimal()
}

# -------------------------------------------------------------------------
# iii. Escalamiento Multidimensional No Métrico (NMDS)
# -------------------------------------------------------------------------
nmds_result <- metaMDS(distancias, k = 2, trymax = 100)

# Graficamos NMDS
df_nmds <- data.frame(
  NMDS1 = nmds_result$points[,1],
  NMDS2 = nmds_result$points[,2],
  Ciudad = nombres_ciudades
)

ggplot(df_nmds, aes(x = NMDS1, y = NMDS2, label = Ciudad)) +
  geom_point(color = "darkgreen") +
  geom_text(vjust = 1.5, size = 3) +
  ggtitle("NMDS - Reconstrucción No Métrica") +
  theme_minimal()

# Evaluamos stress (bondad de ajuste)
print(paste("Stress del NMDS:", nmds_result$stress))

# -------------------------------------------------------------------------
# Ya para finalizar hacemos una comparación de resultados
# -------------------------------------------------------------------------
# Los gráficos generados permiten comparar visualmente las configuraciones.
# El stress < 0.2 indica una buena representación en NMDS.
