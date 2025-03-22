import numpy as np
import matplotlib.pyplot as plt



""""
i)
Para implementar el algoritmo de la urna con K=3 colores y parámetros iniciales α=(2,5,1), necesitamos:

Inicializar la urna con el número correcto de bolas de cada color
Simular el proceso de extraer una bola y añadir una nueva del mismo color N veces
Calcular las proporciones finales

La implementación del algoritmo es:
""""

def urn_algorithm(alpha, N, n_samples=1):
    K = len(alpha)
    # Inicializar la urna
    urn = np.array(alpha)
    
    # Lista para almacenar muestras
    samples = []
    
    # Realizar N extracciones
    for _ in range(N):
        # Calcular probabilidades de extracción
        probs = urn / urn.sum()
        
        # Extraer una bola (seleccionar un color)
        color = np.random.choice(K, p=probs)
        
        # Añadir una bola del mismo color
        urn[color] += 1
        
        # Guardar muestra en algunos puntos del proceso
        if N >= n_samples and _ % (N // n_samples) == 0:
            samples.append(urn / urn.sum())
    
    # Convertir a array de numpy
    return np.array(samples)

# Aplicación con los parámetros dados
alpha = np.array([2, 5, 1])
N = 10000  # "Suficientemente grande"
n = 100    # Tamaño de muestra
samples = urn_algorithm(alpha, N, n)

# Las proporciones finales en la urna representan una muestra de la distribución Dirichlet(2, 5, 1)

""""
ii)
El método más eficiente para generar muestras de una distribución Dirichlet es utilizando la transformación de variables aleatorias gamma. Para una distribución Dirichlet con parámetros α=(α1​,α2​,...,αK​)
""""

def dirichlet_from_gamma(alpha, n_samples=1):
    K = len(alpha)
    samples = np.zeros((n_samples, K))
    
    for i in range(n_samples):
        # Generar variables aleatorias gamma
        gammas = np.array([np.random.gamma(shape=a, scale=1.0) for a in alpha])
        
        # Normalizar para obtener distribución Dirichlet
        samples[i] = gammas / np.sum(gammas)
    
    return samples

# Aplicación con los parámetros dados
alpha = np.array([2, 5, 1])
n = 100  # Tamaño de muestra
samples_gamma = dirichlet_from_gamma(alpha, n)

# Visualización de muestras generadas mediante el método de transformación gamma:

""""
iii)
Análisis de eficiencia


Precisión estadística: Ambos métodos convergen a los valores teóricos esperados, pero el método de transformación gamma alcanza la convergencia con menos muestras.


Eficiencia computacional:

El método de la urna requiere O(N) operaciones para generar cada muestra, donde N es el número de iteraciones.
El método de transformación gamma requiere O(K) operaciones, donde K es el número de dimensiones.



Escalabilidad:

El método de la urna necesita valores grandes de N para aproximar adecuadamente la distribución Dirichlet.
El método de transformación gamma genera muestras exactas de la distribución Dirichlet independientemente del número de muestras.



Por lo tanto
El algoritmo de transformación de variables aleatorias gamma es significativamente más eficiente computacionalmente y produce muestras exactas de la distribución Dirichlet con un menor esfuerzo computacional. Además, no requiere determinar cuán "suficientemente grande" debe ser N para obtener una buena aproximación.
Para aplicaciones prácticas, el método de transformación gamma es claramente superior, especialmente cuando se necesitan generar muchas muestras o trabajar con dimensiones altas.
""""
