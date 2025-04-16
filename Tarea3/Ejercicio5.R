#Librerias usadas
library(ggplot2)
library(GGally)
library(ggthemes)
library(mvtnorm)
library(car)
library(factoextra)
library(here)
library(ggfortify)

#Ejercicio 5 - Tarea 3

#Equipo 6:
#-Reyes Maqueda Edgar
#-Reynoso Torres Ari Genaro
#-Villarreal Maldonado Andre

#Cargamos el archivo de 'heptathlon_2016.txt' de nuestro directorio y lo cargamos en un data frame
file<-"C:\\Users\\edima\\Downloads\\heptathlon_2016 (1).txt" #Cambiarlo para cada equipo
hep<-read.delim(file,header = TRUE,sep = "\t", dec = ".")
#Contiene como datos:
#name - Nombre del deportista y pais de procedencia
#hurdles - 100 m vallas en segundos
#highjump - Salto de altura en metros
#shot - Lanzamiento de peso en metros
#run200m - 200 metros lisos en segundos
#longjump - Salto de longitud en metros
#javelin - Lanzamiento de jabalina en metros
#run800m - 800 metros lisos en segundos
str(hep)

#Calculamos los componentes principales de los distintos deportes, obteniendo asi 7 PC 
pca_hep=prcomp(hep[,-1],scale=T)
pca_hep$sdev^2
#Notemos que tenemos 3 eigenvalores mayores a 1


# Varianza de los componentes/Eigenvalores
x=1:7
y=pca_hep$sdev^2
exp_var=data.frame("PC"=x,"Var"=y)

#Regla del codo
p=ggplot(data=exp_var,aes(x=PC,y=Var))+
  geom_col(fill="orange",alpha=.5)+ 
  theme_light()
p
#Vemos que podemos seleccionar hasta la tercer componente, tomando como decision
# despreciar las componentes con varianza menor a 1, por lo que los primeros 3
#tendran mayor peso.

#Interpretando las vectores de cargas, podemos darle una interpretacion a las dos primeras componentes
cargas=pca_hep$rotation
cargas

# Biplot para las dos primeras componentes
fviz_pca_biplot(pca_hep,title="",
                ggtheme = theme_minimal(),geom="point",addEllipses = TRUE, ellipse.level=0.95)
#=Primer componente: Notando que unicamente los saltos de longitud y de altura,
# para los cuales no hay intervalo de tiempo definido tienen una carga positiva,
# podemos interpretarlo como una comparativa de que tanto se desgasta un deportista conforme pasa el tiempo

#-Segunda componente: Unicamente los 100 en vallas tienen una carga negativa, teniendo
# mas importancia el lanzamiento de peso, por lo que podemos inferir es una comparativa entre
# la fuerza y la velocidad de un atleta.

