# Librerías usadas
library(usmap)
library(vegan)
library(xtable)
library(MASS)
library(plot3D)
library(scatterplot3d)
library(ggplot2)
library(ggthemes)
library(geodist)
library(RColorBrewer)
library(here)

#Ejercicio 4 - Tarea 4

#Equipo 6:
#-Reyes Maqueda Edgar
#-Reynoso Torres Ari Genaro
#-Villarreal Maldonado Andre

#Creamos 1000 variables aleatorias uniformes en (0,1)
set.seed(314)
N=1000
u=runif(N)
v=runif(N)

#Contruimos el bowl mediante las transformaciones
x=0.5*sqrt(v)*cos(2*pi*u)
y=0.5*sqrt(v)*sin(2*pi*u)
z=v-0.5

#Graficamos el bowl, tomando como referencia la coordenada z para colorearlo
#Visto desde arriba
scatter3D(x,y,z,phi=90,theta=180,colkey= FALSE,col=jet.col(100), main="Bowl")
#Visto desde un costado
scatter3D(x,y,z,phi=180,theta=180,colkey= FALSE,col=jet.col(100), main="Bowl")

#Obtenemos la matriz de distancias euclideanas con la funcion dist, de tamano NxN
X=cbind(x,y,z)
D=dist(X,method="euclidean",diag=TRUE)
#La convertimos a matriz para poder observarla, notando que D=D^t
as.matrix(D)

# Escalamiento multidimiensional clásico (metrico) con k=2
mod=cmdscale(D,add=F,eig=T,k=2)

#Primeros dos componentes
coords=data.frame(x=mod$points[,1],y=mod$points[,2],z=z)

ggplot(data=coords,aes(x=x,y=y))+
  geom_point(aes(colour=z))+
  scale_colour_gradientn(colours=jet.col(100))+
  theme_minimal()
#El resultado es el esperado, puesto que podemos ver que las distancias se 
#preservan, inclusive al reducir las dimensiones, creando un resultado parecido 
#a ver como se aprecia la grafica de puntos en 3D desde un solo costado.

#Observando otras tecnicas
# ISOMDS (Escalamiento no metrico)
iso=isoMDS(D,k=2,maxit=300)
Z1=data.frame(x=iso$points[,1],y=iso$points[,2],z=z)

ggplot(data=Z1,aes(x=x,y=y))+
  geom_point(aes(colour=z))+
  scale_colour_gradientn(colours=jet.col(100))+
  theme_minimal()
#Notamos que las distancias no se preservan tan bien conforme z toma valores positivos
#por lo que no es tan eficiente el metodo

# Sammon (Mapeo no lineal)
sam=sammon(d=D,k=2,niter=200,tol=1e-10,magic=.3)
Z3=data.frame(x=sam$points[,1],y=sam$points[,2],z=z)

ggplot(data=Z3,aes(x=x,y=y))+
  geom_point(aes(colour=z))+
  scale_colour_gradientn(colours=jet.col(100))+
  theme_minimal()
#Algunas distancias se preservan, sin embargo hay datos fuera de la figura
#parabolica que se forma, por lo que no todos las distancias se conservan

#ISOMAP (mapeo isometrico)
iso2=isomap(D,ndim=2,k=12)
Z2=data.frame(x=iso2$points[,1],y=iso2$points[,2],z=z)

ggplot(data=Z2,aes(x=x,y=y))+
  geom_point(aes(colour=z))+
  scale_colour_gradientn(colours=jet.col(100))+
  theme_minimal()
#Notemos por la coloracion que podemos identificar que las disctancias se preserva
# teniendo un parecido con la grafica de puntos vista desde arriba, teniendo cierta
#deformacion en una elipse en lugar de un circulo.

#Por lo que patra este caso, nuestra tecnica de Escalamiento Multidimensional
#Metrico es la mas acertadas, pues preserva mejor las distancias, notando mas
#parecido con la figura original, sin embargo el mapeo isometrico tambien
#es una buena opcion.
