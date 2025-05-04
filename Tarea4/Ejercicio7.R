# Librerías usadas
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(expm)
library(ca)
library(here)
library(dplyr)
library(readxl)

library(FactoMineR)
library(ggplot2)
library(FactoClass)
library(factoextra)
library(Rcpp)
library(broom)
library(pander)
library(corrplot)
library(gridExtra)
library(tidyverse)
library(caret)#Paquete para crear la matriz indicadora
library(data.table)
library(reshape2)

#Ejercicio 7 - Tarea 4

#Equipo 6:
#-Reyes Maqueda Edgar
#-Reynoso Torres Ari Genaro
#-Villarreal Maldonado Andre

#Cargamos el archivo de 'survival.txt' de nuestro directorio y lo cargamos en un data frame
file<-"C:\\Users\\edima\\Downloads\\survival.txt" #Cambiarlo para cada equipo
datos<-read.delim(file,header = TRUE,sep = " ", dec = ".")
datos=data_frame(datos)
#Hay 3 variables categoricas para cada individuo, las cuales 
#-Survival: Si el individuo sobrevivio Yes o No
#-Age: las edades de los individuos por bloques <50,50-69,>69
#-City: ciudad de procedencia del individuo las cuales son Boston, Glamorgan o Tokyo
head(datos)

#Obtenemos la matriz indicadora usando one-hot-encoding por medio de la creacion de dummy variables 
#-Survival: Yes (10), No (01)
#-Age:  <50 (100),50-69 (010),>69 (001)
#-City: Boston (100), Glamorgan (010) o Tokyo (001
dummy <- dummyVars(" ~ .", data=datos)
X <- data.matrix(predict(dummy, newdata = datos)) 
head(X)

#Obtenemos la matriz de Burt por medio de la matriz indicadora
B=t(X)%*%X
as.table(B)

#Vamos a hacer Analisis de correspondencia multiple sobre la matriz indicadora
# Total de observaciones
n=sum(X)
#Sumas por renglones y columnas
row_sum=apply(X,1,sum)
col_sum=apply(X,2,sum)

#Masas y centroide
row_masses=row_sum/n
centroide=col_sum/n

#Matriz diagonales de masas y centroide
Dr=diag(row_masses)
Dc=diag(centroide)

#Raiz cuadrada de matrices diagonales
Dr_sq=solve(sqrtm(Dr))
Dc_sq=solve(sqrtm(Dc))

#Transformamos a matrices las masas y centroides
row_m=as.matrix(row_masses)
centroide_m=as.matrix(centroide)

#Matriz de correspondencias
P=X/n

#Matriz estandarizada y centrada
A=Dr_sq%*%(P-(row_m%*%t(centroide_m)))%*%Dc_sq

#svd de A
res=svd(A)

## Valores singulares y vectores
values=res$d
U=res$u
V=res$v

## Coordenadas estándar
X=Dr_sq%*%U
Y=Dc_sq%*%V

## Coordenadas principales
f=X%*%diag(res$d)
g=Y%*%diag(res$d)

## Graficamos modificando las coordenadas para que sea visible
set.seed(31415)
df=data.frame(x=f[,1],y=f[,2]+rnorm(6,0,.5))
df_dual=data.frame(x=g[,1],y=g[,2]+rnorm(6,0,.5))

p=ggplot(df_dual,aes(x=x,y=y))+
  geom_text(size=3.5,show.legend=F,data=df_dual,
            aes(label = c("Survival-Y",
                          "Survival-N",
                          "Age-<50",
                          "Age-50-69",
                          "Age->69",
                          "City-Boston",
                          "City-Glamorgan",
                          "City-Tokyo")))+
  theme_minimal()+
  labs(x="",y="")
p

p=p+geom_text(size=3.5,show.legend=F,data=df,col="blue",
              aes(label = 1:764))+
  theme_minimal()+
  labs(x="",y="")
p


# Inercias sin modificar
inercias=res$d^2;inercias
#Solo tenemos que los primeros 5 eigenvalores se toman en cuenta 

#Inercia total
inercia = sum(inercias);inercia
#Se tiene una inercia del 1.66

# Contribución a la inercia
contribucion=inercias/inercia;contribucion
#Las primeras 2 dimensiones tienen una contribucion mayor al 20%, haciendo casi el 50%
# Inercia acumulada
cumsum(contribucion)
#Observando la grafica, notando que los individuos estan en azul y las 
#categorias en negro, podemos notar que la supervivencia de una
#persona esta mas relacionado con el hecho de que viva en Tokyo y sea menor de 50,
#mientras que un individuo entre 50 y 69 que vive en Boston tiene mas posibilidades
# de que no sobreviva, notando que el hecho de que viva en Glamorgan o tenga mas de 69
# no nos dan informacion suficiente de si sobrevive o no.


