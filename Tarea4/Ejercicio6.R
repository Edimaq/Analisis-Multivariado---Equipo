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

#Realizando un analisis de correspondencias a la base de datos con los siguientes
#paises sobre considerando que una mujer con hijos pueda o no tener un trabajo completo\
#quedarse en casa y/0 no sabe
#AUS - Australia
#DW - Alemania del Oeste
#DE - Alemania del Este
#GB - Reino Unido
#NIRL - Irlanda del Norte
#USA - Estados Unidos de America
#A - Austria
#H - Hungría
#I - Italia
#IRL - Irlanda
#NL - Holanda
#N - Noruega
#S - Suecia
#CZ - Checoslovaquia (cuando aún existía)
#SLO - Eslovenia 
#PL - Polonia
#BG - Bulgaria
#RUS - Rusia
#NZ - Nueva Zelanda
#CDN - Canadá
#RP - Filipinas 
#IL - Israel
#J - Japón
#E - España

file<-"C:\\Users\\edima\\Downloads\\women.xls" #Cambiarlo para cada equipo
women = read_xls(file)
labels = as.matrix(women[,1])
labels = c("Australia","Alemania del Oeste","Alemania del Este","Reino Unido",
           "Irlanda del Norte","EUA","Austria","Hungria","Italia","Irlanda",
           "Holanda","Noruega","Suecia","Checoslovaquia","Eslovenia","Polonia",
           "Bulgaria","Rusia","Nueva Zelanda","Canada","Filipinas","Israel","Japon","España")
women = women%>%
  select(-...1)
women=as.matrix(women)
n=sum(women);n

## Suma por renglones y columnas
row_sum=apply(women,1,sum);row_sum
col_sum=apply(women,2,sum);col_sum

## Masas y centroide
row_masses=row_sum/n;row_masses
centroide=col_sum/n;centroide

## Matrices diagonales de masas y centroide
Dr=diag(row_masses);Dr
Dc=diag(1/centroide);Dc

## Raiz cuadrada de matrices diagonales
Dr_sq=sqrtm(Dr);Dr_sq
Dc_sq=sqrtm(Dc);Dc_sq

## Perfiles por renglón
profiles=matrix(0,nrow=24,ncol=4)
for(i in 1:24){
  profiles[i,]=women[i,]/row_sum[i]
}

profiles

## Matriz quitando el centroide
R=matrix(0,nrow=24,ncol=4)
for(i in 1:24){
  R[i,]=profiles[i,]-centroide
}
R

## Encontramos la descomposición gsvd 
res=svd(Dr_sq%*%R%*%Dc_sq)

## Valores propios
values=res$d;values

## Matrices
U=res$u;U
V=res$v;V

N=solve(Dr_sq)%*%U;N
M=solve(Dc_sq)%*%V;M

## Coordenadas de los renglones
f=N[,c(1:2)]%*%diag(values[c(1:2)]);f

df_points=data.frame(x=f[,1],y=f[,2])
df_labels=data.frame(x=f[,1],y=f[,2])

ggplot(data=df_points,aes(x=x,y=y))+
  geom_text(show.legend=F,size=3,
            aes(label = labels))+
  theme_minimal()+
  labs(x="",y="")+
  ylim(c(-.4,.4))
#Problema dual 
women_d=t(women)

## Suma por renglones y columnas
row_sum_d=apply(women_d,1,sum)
col_sum_d=apply(women_d,2,sum)

## Masas y centroide
row_masses_d=row_sum_d/n
centroide_d=col_sum_d/n

## Matrices diagonales de masas y centroide
Dr_d=diag(row_masses_d)
Dc_d=diag(1/centroide_d)

## Raiz cuadrada de matrices diagonales
Dr_sq_d=sqrtm(Dr_d)
Dc_sq_d=sqrtm(Dc_d)

## Perfiles por renglón
profiles_d=matrix(0,nrow=4,ncol=24)
for(i in 1:4){
  profiles_d[i,]=women_d[i,]/row_sum_d[i]
}

## Matriz quitando el centroide
R_d=matrix(0,nrow=4,ncol=24)
for(i in 1:4){
  R_d[i,]=profiles_d[i,]-centroide_d
}

## Encontramos la descomposición gsvd 
res_d=svd(Dr_sq_d%*%R_d%*%Dc_sq_d)

## Valores propios
values_d=res_d$d

## Matrices
U_d=res_d$u
V_d=res_d$v

N_d=solve(Dr_sq_d)%*%U_d
M_d=solve(Dc_sq_d)%*%V_d

## Coordenadas de los renglones
g=N_d[,c(1:2)]%*%diag(values_d[c(1:2)])

df_points_dual=data.frame(x=g[,1],y=g[,2])
df_labels_dual=data.frame(x=g[,1],y=g[,2])

ggplot(data=df_points_dual,aes(x=x,y=y))+
  geom_text(show.legend=F,col="red",size=3,
            aes(label = c("Completo","Parcial","Casa",
                          "Sin saber")))+
  theme_minimal()+
  labs(x="",y="")+
  ylim(c(-.4,.4))

# Se grafican las dos
ggplot(data=df_labels,aes(x=x,y=y))+
  geom_text(show.legend = F,size=3,aes(label = labels))+
  geom_text(show.legend=F,data=df_labels_dual,col='red',size=3,
            aes(label = c("Completo","Parcial","Casa",
                          "Sin saber")))+
  theme_minimal()+
  labs(x="",y="")+
  ylim(c(-.4,.4))


df_labels_dual=data.frame(x=-g[,1],y=g[,2])
#Notemos que haciendo un primer analisis, paises como EUA o Israel estan
#ms de acuerdo con tener un trabajo completo, mientras que paises europeos y de Oceania
#estan mas de acuerdo con tener uno parcial, por otro lado paises como Filipinas 
#consideran que es mejor quedarse en casa. Casos como Espana o Eslovenia estan entre 
#un trabajo completo o no se sabe su opinion.

# Se grafican las dos
ggplot(data=df_labels,aes(x=x,y=y))+
  geom_text(show.legend = F,size=3,aes(label = labels))+
  geom_text(show.legend=F,data=df_labels_dual,col='red',size=3,
            aes(label = c("Completo","Parcial","Casa",
                          "Sin saber")))+
  theme_minimal()+
  labs(x="",y="")+
  ylim(c(-.4,.4))
#Haciendo el analisis dual, nos damos cuenta que el primer analisis es parecido, 
#puesto que los paises europeos y de oceania estan de acuerdo con la idea de un 
#trabajo parcial, siendo mas tradicional Filipinas, mientras que EUA e Israel comparten ideas
#similares, al igual que Eslovenia y Espana.


##############################################################################
#Haciendo el analisis, diferenciando entre hombres y mujeres
file<-"C:\\Users\\edima\\Downloads\\women2.xls" #Cambiarlo para cada equipo
women2 = read_xls(file)
labels2 = as.matrix(women2[,1])
labels2 = c("Australia(H)","Alemania del Oeste(H)","Alemania del Este(H)","Reino Unido(H)",
           "Irlanda del Norte(H)","EUA(H)","Austria(H)","Hungria(H)","Italia(H)","Irlanda(H)",
           "Holanda(H)","Noruega(H)","Suecia(H)","Checoslovaquia(H)","Eslovenia(H)","Polonia(H)",
           "Bulgaria(H)","Rusia(H)","Nueva Zelanda(H)","Canada(H)","Filipinas(H)","Israel(H)","Japon(H)","España(H)",
           "Australia(M)","Alemania del Oeste(M)","Alemania del Este(M)","Reino Unido(M)",
           "Irlanda del Norte(M)","EUA(M)","Austria(M)","Hungria(M)","Italia(M)","Irlanda(M)",
           "Holanda(M)","Noruega(M)","Suecia(M)","Checoslovaquia(M)","Eslovenia(M)","Polonia(M)",
           "Bulgaria(M)","Rusia(M)","Nueva Zelanda(M)","Canada(M)","Filipinas(M)","Israel(M)","Japon(M)","España(M)")
women2 = women2%>%
  select(-...1)
women2=as.matrix(women2)
n=sum(women2);n

## Suma por renglones y columnas
row_sum=apply(women2,1,sum);row_sum
col_sum=apply(women2,2,sum);col_sum

## Masas y centroide
row_masses=row_sum/n;row_masses
centroide=col_sum/n;centroide

## Matrices diagonales de masas y centroide
Dr=diag(row_masses);Dr
Dc=diag(1/centroide);Dc

## Raiz cuadrada de matrices diagonales
Dr_sq=sqrtm(Dr);Dr_sq
Dc_sq=sqrtm(Dc);Dc_sq

## Perfiles por renglón
profiles=matrix(0,nrow=48,ncol=4)
for(i in 1:48){
  profiles[i,]=women2[i,]/row_sum[i]
}

profiles

## Matriz quitando el centroide
R=matrix(0,nrow=48,ncol=4)
for(i in 1:48){
  R[i,]=profiles[i,]-centroide
}
R


## Encontramos la descomposición gsvd 
res=svd(Dr_sq%*%R%*%Dc_sq)
res

## Valores propios
values=res$d;values

## Matrices
U=res$u;U
V=res$v;V

N=solve(Dr_sq)%*%U;N
M=solve(Dc_sq)%*%V;M

## Coordenadas de los renglones
f=N[,c(1:2)]%*%diag(values[c(1:2)]);f


df_points=data.frame(x=f[,1],y=f[,2])

df_labels=data.frame(x=f[,1],y=f[,2])

ggplot(data=df_points,aes(x=x,y=y))+
  geom_text(show.legend=F,size=3,
            aes(label = labels2),)+
  theme_minimal()+
  labs(x="",y="")+
  ylim(c(-.6,.6))
# Problema dual
women_d=t(women2)

## Suma por renglones y columnas
row_sum_d=apply(women_d,1,sum)
col_sum_d=apply(women_d,2,sum)

## Masas y centroide
row_masses_d=row_sum_d/n
centroide_d=col_sum_d/n

## Matrices diagonales de masas y centroide
Dr_d=diag(row_masses_d)
Dc_d=diag(1/centroide_d)

## Raiz cuadrada de matrices diagonales
Dr_sq_d=sqrtm(Dr_d)
Dc_sq_d=sqrtm(Dc_d)

## Perfiles por renglón
profiles_d=matrix(0,nrow=4,ncol=48)
for(i in 1:4){
  profiles_d[i,]=women_d[i,]/row_sum_d[i]
}

## Matriz quitando el centroide
R_d=matrix(0,nrow=4,ncol=48)
for(i in 1:4){
  R_d[i,]=profiles_d[i,]-centroide_d
}

## Encontramos la descomposición gsvd 
res_d=svd(Dr_sq_d%*%R_d%*%Dc_sq_d)

## Valores propios
values_d=res_d$d

## Matrices
U_d=res_d$u
V_d=res_d$v

N_d=solve(Dr_sq_d)%*%U_d
M_d=solve(Dc_sq_d)%*%V_d

## Coordenadas de los renglones
g=N_d[,c(1:2)]%*%diag(values_d[c(1:2)])

df_points_dual=data.frame(x=g[,1],y=g[,2])
df_labels_dual=data.frame(x=g[,1],y=g[,2])

ggplot(data=df_points_dual,aes(x=x,y=y))+
  geom_text(show.legend=F,col="red",size=3,
            aes(label = c("Completo","Parcial","Casa",
                          "Sin saber")))+
  theme_minimal()+
  labs(x="",y="")+
  ylim(c(-.6,.6))

# Se grafican las dos
ggplot(data=df_labels,aes(x=x,y=y))+
  geom_text(show.legend = F,size=3,aes(label = labels2))+
  geom_text(show.legend=F,data=df_labels_dual,col='red',size=3,
            aes(label = c("Completo","Parcial","Casa",
                          "Sin saber")))+
  theme_minimal()+
  labs(x="",y="")+
  ylim(c(-.6,.6))


df_labels_dual=data.frame(x=-g[,1],y=g[,2])
#Notemos que haciendo un primer analisis, que la mayoria de paises tiende a estar 
#acuerdo con el genero opuesto , teniendo un enfoque casi parecido al analisis sin genero
#a excepcion paises como Rusia, Filipinas, Irlanda y Suecia, donde la diferencia es un poco mas notoria
#Notanto que en EUA las mujeres prefieren un trabajo completo y los hombres tienden a
#preferiri que se queden en casa, mientra sque en Israel pasa lo contrario.,
# Se grafican las dos
ggplot(data=df_labels,aes(x=x,y=y))+
  geom_text(show.legend = F,size=3,aes(label = labels2))+
  geom_text(show.legend=F,data=df_labels_dual,col='red',size=3,
            aes(label = c("Completo","Parcial","Casa",
                          "Sin saber")))+
  theme_minimal()+
  labs(x="",y="")+
  ylim(c(-.6,.6))
#Con el analisis Dual, podemos darnos cuenta que no hay demasiada diferencia
