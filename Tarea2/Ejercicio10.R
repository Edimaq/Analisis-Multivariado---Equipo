#Librerias usadas
library(tidyverse)
library(readxl)
library(GGally)
library(ggplot2)
library(MVN)#Nomalidad Multi

#Ejercicio 10 - Tarea 2

#Equipo 6:
#-
#-
#-


#Cargamos el archivo de 'wine.txt' de nuestro directorio y lo cargamos en un data frame
file<-"C:\\Users\\edima\\Downloads\\wine.txt" #Cambiarlo para cada equipo
datos<-read.delim(file,header = TRUE,sep = ",", dec = ".")
datos$Class<-factor(datos$Class)

#-----------------------------------------------------------------------
#Probar la hipotesis nula de que el vino promedio difiera de 13.15 grados de alcohol y
#2.5 unidades de acido malico.

#Visualizamos os niveles de alcohol y de acido malico de todos los vinedos
vinedos<-datos
variables<-colnames(vinedos)
variables_1<-variables[c(2,3)]
ggpairs(vinedo_1,columns=variables_1,aes(color=Class,alpha=0.5),
           upper= list(continuous= wrap("cor",size=2.5)))


# Asumimos que las variables alcohol y malic acid siguen
#una distribucion normal multivariada (bivariada).
X_1<-datos$Alcohol
X_2<-datos$Malic_acid
n<-length(X_2)
p<-2
X<-cbind(X_1,X_2)

g=ggplot(X,aes(x=X_1,y=X_2))+geom_point()+
  geom_point(aes(x=13.15,y=2.5),col="red",shape=3)+labs(x="Alcohol",y="Acido malico")+
  theme_light()
g

#Calculamos los estadisticos necesarios 
X_bar<-rbind(mean(X_1),mean(X_2))
H=diag(1,nrow=n)-matrix(1/n,nrow=n,ncol=n)
S=t(X)%*%H%*%X/(n-1)
mu_0A<-13.15
mu_0AM<-2.5
mu_0<-rbind(mu_0A,mu_0AM)

#Estadistico de prueba y nivel de confianza
g_2<-((n*(n-p))/((n-1)*p))*t(X_bar-mu_0)%*%solve(S)%*%(X_bar-mu_0)
g_2
#Significancia de 0.05
alpha<-0.05

#Usamos el (1-alpha) cuantil de la distribucion F con p y n-p de parametros
F_np<-qf(1-alpha,p,n-p)
tau<-F_np-g_2
tau
#Vemos que tau es negativo, por lo que rechazamos H_0, es decir el vino promedio
# tiene niveles distintos de alcohol y acido malico distintos a 13.15 y 2.5

#Hacemos la prueba con la funcion integrada de R y vemos que p_value<alpha
HotellingsT2(X,mu=c(13.15,2.5))

#Creamos la elipse de la region, viendo que el punto rojo esta fuera de la region 
# con 95% de significancia
ConfReg=data.frame(ellipse(c(mean(X_1),mean(X_2)), shape=S, 
                           radius=sqrt(((p*(n-1))/((n-p)*n))*F_np), 
                           col="red",add=F,draw=F, lty=2))

g+geom_point(data=ConfReg,aes(x=x,y=y),col="blue",size=.1)
#Por lo que concluimos que el vino promedio difiere de 13.15 grados de alcohol y
#2.5 unidades de acido malico con 95% de confianza.
 
#------------------------------------------------------------------------------
# Realizar los contrastes de hipotesis necesarios para verificar si existe o no una diferencia
#para los niveles de alcohol y acido malico para las clases 1 y 2 de vinos.
vinedo_1<-subset(datos, datos$Class==1)
vinedo_2<-subset(datos, datos$Class==2)

#Graficamos para cada clase de vino sus niveles de alcohol y de acido malico
variables_1<-colnames(vinedo_1)
variables_1<-variables[c(2,3)]
ggpairs(vinedo_1,columns=variables_1,aes(color=Class,alpha=0.5),
        upper= list(continuous= wrap("cor",size=2.5)))

variables_2<-colnames(vinedo_2)
variables_2<-variables[c(2,3)]
ggpairs(vinedo_2,columns=variables_2,aes(color=Class,alpha=0.5),
        upper= list(continuous= wrap("cor",size=2.5)))
# Asumimos que las variables alcohol y malic acid siguen
#una distribucion normal multivariada (bivariada), considerando a la clase 1 y 2 como diferentes.
p<-2
#Para la clase 1
X_1<-vinedo_1$Alcohol
X_2<-vinedo_1$Malic_acid
n<-length(X_2)
X<-cbind(X_1,X_2)

#Para la clase 2
Y_1<-vinedo_2$Alcohol
Y_2<-vinedo_2$Malic_acid
m<-length(Y_2)
Y<-cbind(Y_1,Y_2)

#Calculamos los estadisticos necesarios 
#Para la clase 1
X_bar<-rbind(mean(X_1),mean(X_2))
H_1=diag(1,nrow=n)-matrix(1/n,nrow=n,ncol=n)
S_1=t(X)%*%H_1%*%X/(n-1)


#Para la clase 2
Y_bar<-rbind(mean(Y_1),mean(Y_2))
H_2=diag(1,nrow=m)-matrix(1/m,nrow=m,ncol=m)
S_2=t(Y)%*%H_2%*%Y/(m-1)

#En este caso como m no es igual a n, queremos saber si los niveles son distintos
# es decir  H_0:mu_1=mu_2 vs H_0:mu_1\=mu_2, pero para ello tenemos dos casos:
#Caso 1: Sigma_1=Sigma_2=Sigma desconocido
#Calculamos los estadisticos para la prueba
S_u<-((n-1)*S_1 + (m-1)*S_2)/(n + m - 2)
d_2<-(n*m/((n+m)))*t(Y_bar-X_bar)%*%solve(S_u)%*%(Y_bar-X_bar)

#Significancia del 0.05
alpha<-0.05
#Cuantil de F
F_nmp<-qf(1-alpha,p,n+m-p-1)
tau<-F_nmp-d_2
tau
#Tau es negativo, por lo que  rechazamos H_0

#Usando la funcion de R, obtenemos el mismo estadistico
HotellingsT2(X,Y,test="chi")
#Como p_value<alpha, rechazamos H_0
#Por lo que bajo la suposicion de que tienen misma varianza,
#existe suficiente evidencia para concluir que el promedio de 
#alcohol y acido malico difiere entre clases de vinos (vinedos).


#Caso 2: Sigma_1\=Sigma_2
#Calculamos los estadisticos para la prueba, suponiendo que m y n son grandes
S_u<-S_1/n + S_2/m

dT<-t(Y_bar-X_bar)%*%solve(S_u)%*%(Y_bar-X_bar)
dT
#Significancia del 0.05
alpha<-0.05
#Cuantil de Chi
Chi_p<-qchisq(1-alpha,p)
Chi_p
tau<-Chi_p-dT
tau
#Tau es negativo, por lo que  rechazamos H_0

#Por lo que bajo la suposicion de que no tienen misma varianza,
#existe suficiente evidencia para concluir que el promedio de 
#alcohol y acido malico difiere entre clases de vinos (vinedos).

#Por lo que con 95% de confianza podemos concluir que el 
#el promedio de alcohol y acido malico difiere entre clases de vinos (vinedos).


