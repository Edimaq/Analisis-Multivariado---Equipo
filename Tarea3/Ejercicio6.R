#Librerias usadas
library(ggplot2)
library(GGally)
library(ggthemes)
library(mvtnorm)
library(car)
library(factoextra)
library(here)
library(ggfortify)

#Ejercicio 6 - Tarea 3

#Equipo 6:
#-Reyes Maqueda Edgar
#-Reynoso Torres Ari Genaro
#-Villarreal Maldonado Andre

#Cargamos el archivo de 'EPL_19_20.txt' de nuestro directorio y lo cargamos en un data frame
file<-"C:\\Users\\edima\\Downloads\\EPL_19_20.txt" #Cambiarlo para cada equipo
lp<-read.delim(file,header = TRUE,sep = ",")
#Contiene nombre de equipo (Teams), Victorias (W), Empates (D), Derrotas (L),
#Goles a favor (GF), Goles en contra (GA) y Diferencia de Goles (GD) 
str(lp)

#Es importante notar que como GD=GF-GA, existe una correlacion inmediata entre
#las ultimas tres variables

#Calculamos los componentes principales de los valores, omitiendo el equipo
pca_lp=prcomp(lp[,-1],scale=T)
pca_lp$sdev^2

#Como la variable de diferencia de goles se relaciona con los a favor y en contra
#podemos ver que el sexto y quinto eigenvalor son practicamente 0, lo que nos
#dice que practicamente no hay correlacion con las variables

# Varianza de los componentes
x=1:6
y=pca_lp$sdev^2
exp_var=data.frame("PC"=x,"Var"=y)

#Regla del codo
p=ggplot(data=exp_var,aes(x=PC,y=Var))+
  geom_col(fill="blue",alpha=.5)+ 
  theme_light()
p
#Vemos que el "codo" esta en la segunda componente, pues si consideramos las 
#componentes con varianza menor a 1, solo consideraremos como de mayor importancia 
# a las primeras dos, teniendo mas importancia la primer componente.

#Interpretando las vectores de cargas, podemos darle una interpretacion a las componentes
cargas=pca_lp$rotation
cargas

# Biplot
fviz_pca_biplot(pca_lp,title="",
                ggtheme = theme_minimal(),geom="point",,addEllipses = TRUE, ellipse.level=0.95)
#-Para la 1er componente:Podemos ver que hay una carga negativa para 
# las victorias, los goles a favor y la diferencia, podemos interpretarlo como 
# un promedio de malos desempenos de equipo.

#-Para la 2da componente: Notando que los empates y la diferencia de goles (siendo muy pequena) tienen 
# una carga negativa (los demas positivas) podemos considerarla como un indicador de 
# que tanto se mantiene un equipo en la media.
