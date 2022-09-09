# Software_III

La asignatura Software Estadístico III pertenece al área de formación profesionalizante,
porque está relacionada con el desempeño laboral del futuro profesional en el campo de 
la Estadística. Software Educativo III se impartirá en el segundo semestre de cuarto año de la carrera 
Ingeniería Estadística, es la continuidad en el manejo de bases y de datos obtención de 
resultados de pruebas estadísticas. Tiene como asignaturas precedente Software
Estadístico II, Inferencia Estadística II y Modelos Lineales I, además sienta las bases 
para cursos posteriores tales como Técnicas de Muestreo II y Análisis Multivariado II, 
teniendo como premisa la obtención de información que apoye la toma de decisión, 
utilizando como herramientas el software especializado de estadística, así como hojas 
de cálculo.

Los contenidos a desarrollar en esta asignatura permitirán al futuro profesional el manejo 
de grandes volúmenes de datos, además el conocimiento de aspectos importantes del
uso de software en el análisis de datos estadísticos que apoyan y agilizan la toma de 
decisiones oportunas, así como el desarrollo de destrezas y habilidades en el manejo de 
los diferentes comandos/opciones para el manejo eficiente de bases de datos y la prueba
de significancia de una y dos variables.


Clase número 1[11/08/2022]

1. Lectura sobre el génesis de la regresión lineal

https://github.com/Hen1985/Software_III/blob/main/galton-1886-jaigi-regression-stature.pdf

2. Introducción al análisis de regresión lineal

https://github.com/Hen1985/Software_III/blob/main/Regresi%C3%B3n%20Lineal.pdf

Clase número 4 [19/08/22]



################################################################
###                Clase 2. Modelos lineales
################################################################
# Cargar el marco de datos iris 

data("iris")
dat <- iris
View(dat)

# Modelaje 

cor(dat[,c(1:4)])

###############################################################
### Visualización del de la relación lineal 
###############################################################

plot(dat$Sepal.Length, dat$Petal.Width)

###############################################################
### Modelar  
###############################################################

modelo1 <- lm(dat$Sepal.Length~dat$Petal.Width)

summary(modelo1)

###############################################################
### Validación visual  
###############################################################

plot(modelo1)

###############################################################
### Validación Analítica  
###############################################################
###############################################################
### Crierio 1. Autocorrelacion 
###############################################################

library(car)
library(carData)

#Hipótesis nula: No autocorrelación vs autocorrelación

durbinWatsonTest(modelo1, simulate = T, reps = 1000)

###############################################################
### Crierio 4. Heteroscedasticidad
###############################################################

Sepal.Length <- dat[,c(1)]

Petal.Width<- dat[,c(3)]

datos <- c(Sepal.Length, Petal.Width)

tratamiento <- rep(c("Sepal.Length", "Petal.Width"), each = 150)

tratamiento

bartlett.test(datos, tratamiento, center = mean)# median o mean

leveneTest(datos, tratamiento, center = mean) # car, requiere car data

###############################################################
### Nota: La prueba es aplicada cuando se ha mostrado normalidad, 
### de lo contrario usar leveneTest es para pruebas no paramétricas
###############################################################

###############################################################
### Crierio 5. Distribución normal de los términos de error
###############################################################




Clase número [090922]

1. Lectura sobre el génesis 

https://www.nature.com/articles/s41437-020-00394-6


2. Modelado de l DCA






###############################################################
### Problema 1.1
###############################################################

## Se desea conocer los efectos del alcohol en la realización de operaciones
## matemáticas sencillas (sumas). Para ello se eligen al azar 20 personas que 
## subduvidimos aletoriamente en cuatro grupo de cinco y a los que se les aplica
## tambien aleatoriamente, cuatro tratamiento distintos: T1: Sin alchol, 
## T2:baja dosis de alchol, T3: media dosis de alchol, T4: alta dosis de alchol
## Los timpos que tardan en realizar las sumas sin error son los siguientes.

t1<-	c(42,39,48,43,44)
t2<- c(45,46, 45,39,43)
t3<- c(64,61,50,55,58)
t4<- c(56,55,62,59,60)

###############################################################
### Inciso 1. Comprobar si los datos de cada tratamiento se distribuyen de manera normal
###############################################################


ks.test(t1, mean(t1), sd(t1))
ks.test(t2, mean(t2), sd(t2))
ks.test(t3, mean(t3), sd(t3))
ks.test(t4, mean(t4), sd(t4))

###############################################################
### Inciso 2. Comprobar si existe Varianza iguales dentro de los tratamientos
###############################################################
 

tiempo<- c(42,39,48,43,44, 45,46, 45,39,43,64,61,50,55,58,56,55,62,59,60)
tratamiento <- c("t1","t1","t1","t1","t1", "t2", "t2","t2","t2","t2","t3",
                 "t3","t3","t3","t3","t4","t4","t4","t4","t4")

dat<- data.frame(tratamiento,tiempo)
dat

bartlett.test(tiempo~tratamiento, data = dat)


###############################################################
### Inciso 3. Construya el modelo ANAVA
###############################################################
 
modelo1 <- aov(tiempo~tratamiento, dat)
modelo1

plot(modelo1)
(result<- summary(modelo1))

###############################################################
### Inciso 4. Aplique de comparaciones múltiples para las medias observadas 
###############################################################

# install.packages("agricolae")
# library(agricolae)

utLSD <-LSD.test(modelo1, "tratamiento", group=FALSE,console=TRUE)



###############################################################
### Inciso 5. Visualizar las comparaciones múltiples para las medias observadas
###############################################################


#library(ggpval)
#library(ggstatsplot)
#library(rstantools)
#library(PMCMRplus)

x1<- ggbetweenstats(
data  = dat,
x     = tratamiento,
y     = tiempo,
plot.type = "box",
xlab="Tratamiento",
ylab="Tiempo en minutos",
palette = "Blues",
centrality.point.args = list(size = 5, color = "#49525E"),
type= "parametric",
title = "Efecto del alcolh",
var.equal = T
)
x1
