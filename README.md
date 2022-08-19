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

1. Lectura sobre el genesis de la regresión lineal

https://github.com/Hen1985/Software_III/blob/main/galton-1886-jaigi-regression-stature.pdf

2. Introducción al análisis de regresión lineal

https://github.com/Hen1985/Software_III/blob/main/Regresi%C3%B3n%20Lineal.pdf

Desarrollo de la clase 

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
