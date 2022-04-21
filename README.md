# Software_II


Clase  21/04/22 
---
title: "Analisis de los precios de los Carros Toyota"
output:
  html_document:
    df_print: paged
  word_document: default
---



```{r}
library(ggplot2)
ToyotaCorolla <- read.csv("C:/Users/Lenovo/Downloads/PredictingToyotaPricesBlog-master/ToyotaCorolla.csv")
View(ToyotaCorolla)
```


# Analisis exploratorio de la data

El marco de datos de los precios de los carros toyota, contiene diez variables, en su mayoria en escala cuantitativa y una de ella en  escala nominal (factor). La primer varibale se refiere (Price) a los precios de los vehiculos, la segunda (Age) a la edad, la tercera (Km) a los kilometros recorridos, la cuarta (FUelType) al tipo de combustible,............................


```{r}
head(ToyotaCorolla,n=5)
```


```{r}
ggplot(ToyotaCorolla, aes(x = Price, fill = FuelType)) + 
  geom_histogram()
```

