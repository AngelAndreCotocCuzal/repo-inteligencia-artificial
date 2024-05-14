# Paquetes que requieren ser instalados 
# install.packages(c("tidyverse", "caret", "neuralnet"))
# Carga de paquetes
library(tidyverse)
library(caret)
library(neuralnet)

datos = pin
muestra = createDataPartition(datos$,p=0.8, list=F)
train = datos[muestra,]
test = datos[-muestra,]
# Entrenamiento de la red neuronal
red.neuronal = neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = train, hidden = c(2,3))
red.neuronal$act.fct
# Graficacion de la red neuronal 
plot(red.neuronal)

prediccion = predict(red.neuronal, test, type="class")
# Decodificar la columna que contene el valor maximo
# y por ende la especie de la que se trata decodificarCol
decodificarCol = apply(prediccion, 1, which.max)
# Crear una columna nueva con los valores decodificados 
codificado = data_frame(decodificarCol)
codificado = mutate(codificado, especie=recode(decodificarCol, "1"="Adelie", "2"="Chinstrap", "3"="Gentoo"))
test$Especie.Pred = codificado$especie 


