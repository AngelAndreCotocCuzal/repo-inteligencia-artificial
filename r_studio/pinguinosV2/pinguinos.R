# Carga de paquetes
library(tidyverse)
library(caret)
library(neuralnet)
library(palmerpenguins)

# Cargar los datos
datos = penguins

# Filtrar filas con valores faltantes
datos_filtrados = datos %>%
  filter(!is.na(bill_length_mm) & !is.na(bill_depth_mm) & !is.na(flipper_length_mm) & !is.na(body_mass_g))

# Separar en datos de entrenamiento y prueba
muestra = createDataPartition(datos_filtrados$species, p=0.8, list=F)
train = datos_filtrados[muestra,]
test = datos_filtrados[-muestra,]

# Entrenamiento de la red neuronal
red.neuronal = neuralnet(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = train, hidden = c(2,3))
red.neuronal$act.fct

# Graficación de la red neuronal 
plot(red.neuronal)

# Hacer predicciones
prediccion = predict(red.neuronal, test)
# Decodificar la columna que contiene el valor máximo
decodificarCol = apply(prediccion, 1, which.max)
# Crear una columna nueva con los valores decodificados 
codificado = tibble(decodificarCol)
colnames(codificado) = "decodificarCol"
codificado = mutate(codificado, especie=recode(decodificarCol, "1"="Adelie", "2"="Chinstrap", "3"="Gentoo"))
test$Especie.Pred = codificado$especie

