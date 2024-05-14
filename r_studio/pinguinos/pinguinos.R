# Carga de paquetes
library(tidyverse)
library(neuralnet)
library(palmerpenguins)

# Cargar los datos
datos <- penguins

# Filtrar filas con valores faltantes
datos_filtrados <- datos %>%
  drop_na(c(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g))

# Guardar las especies en una variable separada
especies <- datos_filtrados$species

# Normalizar y reescalar solo las variables numéricas
datos_numericos <- datos_filtrados %>%
  mutate_at(vars(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g), scale)

# Combinar datos numéricos con especies
datos_preprocesados <- cbind(datos_numericos, species = especies)

# Separar en datos de entrenamiento y prueba
set.seed(123) # Fijar semilla para reproducibilidad
muestra <- createDataPartition(datos_preprocesados$species, p = 0.8, list = FALSE)
train <- datos_preprocesados[muestra, ]
test <- datos_preprocesados[-muestra, ]

# Entrenamiento de la red neuronal con datos preprocesados
red.neuronal <- neuralnet(species ~ ., 
                          data = train[, -5], 
                          hidden = c(5, 3))  # Número de neuronas en las capas ocultas

# Hacer predicciones en datos de prueba
prediccion <- predict(red.neuronal, test[, -5])

# Decodificar las predicciones
especies_predichas <- colnames(prediccion)[apply(prediccion, 1, which.max)]
test$Especie.Pred <- especies_predichas

# Calcular porcentaje de error
error <- mean(test$species != test$Especie.Pred)
error_porcentaje <- error * 100

print(paste("Porcentaje de error:", error_porcentaje, "%"))



