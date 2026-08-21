#Taller REGRESIÓN FUNDAMENTOS DE APRENDIZAJE DE MÁQUINA
## Sergio Amaya
#----------------------------------------------------------------------------------------------------------------- # nolint
## Instalacion e importacion de librerias necesarias
#----------------------------------------------------------------------------------------------------------------- # nolint
install.packages("corrplot")
install.packages("lmtest")
install.packages("MASS")
install.packages("leaps")
install.packages("readxl")
library(corrplot)
library(lmtest)
library(MASS)
library(leaps)
library(readxl)
#-----------------------------------------------------------------------------------------------------------------
## Importacion y preparacion de los datos de datos 
#-----------------------------------------------------------------------------------------------------------------
###importacion del archivo canada_temperaturas_enviar.xlsx

archivo <- "Ejercicios_relizados\\canada_temperaturas_enviar.xlsx"
hojas <- excel_sheets(archivo)
datos_completos <- lapply(hojas, function(hoja) read_excel(archivo, sheet = hoja))
names(datos_completos) <- hojas

###Separacion de las hojas y preparacion de los datos de  base y predecir

datos_entrenamiento <- datos_completos[["base"]]
datos_predecir <- datos_completos[["predecir"]]
datos_entrenamiento <- as.data.frame(datos_entrenamiento)
datos_predecir <- as.data.frame(datos_predecir)
str(datos_entrenamiento)
str(datos_predecir)

### Eliminacion de las variables que no se van a utilizar en el modelo de regresion


datos_entrenamiento <- datos_entrenamiento[, !(names(datos_entrenamiento) %in% c("State", "City"))]
datos_predecir <- datos_predecir[, !(names(datos_predecir) %in% c("State", "City", "id"))]

str(datos_entrenamiento)
str(datos_predecir)

###division de los datos en conjunto de entrenamiento y conjunto de prueba

sample <- sample.int(nrow(datos_entrenamiento), floor(.8*nrow(datos_entrenamiento)))
datos_entrenamiento.train <- datos_entrenamiento[sample, ]
datos_entrenamiento.test <- datos_entrenamiento[-sample, ]

#-----------------------------------------------------------------------------------------------------------------
# Entendimiento de los datos
#-----------------------------------------------------------------------------------------------------------------
###Analisis de correlacion entre las variables del conjunto de entrenamiento
matrizcor<-cor(datos_entrenamiento.train)
corrplot(matrizcor)
#-----------------------------------------------------------------------------------------------------------------
##modelado y valoracion del modelo
#-----------------------------------------------------------------------------------------------------------------
### modelado de regresion lineal multiple

modelo <- lm(Feelslike_C ~ ., data = datos_entrenamiento.train)

### Valoracion del modelo 
#### metricas
summary(modelo)

####Intervalos de confianza
confint(modelo, level=0.95)

#### graficos
layout(matrix(c(1,2,3,4),2,2))
plot(modelo)

#### Autocorrelacion de los residuos
dwtest(modelo)

#-----------------------------------------------------------------------------------------------------------------
##feauture selection y overfitting
#-----------------------------------------------------------------------------------------------------------------
### AIC
AIC<-AIC(modelo)
AIC

### predicciones

pred<-predict(modelo, datos_entrenamiento.test, se.fit=TRUE)
RMSE<-sqrt(mean((pred$fit-datos_entrenamiento.test$Feelslike_C)^2))
RMSE
