# Ejercicio tecnicas de aprendizaje de maquina
## importar librerias
install.packages("readxl")

library(readxl)

## importar datos
datos <- read_excel("Espacios_métricos_métricas_distancia/Encuesta de características personales.csv")
datos <- as.data.frame(datos)
colnames(datos) #ver nombres de las columnas
