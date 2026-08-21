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
## Importacion de datos 
#-----------------------------------------------------------------------------------------------------------------
archivo <- "canada_temperaturas_enviar.xlsx"
hojas <- excel_sheets(archivo)
Datos <- lapply(hojas, function(hoja) read_excel(archivo, sheet = hoja))
names(datos_lista) <- hojas
