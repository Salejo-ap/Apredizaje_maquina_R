# Ejercicio tecnicas de aprendizaje de maquina
## importar librerias
install.packages("readxl")
install.packages("cluster")
install.packages("dplyr")
install.packages("reshape2")
library(readxl)
library(cluster)
library(dplyr)
library(reshape2)

## importar datos csv
path <- "Espacios_métricos_métricas_distancia/Encuesta de características personales.csv" # nolint: line_length_linter.
datos <- read.csv(path, sep = ";", header = TRUE, fileEncoding = "Latin1") # nolint: line_length_linter.
datos <- as.data.frame(datos)
colnames(datos) #ver nombres de las columnas

## seleccionar columnas de interes
# eliminar columnas que no se van a utilizar dado que son metadatos del cuestionario
datos <- datos[, c("Nombre2", "Genero", "X.qué.tanto.le.gusta.la.música.flamenca."
    , "X.qué.tanto.le.gusta.la.música.clásica.", "X.qué.tanto.le.gusta.la.música.pop.", "X.qué.tanto.le.gusta.la.música.reggaeton."
    , "X.qué.tanto.le.gusta.la.música.salsa.", "X.qué.tanto.le.gusta.la.música.blues."               
    , "X.qué.tanto.le.gusta.la.música.rock.", "X.qué.tanto.le.gusta.la.música.merengue.", "X.qué.tanto.le.gusta.la.música.electrónica."         
    , "X.qué.tanto.le.gusta.la.música.tango.", "X.qué.tanto.le.gusta.la.música.jazz.", "X.qué.tanto.le.gusta.la.música.cumbia."              
    , "X.qué.tanto.le.gusta.la.música.andina.", "X.qué.tanto.le.gusta.la.música.rap.", "X.qué.tanto.le.gusta.la.música.norteña."             
    , "X.qué.tanto.le.gusta.la.música.reggae.", "X.qué.tanto.le.gusta.la.música.country.", "Estatura..en.centimetros.", "Número.de.contactos.en.Linkedin"                     
    , "Horas.gastadas.la.última.semana.en.el.teléfono.móvil", "Tiempo.de.transporte.de.la.casa..a.la.Universidad", "Horas.de.sueño.promedio.la.última.semana"            
    , "Número.de.países.visitados", "Medio.de.transporte.a.la.Universidad.más.usado", "Bebida..no.alcoholica..preferida")]

colnames(datos) #ver nombres de las columnas

## estadisticas descriptivas de las variables
# Numero de reguistros nulos por columna
nulos <- sapply(datos, function(x) sum(is.na(x)))
nulos

# llenar nulo con mediana de la columna X.qué.tanto.le.gusta.la.música.electrónica
datos$X.qué.tanto.le.gusta.la.música.electrónica.[is.na(datos$X.qué.tanto.le.gusta.la.música.electrónica.)] <- median(datos$X.qué.tanto.le.gusta.la.música.electrónica., na.rm = TRUE)

# verifcar si ya no hay nulos
nulos <- sapply(datos, function(x) sum(is.na(x)))
nulos

#Resumen estadistico de las variables
summary(datos)
# correción de las variables
datos$Nombre2[1] <- "NA"
datos$Nombre2[13] <- "NA"
datos$Tiempo.de.transporte.de.la.casa..a.la.Universidad[1] <- 78
datos$Tiempo.de.transporte.de.la.casa..a.la.Universidad[5] <- 60
datos$Tiempo.de.transporte.de.la.casa..a.la.Universidad[6] <- 120
datos$Tiempo.de.transporte.de.la.casa..a.la.Universidad[7] <- 60
datos$Tiempo.de.transporte.de.la.casa..a.la.Universidad[11] <- 120
datos$Horas.de.sueño.promedio.la.última.semana[1] <- 6.2
datos$Horas.de.sueño.promedio.la.última.semana[10] <- 6.5
datos$Horas.de.sueño.promedio.la.última.semana[13] <- 5.7
datos

#Dada la variedad de los datos, Varables continuas, categóricas y ordinales, se opta por utilizar la métrica de Gower para calcular la distancia entre los registros. Esta métrica es adecuada para datos mixtos.
# guardar los nombre 
nombres <- datos$Nombre2
datos$Nombre2 <- NULL
#copiar datos para trabajar con ellos
datos_original <- datos
# lista variables categoricas
categoricas <- c("Genero", "Medio.de.transporte.a.la.Universidad.más.usado", "Bebida..no.alcoholica..preferida")
# variables categoricas a factor
datos[categoricas] <- lapply(datos[categoricas], as.factor)
# Columnas 2:18 son gustos musicales (asumiendo que ya vienen como números 1-5)
datos[, 2:18] <- lapply(datos[, 2:18], as.numeric)

# Columnas numéricas: estatura, contactos, horas móvil, transporte, sueño, países
# Ajusta los índices según tu data.frame real
datos[, 19:26] <- lapply(datos[, 19:26], as.numeric)

# Calcular distancia Gower
dist_gower <- daisy(datos, metric = "gower")
matriz_dist <- as.matrix(dist_gower)

# Función para obtener los dos vecinos más cercanos
obtener_vecinos <- function(matriz, nombres) {
  n <- nrow(matriz)
  vecinos <- data.frame(
    Estudiante = nombres,
    Vecino1 = character(n),
    Dist1 = numeric(n),
    Vecino2 = character(n),
    Dist2 = numeric(n),
    stringsAsFactors = FALSE
  )
  for (i in 1:n) {
    distancias <- matriz[i, ]
    distancias[i] <- NA  # excluir a sí mismo
    orden <- order(distancias, na.last = NA)
    vecinos$Vecino1[i] <- nombres[orden[1]]
    vecinos$Dist1[i] <- distancias[orden[1]]
    vecinos$Vecino2[i] <- nombres[orden[2]]
    vecinos$Dist2[i] <- distancias[orden[2]]
  }
  return(vecinos)
}

resultado_gower <- obtener_vecinos(matriz_dist, nombres)
print(resultado_gower)

## metodo de la correlacion
# Matriz de correlación de variables numéricas
datos_cor <- datos_original
datos_num <- datos_cor[, sapply(datos_cor, is.numeric)]
datos_cat <- datos_original[categoricas]
datos_cat
datos_num
cor_mat <- cor(datos_num, use = "pairwise.complete.obs")

# Ver pares con alta correlación
cor_melt <- melt(cor_mat)
cor_melt <- subset(cor_melt, abs(value) > 0.8 & Var1 != Var2)
print(cor_melt)

# Eliminar una variable de cada par redundante
datos_cor$X.qué.tanto.le.gusta.la.música.flamenca. <- NULL
datos_cor$X.qué.tanto.le.gusta.la.música.rock. <- NULL
colnames(datos_cor)
datos_cor[categoricas] <- lapply(datos_cor[categoricas], as.factor)
# Columnas 2:18 son gustos musicales (asumiendo que ya vienen como números 1-5)
datos_cor[, 2:16] <- lapply(datos_cor[, 2:16], as.numeric)

# Columnas numéricas: estatura, contactos, horas móvil, transporte, sueño, países
# Ajusta los índices según tu data.frame real
datos_cor[, 17:20] <- lapply(datos_cor[, 17:22], as.numeric)
# Calcular distancia Gower
dist_gower_cor <- daisy(datos_cor, metric = "gower")
matriz_dist_cor <- as.matrix(dist_gower_cor)

resultado_gower_cor <- obtener_vecinos(matriz_dist_cor, nombres)
print(resultado_gower_cor)
## PCA
datos_num <- datos_original[, sapply(datos_original, is.numeric)]
datos_num
# PCA solo con  todas las numéricas
pca <- prcomp(datos_num, scale. = TRUE)
summary(pca)  # ver varianza explicada

# Quedarse con componentes que expliquen >80%
var_exp <- summary(pca)$importance[2, ]
n_comp <- which(cumsum(var_exp) >= 0.8)[1]
datos_pca <- as.data.frame(pca$x[, 1:n_comp])

# Ahora puedes usar datos_pca junto con las categóricas en Gower
datos_gower_pca <- cbind(datos_pca, datos[, categoricas])
datos_gower_pca
# Calcular distancia Gower
dist_gower_pca <- daisy(datos_gower_pca, metric = "gower")
matriz_dist_pca <- as.matrix(dist_gower_pca)

resultado_gower_pca <- obtener_vecinos(matriz_dist_pca, nombres)
print(resultado_gower_pca)
print(resultado_gower_cor)
print(resultado_gower)
