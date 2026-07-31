install.packages("recommenderlab")
library(recommenderlab)
library(tidyverse)
library(readxl)
data("MovieLense")
##primer ejercicio: datos de la librería
##miremos su estructura y obtener los datos
str(MovieLense)
guarda<-as.data.frame(as.matrix(MovieLense@data))
dim(guarda)
colnames(guarda)
guarda[0:2,0:2]
str(guarda)
##Data understanding
#1. Cómo puntua la gente?
raw_puntuaciones<-as.vector(MovieLense@data)
hist(raw_puntuaciones)
prop.table(table(raw_puntuaciones))
##el 93% son datos no existentes!!
puntuaciones_reales<-raw_puntuaciones[raw_puntuaciones!=0]
hist(puntuaciones_reales)
summary(puntuaciones_reales)
##hay sesgo?
base<-as(MovieLense,"matrix")
dim(base)
base[0:2,0:2]
medias_persona<-as.data.frame(apply(base,1,function(x) mean(x, na.rm=TRUE)))
psych::describe(medias_persona)
hist(medias_persona$`apply(base, 1, function(x) mean(x, na.rm = TRUE))`)
medias_movie<-as.data.frame(apply(base,2,function(x) mean(x, na.rm=TRUE)))
psych::describe(medias_movie)
hist(medias_movie$`apply(base, 2, function(x) mean(x, na.rm = TRUE))`)
peli_persona<-as.data.frame(apply(base,1,function(x) sum(!is.na(x))))
psych::describe(peli_persona)
punt_peli<-as.data.frame(apply(base,2,function(x) sum(!is.na(x))))
psych::describe(punt_peli)

##Data preparation: normalizar / seleccionar cantidad
summary(peli_persona)
summary(punt_peli)
seleccion<-MovieLense[rowCounts(MovieLense) > 32, colCounts(MovieLense) > 7]
dim(seleccion)
seleccionz<-normalize(seleccion)
##ver los datos seleccionados y normalizados
seleccionver<-as.vector(seleccionz@data)
seleccionver<-seleccionver[seleccionver!=0]
hist(seleccionver)

##Modeling
##por defecto trabaja con la distancia coseno
##se le pueden agregar otras (p.e, pearson, euclidean, jaccard: pero no manhattan por ejemplo)
#hacer modelo User-based
dim(seleccion)
modelo_usuario<-Recommender(seleccion[1:700],method="UBCF", param=list(normalize="Z-score"))

##generar y ver recomendación
recomendacion1<-predict(modelo_usuario,seleccion[701])
as(recomendacion1,"list")

recomendacion5<-bestN(recomendacion1,n=5)
as(recomendacion5,"list")


##modelo Item-based
modelo_item<-Recommender(seleccion[1:700],method="IBCF", param=list(normalize="Z-score"))
##generar y ver recomendación
recomendacion1item<-predict(modelo_item,seleccion[701])
as(recomendacion1item,"list")

recomendacion5item<-bestN(recomendacion1item,n=5)
as(recomendacion5item,"list")



##Evaluation
evaluar<-evaluationScheme(data=seleccion, method="split", train=0.85, given=10, goodRating=3)

modelo_train<-Recommender(data = getData(evaluar, "train"),
                               method = "UBCF", param=list(normalize="Z-score"))

predecir_test = predict(object = modelo_train,
                          newdata = getData(evaluar, "known"),
                          n = 5,
                          type = "ratings")

precision_uno = calcPredictionAccuracy(x = predecir_test,
                                       data = getData(evaluar, "unknown"),
                                       byUser = TRUE)
head(precision_uno)

precision_total = calcPredictionAccuracy(x = predecir_test,
                                       data = getData(evaluar, "unknown"))
precision_total

## repito el proceso para el modelo 2
modelo_train_i<-Recommender(data = getData(evaluar, "train"),
                          method = "IBCF", param=list(normalize="Z-score"))

predecir_test_i<- predict(object = modelo_train_i,
                        newdata = getData(evaluar, "known"),
                        n = 5,
                        type = "ratings")

precision_total_i = calcPredictionAccuracy(x = predecir_test_i,
                                         data = getData(evaluar, "unknown"))
precision_total_i
precision_total

##cambiar la métrica-- no funciona
modelo_train_p<-Recommender(data = getData(evaluar, "train"),
                            method = "IBCF", param=list(method="pearson"))

predecir_test_p<- predict(object = modelo_train_p,
                          newdata = getData(evaluar, "known"),
                          n = 5,
                          type = "ratings")

precision_total_p <- calcPredictionAccuracy(x = predecir_test_p,
                                           data = getData(evaluar, "unknown"))
precision_total_i
precision_total
precision_total_p

##popular
modelo_train_b<-Recommender(data = getData(evaluar, "train"),method="POPULAR", param=list(normalize="Z-score"))

predecir_test_b<- predict(object = modelo_train_b,
                          newdata = getData(evaluar, "known"),
                          n = 5,
                          type = "ratings")

precision_total_b <- calcPredictionAccuracy(x = predecir_test_b,
                                            data = getData(evaluar, "unknown"))

precision_total_i
precision_total
precision_total_p
precision_total_b

##hagamos entonces un sistema hibrido
hibrido<-HybridRecommender(modelo_train,modelo_train_b,weights=c(0.2,0.8))

predecir_test_h<- predict(object = hibrido,
                          newdata = getData(evaluar, "known"),
                          n = 5,
                          type = "ratings")
precision_total_h <- calcPredictionAccuracy(x = predecir_test_h,
                                            data = getData(evaluar, "unknown"))
precision_total_h


####segundo_ejercicio#####
libros<-read_excel("books_recommender_trimclean.xlsx")
str(libros)
head(libros)
libros_wider<-pivot_wider(libros,names_from=libro_nombre, values_from=Rating)
str(libros_wider)
rownames(libros_wider)<-libros_wider$user_id
libros_wider<-libros_wider[,-1]
dim(libros_wider)
libros_r<-as(as.matrix(libros_wider),"realRatingMatrix")

raw_puntuaciones<-as.vector(libros_r@data)
hist(raw_puntuaciones)
prop.table(table(raw_puntuaciones))

puntuaciones_reales<-raw_puntuaciones[raw_puntuaciones!=0]
hist(puntuaciones_reales)
summary(puntuaciones_reales)
length(puntuaciones_reales)/length(raw_puntuaciones)

##hay sesgo?
medias_persona<-as.data.frame(apply(libros_wider,1,function(x) mean(x, na.rm=TRUE)))
ver_media_persona<-psych::describe(medias_persona)
medias_libro<-as.data.frame(apply(libros_wider,2,function(x) mean(x, na.rm=TRUE)))
ver_media_libro<-psych::describe(medias_libro)
libro_persona<-as.data.frame(apply(libros_wider,1,function(x) sum(!is.na(x))))
ver_libro_persona<-psych::describe(libro_persona)
punt_libro<-as.data.frame(apply(libros_wider,2,function(x) sum(!is.na(x))))
psych::describe(punt_libro)

##Data preparation: normalizar / seleccionar cantidad
summary(libro_persona)
summary(punt_libro)
##dados los pocos libros por usuario, es necesario tener un minimo de 7 
seleccion<-libros_r[rowCounts(libros_r) > 7, colCounts(libros_r) > 127]
dim(seleccion)
seleccionz<-normalize(seleccion)
##ver los datos seleccionados y normalizados
seleccionver<-as.vector(seleccionz@data)
seleccionver<-seleccionver[seleccionver!=0]
hist(seleccionver)

##Modeling
##por defecto trabaja con la distancia coseno
##se le pueden agregar otras (p.e, pearson, euclidean, jaccard: pero no manhattan por ejemplo)
#hacer modelo User-based
dim(seleccion)
modelo_usuario<-Recommender(seleccion[1:2000],method="UBCF", param=list(normalize="Z-score"))

##generar y ver recomendación
recomendacion1<-predict(modelo_usuario,seleccion[2001])
as(recomendacion1,"list")

recomendacion5<-bestN(recomendacion1,n=5)
as(recomendacion5,"list")


##modelo Item-based
modelo_item<-Recommender(seleccion[1:2000],method="IBCF", param=list(normalize="Z-score"))
##generar y ver recomendación
recomendacion1item<-predict(modelo_item,seleccion[2001])
as(recomendacion1item,"list")

recomendacion5item<-bestN(recomendacion1item,n=5)
as(recomendacion5item,"list")



##Evaluation
##given 2, 5, 10 or negative (all-but-1)
#al evaluar da solo x items al usuario, y como lo predice
evaluar<-evaluationScheme(data=seleccion, method="split", train=0.8, given=-1, goodRating=4)

modelo_train<-Recommender(data = getData(evaluar, "train"),
                          method = "UBCF", param=list(normalize="Z-score"))

predecir_test = predict(object = modelo_train,
                        newdata = getData(evaluar, "known"),
                        n = 5,
                        type = "ratings")

precision_uno = calcPredictionAccuracy(x = predecir_test,
                                       data = getData(evaluar, "unknown"),
                                       byUser = TRUE)
head(precision_uno)

precision_total = calcPredictionAccuracy(x = predecir_test,
                                         data = getData(evaluar, "unknown"))
precision_total

## repito el proceso para el modelo 2
modelo_train_i<-Recommender(data = getData(evaluar, "train"),
                            method = "IBCF", param=list(normalize="Z-score"))

predecir_test_i<- predict(object = modelo_train_i,
                          newdata = getData(evaluar, "known"),
                          n = 5,
                          type = "ratings")

precision_total_i = calcPredictionAccuracy(x = predecir_test_i,
                                           data = getData(evaluar, "unknown"))
precision_total_i
precision_total

##cambiar la métrica--
modelo_train_p<-Recommender(data = getData(evaluar, "train"),
                            method = "IBCF", param=list(method="euclidean"))

predecir_test_p<- predict(object = modelo_train_p,
                          newdata = getData(evaluar, "known"),
                          n = 5,
                          type = "ratings")

precision_total_p <- calcPredictionAccuracy(x = predecir_test_p,
                                            data = getData(evaluar, "unknown"))
precision_total_i
precision_total
precision_total_p

##RANDOM
modelo_train_b<-Recommender(data = getData(evaluar, "train"),method="RANDOM", param=list(normalize="Z-score"))

predecir_test_b<- predict(object = modelo_train_b,
                          newdata = getData(evaluar, "known"),
                          n = 5,
                          type = "ratings")

precision_total_b <- calcPredictionAccuracy(x = predecir_test_b,
                                            data = getData(evaluar, "unknown"))

precision_total_i
precision_total
precision_total_p
precision_total_b

###cierre segundo###


##ejercicio 2- matriz binaria- cosmetics
cosmetics<-read_excel("cosmeticspanreduced.xlsx")
dim(cosmetics)
str(cosmetics)
class(seleccion)
cosmbin<-as(as.matrix(cosmetics),"binaryRatingMatrix")
str(cosmbin)
modelo_cosm<-Recommender(cosmbin[1:750],method="UBCF", param=list(method="jaccard"))

##generar y ver recomendación
recomendacion_cosm<-predict(modelo_cosm,cosmbin[801])
as(recomendacion_cosm,"list")
##solo recomienda lo que no ha comprado
recomendacion5cosm<-bestN(recomendacion_cosm,n=5)
as(recomendacion5cosm,"list")

##Evaluation
##Evaluation
evaluar_cosm<-evaluationScheme(data=cosmbin, method="split", train=0.85, given=2)

modelo_train_cosm<-Recommender(data = getData(evaluar_cosm, "train"),
                          method = "UBCF", param=list(method="jaccard"))

predecir_test_cosm<-predict(object = modelo_train_cosm,
                        newdata = getData(evaluar_cosm, "known"), type="topNList", n=5)

precision_cosm = calcPredictionAccuracy(x = predecir_test_cosm,
                                       data = getData(evaluar_cosm, "unknown"), given=2)
precision_cosm


modelo_train_r<-Recommender(data = getData(evaluar_cosm, "train"),
                               method = "RANDOM", param=list(method="jaccard"))

predecir_test_r<-predict(object = modelo_train_r,
                            newdata = getData(evaluar_cosm, "known"), type="topNList", n=5)

precision_cosm_r = calcPredictionAccuracy(x = predecir_test_r,
                                        data = getData(evaluar_cosm, "unknown"), given=2)
precision_cosm
precision_cosm_r

hibrido2<-HybridRecommender(modelo_train_cosm,modelo_train_r,weights=c(0.99,0.01))
predecir_test_hy<-predict(object = hibrido2,
                         newdata = getData(evaluar_cosm, "known"), type="topNList", n=5)
precision_cosm_hy = calcPredictionAccuracy(x = predecir_test_hy,
                                          data = getData(evaluar_cosm, "unknown"), given=2)

precision_cosm
precision_cosm_r
precision_cosm_hy
str(predecir_test_hy)

##parafunksvd debo darle una matriz, con potenciales NAs. 
#es más intensivo en cómputo
data("Jester5k")
JesterJokes[5]
dim(Jester5k)
matriz<-as(Jester5k[1:100],"matrix")
porfilam<-apply(matriz,1,function(x) mean(x,na.rm=TRUE))
psych::describe(porfilam)
obtener<-funkSVD(matriz,verbose=TRUE,k=15)
#obtener<-funkSVD(matriz,k=50)
#summary(Jester5k@data)
##POR defecto el rank que es k=10
nuevamatriz<- as(Jester5k[101:135], "matrix")
puntuar<-predict(obtener, nuevamatriz)
medir<-abs(nuevamatriz-puntuar)
porfila<-apply(medir,1,function(x) mean(x,na.rm=TRUE))
porfila
MAE<-mean(porfila)
sdMAE<-sd(porfila)
MAE
sdMAE
medir2<-(nuevamatriz-puntuar)**2
porfila2<-apply(medir2,1,function(x) mean(x,na.rm=TRUE))
MSE<-mean(porfila2)
RMSE<-sqrt(mean(porfila2))
#rmse(nuevamatriz,puntuar)
MAE
MSE
RMSE


