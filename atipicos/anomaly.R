anomaly<-readRDS("colext.rds")

summary(anomaly$Edad)
summary(anomaly$Estatura)
anomaly1<-subset(anomaly,is.na(anomaly$Edad)==0)
anomaly2<-subset(anomaly1,Estatura>0)
nrow(anomaly)
nrow(anomaly1)
nrow(anomaly2)

boxplot(anomaly2$Edad)
boxplot(anomaly2$Estatura)

anomaly3<-subset(anomaly2,Edad<116)
anomaly4<-subset(anomaly3,Estatura>24 & Estatura<252)
nrow(anomaly3)
nrow(anomaly4)
boxplot(anomaly4$Edad)
boxplot(anomaly4$Estatura)

library(ggplot2)
ggplot(aes(x=Edad,y=Estatura), data= anomaly4)+ geom_point()

##guardo los boxplots
boxedad<-boxplot(anomaly4$Edad)
boxestat<-boxplot(anomaly4$Estatura)
#creo variables que son 1 si es anómalo, 0 si no
##para edad
anomaly4$boxage<-as.factor(ifelse(anomaly4$Edad<boxedad$stats[1]|anomaly4$Edad>boxedad$stats[5],1,0))
##para estatura
anomaly4$boxtall<-as.factor(ifelse(anomaly4$Estatura<boxestat$stats[1]|anomaly4$Estatura>boxestat$stats[5],1,0))
##grafico
ggplot(data= anomaly4, aes(x=Edad,y=Estatura, color=boxage, shape=boxtall))+ geom_point()


#solo numericos
numericos <- sapply(anomaly4, is.numeric)
numeros<- anomaly4[ ,numericos]
#solo edad y estatura (hay una tercera variable)
numerosfin<-numeros[,1:2]
#estandarizar las variables
anomz<-as.data.frame(scale(numerosfin))
#señalar los extremos
anomz$zage<-as.factor(ifelse(anomz$Edad>=3|anomz$Edad<=-3,1,0))
anomz$ztall<-as.factor(ifelse(anomz$Estatura>=3|anomz$Estatura<=-3,1,0))

#unirlos a la base inicial
anomaly5<-as.data.frame(cbind(anomaly4,anomz$zage,anomz$ztall))

ggplot(data= anomaly5, aes(x=Edad,y=Estatura, color=anomz$zage, shape=anomz$ztall))+ geom_point()


#medias para cada punto retirando el punto
listmean <- lapply(1:nrow(numeros), function(i) mean(numeros$Estatura[-i]))
#desviaciones para cada punto sin el punto
listsd <- lapply(1:nrow(numeros), function(i) sd(numeros$Estatura[-i]))
#crear las variables nuevas
numeros$mean<-unlist(listmean)
numeros$sd<-unlist(listsd)
##señalar el extremo
numeros$zestatlou<-as.factor(ifelse(numeros$Estatura>numeros$mean+3*numeros$sd|numeros$Estatura<numeros$mean-3*numeros$sd,1,0))
##unir la base de datos
anomaly6<-as.data.frame(cbind(anomaly5,numeros$zestatlou))


##observar
table(anomaly6$`anomz$ztall`,anomaly6$`numeros$zestatlou`)

ggplot(data= anomaly5, aes(x=Edad,y=Estatura, color=numeros$zestatlou))+ geom_point()


library(mvoutlier)
##utilizo los datos sin estandarizar
##obtengo los outliers (ver gráfico)
basado<-aq.plot(as.matrix(numerosfin))
##guardo la información y la uno a la base de datos
basado2<-as.data.frame(basado)
anomaly7<-as.data.frame(cbind(anomaly6,basado2))
##grafico
ggplot(data= anomaly7, aes(x=Edad,y=Estatura, color=outliers))+ geom_point()

## DB-SCAN
##uso datos estandarizados
kNNdistplot(anomz[,1:2],k=50)
#escojo eps. Alto si solo quiero outliers
dbout<-dbscan(anomz[,1:2],eps=0.3)
#Miro la tabla 
table(dbout$cluster)
anomaly7$dbout<-as.factor(ifelse(dbout$cluster==0,1,0))
##uno a la base de datos y grafico
ggplot(data= anomaly7, aes(x=Edad,y=Estatura, color=dbout))+ geom_point()


##dejo de nuevo solos edad y estatura estándar
lev<-anomz[,1:2]
#creo variable "fake"
lev$fake<-rnorm(nrow(lev),1,0)
#hago el modelo de regresión
resulta=lm(fake~.,data=lev)
#obtengo la matriz
leverage<-hat(model.matrix(resulta))
plot(leverage)

#guardo el boxplot
boxlev<-boxplot(leverage)
#busco la anomalía univariada
anomaly7$levout<-as.factor(ifelse(leverage>0.0015,1,0))

ggplot(data= anomaly7, aes(x=Edad,y=Estatura, color=levout))+ geom_point()

boxlev$stats[5]

library(entropy)
##base de datos
##sacar la muestra
numfins<-sample.int(nrow(numerosfin),0.1*nrow(numerosfin))
numfinred<-numerosfin[numfins,]
#estandarizar
numfinred2<-as.data.frame(scale(numfinred))

##entropia general
genentropy<-entropy(discretize(numfinred2$Estatura,10))
genentropy
#calcula la diferencia absoluta de la general con la entropía sin el punto
listentropy<-lapply(1:nrow(numfinred2),                    function(i) abs(genentropy-entropy(discretize(numfinred2$Estatura[-i],10))))
#sacar los resultados a una columna
entropyloo<-unlist(listentropy)                    
numfinred2$entropy<-entropyloo

#gráfico de caja y bigotes. Se observan algunos outliers
ak<-boxplot(numfinred2$entropy)


numfinred$entrout<-as.factor(ifelse(numfinred2$entropy>ak$stats[5],1,0))

ggplot(data= numfinred, aes(x=Edad,y=Estatura, color=entrout))+ geom_point()
