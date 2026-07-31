library(Rtsne)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(psych)
library(reshape2)
library(RColorBrewer)
library(readxl)
library(ggrepel)
library(tidyverse)
library(dbscan)

###EJEMPLO 1: PAPAS FRITAS##########

##traer la base de datos
fritasfp<-read_excel("fritas1.xlsx")
fritasf<-as.data.frame(fritasfp[,-1])
rownames(fritasf)<-fritasfp$Marca
str(fritasf)
fritas_tsne<-scale(fritasf[,1:38])
##crear el t-sne
set.seed(3445)
guardaen<-Rtsne(as.matrix(fritas_tsne), perplexity=5,eta=85)
reducen<-as.data.frame(guardaen$Y)
reducen$nombres<-fritasfp$Marca
colnames(reducen)
ggplot(reducen, aes(x=V1,y=V2, label=nombres))+geom_point()+geom_text_repel(size=2, max.overlaps=20)


###EJEMPLO 2: TEXTOS##########


########t-sne for the map of language#######

set.seed(11) 
##leo,centros, nombres iniciales y palabras asignadas a centros
##solo los valores de los centros
centros<-read_excel("center1024.xlsx")
colnames(centros)[1]<-"grupo1024"
##las 3 palabras clave, con la palabra escogida inicial
nombresa<-read_excel("nombreclusters.xlsx")

##observe que solo uso los centros
##use perplexity=10 
##n/10 seems to be a good choice
set.seed(3445)
guarda<-Rtsne(as.matrix(centros[,2:301]), perplexity=10, eta=85)
## se le pone nombre para gráfcio
nombres<-nombresa$descripción
reduc<-as.data.frame(guarda$Y)
reduc$nombres<-nombres
reduc$grupo1024<-centros$grupo1024

##se sugiere minponits= dimensionsx2, en mi caso 4 (default usa 5).
##el knn debe ser con minpoints-1, en este caso 5-1 o 4-1
kNNdistplot(reduc[,1:2],k=4)
dbout<-dbscan(reduc[,1:2],eps=2.1)
reducver<-as.data.frame(cbind(reduc,dbout$cluster))
colnames(reducver)[5]<-c("clusdbbase")
reducver$clusdb<-as.factor(reducver$clusdbbase)
colnames(reducver)
ggplot(reducver, aes(x=V1,y=V2, label=nombres, color=clusdb))+geom_point()+geom_text_repel(size=1, max.overlaps=100)+theme(legend.position = "none")
table(dbout$cluster)

reducver2<-subset(reducver,reducver$V1<0)
reducver2<-subset(reducver2,reducver$V2>0)

ggplot(reducver2, aes(x=V1,y=V2, label=nombres, color=clusdb))+geom_point()+geom_text_repel(size=2, max.overlaps=100)+theme(legend.position = "none")


##paises del mundo, ejercicio##




