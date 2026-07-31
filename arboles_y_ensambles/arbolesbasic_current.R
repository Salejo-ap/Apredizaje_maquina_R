getwd()
install.packages("party")
install.packages("rpart")
install.packages("rpart.plot")

library("rpart")
library("party")
library("rpart.plot")


cerealtrain<-read.csv("cerealtrain.csv", header=TRUE, sep=";", dec=",")
cerealtest<-read.csv("cerealtest.csv", header=TRUE, sep=";", dec=",")
str(cerealtrain)
cerealtrain[,1:5]<-lapply(cerealtrain[,1:5],as.factor)
cerealtest[,1:5]<-lapply(cerealtest[,1:5],as.factor)
levels(cerealtrain$edadcat)<-c("31-45","> 60","46-60","< 31")
levels(cerealtest$edadcat)<-c("31-45","46-60","< 31","> 60")
cerealtest$edadcat<-factor(cerealtest$edadcat, levels= c("31-45","> 60","46-60","< 31"))
set.seed(3435)
arbol <- rpart(desayuno ~ ., data=cerealtrain)
rpart.plot(arbol) 

# Ver resultado del desempeno del arbol de acuerdo a cp por cada particion
printcp(arbol)
# visualizar resultados cost complexity pruning 
plotcp(arbol)

arbolPodado = prune(arbol, cp = 0.02)
rpart.plot(arbolPodado)
printcp(arbolPodado)


arbolnew <- rpart(desayuno ~ ., data=cerealtrain,
                  control=rpart.control(xval=10, minbucket = 5, minisplit=30, cp=0.0041))
rpart.plot(arbolnew)

arbolnew2 <- rpart(desayuno ~ ., data=cerealtrain,
                   control=rpart.control(maxdepth=2))
rpart.plot(arbolnew2)

arbre<-ctree(desayuno~., data=cerealtrain)
plot(arbre)

#predicciones
pr<-predict(arbol,cerealtest, type="class")
pr1<-predict(arbolPodado,cerealtest,type="class")
pr2<-predict(arbolnew,cerealtest,type="class")
pr3<-predict(arbolnew2,cerealtest,type="class")
prchaid<-predict(arbre,cerealtest,type="response")

pr1
pr1a<-predict(arbol,cerealtest, type="prob")
pr1a<-as.data.frame(pr1a)
#matriz de confusion
library(caret)
conf1<-confusionMatrix(pr,cerealtest$desayuno)
conf2<-confusionMatrix(pr1,cerealtest$desayuno)
conf3<-confusionMatrix(pr2,cerealtest$desayuno)
conf4<-confusionMatrix(pr3,cerealtest$desayuno)
conf5<-confusionMatrix(prchaid,cerealtest$desayuno)
conf1$byClass[,"F1"]
conf2$byClass[,"F1"]
conf3$byClass[,"F1"]
conf4$byClass[,"F1"]
conf5$byClass[,"F1"]

Fconf1<-as.data.frame(conf1$byClass)
##Macro F1
macrof1_1<-mean(Fconf1$F1)
##Weighted F1
macrof1_1_weighted<-weighted.mean(Fconf1$F1,prop.table(table(cerealtest$desayuno)))


Fconf2<-as.data.frame(conf2$byClass)
##Macro F1
macrof1_2<-mean(Fconf2$F1)
##Weighted F1
macrof1_2_weighted<-weighted.mean(Fconf2$F1,prop.table(table(cerealtest$desayuno)))

Fconf3<-as.data.frame(conf3$byClass)
##Macro F1
macrof1_3<-mean(Fconf3$F1)
##Weighted F1
macrof1_3_weighted<-weighted.mean(Fconf3$F1,prop.table(table(cerealtest$desayuno)))

Fconf4<-as.data.frame(conf4$byClass)
##Macro F1
macrof1_4<-mean(Fconf4$F1)
##Weighted F1
macrof1_4_weighted<-weighted.mean(Fconf4$F1,prop.table(table(cerealtest$desayuno)))

Fconf5<-as.data.frame(conf5$byClass)
##Macro F1
macrof1_5<-mean(Fconf5$F1)
##Weighted F1
macrof1_5_weighted<-weighted.mean(Fconf5$F1,prop.table(table(cerealtest$desayuno)))


macrof1_1
macrof1_2
macrof1_3
macrof1_4
macrof1_5

macrof1_1_weighted
macrof1_2_weighted
macrof1_3_weighted
macrof1_4_weighted
macrof1_5_weighted

##micro f1= accuracy
conf1$overall["Accuracy"]
conf2$overall["Accuracy"]
conf3$overall["Accuracy"]
conf4$overall["Accuracy"]
conf5$overall["Accuracy"]




