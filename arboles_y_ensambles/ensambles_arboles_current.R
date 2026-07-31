library(readxl)
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)
library(party)
library(mboost)
library(adabag)
library(randomForest)
library(xgboost)

bag1<-bagging(desayuno~.,data=cerealtrain)
bag1$votes
bag1$class
bag1$importance

bag2<-bagging(desayuno~.,data=cerealtrain,mfinal = 200,
              control=rpart.control(minbucket=30,maxdepth=2))
predbag1<-predict(bag1,cerealtest)
predbag2<-predict(bag2,cerealtest)
p1<-confusionMatrix(as.factor(predbag1$class),cerealtest$desayuno)
p1$table
p1$byClass
#f1 macro micro
Fens1<-as.data.frame(p1$byClass)
##Macro F1
macrof1_e1<-mean(Fens1$F1)
##Weighted F1
macrof1_e1_weighted<-weighted.mean(Fens1$F1,prop.table(table(cerealtest$desayuno)))
accuracyens1<-p1$overall["Accuracy"]


p2<-confusionMatrix(as.factor(predbag2$class),cerealtest$desayuno)
p2$byClass
#f1 macro micro
Fens2<-as.data.frame(p2$byClass)
##Macro F1
macrof1_e2<-mean(Fens2$F1)
##Weighted F1
macrof1_e2_weighted<-weighted.mean(Fens2$F1,prop.table(table(cerealtest$desayuno)))
accuracyens2<-p2$overall["Accuracy"]


predrf1<-randomForest(desayuno~.,data=cerealtrain)
predrf1$importance
predrf2<-randomForest(desayuno~.,data=cerealtrain,
                      ntree=1000,mtry=1,nsize=2)
predicrf1<-predict(predrf1,cerealtest)
predicrf2<-predict(predrf2,cerealtest)


p3<-confusionMatrix(predicrf1,cerealtest$desayuno)
p4<-confusionMatrix(predicrf2,cerealtest$desayuno)

p3$byClass
#f1 macro micro
Fens3<-as.data.frame(p3$byClass)
##Macro F1
macrof1_e3<-mean(Fens3$F1)
##Weighted F1
macrof1_e3_weighted<-weighted.mean(Fens3$F1,prop.table(table(cerealtest$desayuno)))
accuracyens3<-p3$overall["Accuracy"]




p4$byClass
Fens4<-as.data.frame(p4$byClass)
##Macro F1
macrof1_e4<-mean(Fens4$F1)
##Weighted F1
macrof1_e4_weighted<-weighted.mean(Fens4$F1,prop.table(table(cerealtest$desayuno)))
accuracyens4<-p4$overall["Accuracy"]



boost1<-boosting(desayuno~.,cerealtrain)
boost2<-boosting(desayuno~.,cerealtrain,
                 mfinal=200,
                 control=rpart.control(minbucket=30,maxdepth=2))
pr5<-predict(boost1,cerealtest)
pr6<-predict(boost2,cerealtest)

p5<-confusionMatrix(as.factor(pr5$class),cerealtest$desayuno)
p6<-confusionMatrix(as.factor(pr6$class),cerealtest$desayuno)

p5$byClass
Fens5<-as.data.frame(p5$byClass)
##Macro F1
macrof1_e5<-mean(Fens5$F1)
##Weighted F1
macrof1_e5_weighted<-weighted.mean(Fens5$F1,prop.table(table(cerealtest$desayuno)))
accuracyens5<-p5$overall["Accuracy"]




p6$byClass
Fens6<-as.data.frame(p6$byClass)
##Macro F1
macrof1_e6<-mean(Fens6$F1)
##Weighted F1
macrof1_e6_weighted<-weighted.mean(Fens6$F1,prop.table(table(cerealtest$desayuno)))
accuracyens6<-p6$overall["Accuracy"]


modeloxg<-xgboost(x=cerealtrain[,1:4],y=cerealtrain$desayuno,max_depth=3,learning_rate=0.2,
                  nthread=10,nrounds=100)

prxg<-predict(modeloxg,cerealtest, type="class")
confxg<-confusionMatrix(prxg,cerealtest$desayuno)
confxg$byClass
Fens7<-as.data.frame(confxg$byClass)
##Macro F1
macrof1_e7<-mean(Fens7$F1)
##Weighted F1
macrof1_e7_weighted<-weighted.mean(Fens7$F1,prop.table(table(cerealtest$desayuno)))
accuracyens7<-confxg$overall["Accuracy"]


macrof1_1_weighted
macrof1_e1_weighted
macrof1_e2_weighted
macrof1_e3_weighted
macrof1_e4_weighted
macrof1_e5_weighted
macrof1_e6_weighted
macrof1_e7_weighted

