#####################################
##決策樹、隨機森林專題報告        ###
#####################################
library(rpart)
#載入鳶尾花資料集，總計150筆
data("iris")
iris
dim(iris)
#CART決策樹
#隨機取其中10%為測試資料
np <-ceiling(0.1*nrow(iris))
np
test.index<-sample(1:nrow(iris),np)
#測試樣本
iris.test <-iris[test.index,]
#訓練樣本
iris.train <-iris[-test.index,]
#訓練樣本投入分類
iris.tree <- rpart(Species~.,data=iris.train)
iris.tree
#畫出分類圖
summary(iris.tree)
plot(iris.tree);text(iris.tree)
#訓練樣本的混淆矩陣與辨識正確率97%
species.train <-iris$Species[-test.index]
train.pred <-factor(predict(iris.tree,iris.train,type = 'class'),level=levels(species.train))
table.train <-table(species.train,train.pred)
table.train
correct.train <-sum(diag(table.train)/sum(table.train))
correct.train
#測試樣本的混淆矩陣與辨識正確率80%
species.test <-iris$Species[test.index]
test.pred <-factor(predict(iris.tree,iris.test,type = 'class'),levels = levels(species.test))
table.test <-table(species.test,test.pred)
table.test
correct.test <-sum(diag(table.test)/sum(table.test))
correct.test

#CHAID決策樹
library(CHAID)
#依據花萼長度寬度分割資料集各為10等分
SepL=cut(iris$Sepal.Length,breaks = 10)
SepW=cut(iris$Sepal.Width,breaks = 10)
PetL=cut(iris$Petal.Length,breaks = 10)
PetW=cut(iris$Petal.Width,breaks = 10)
SepL=ordered(SepL)
SepW=ordered(SepW)
PetL=ordered(PetL)
PetW=ordered(PetW)
iris2=data.frame(SepL,SepW,PetL,PetW,Species=iris$Species)
iris2
#建構CHAID決策樹模型
iris.chaid=chaid(Species~.,data=iris2)
#畫出CHAID決策樹狀圖
plot(iris.chaid)
print(iris.chaid)
#計算混淆矩陣與預測正確率96%
pred <-predict(iris.chaid,newdata = iris2)
pred
table.chaid=table(Species=iris2$Species,prediction=pred)
table.chaid
sum(diag(table.chaid)/sum(table.chaid))

#隨機森林
library(randomForest)
set.seed(71)
#使用訓練樣本進行建模與評估辨識正確率97.04%
iris.rf <-randomForest(Species~.,data=iris.train,importance=TRUE,proximity=TRUE)
print(iris.rf)
#判斷變數重要性
round(importance(iris.rf),2)
#擷取混淆矩陣取得更精確平均辨識率96.98%
names(iris.rf)
sum(diag(iris.rf$confusion)/sum(iris.rf$confusion))
#測試樣本混淆矩陣與平均預測正確率80%
rf.pred=predict(iris.rf,newdata = iris.test)
rf.pred
table.test <-table(Species=species.test,Predicated=rf.pred)
table.test
sum(diag(table.test)/sum(table.test))

#利用隨機森林分群
set.seed(17)
iris.urf=randomForest(iris[,-5])
iris.urf
MDSplot(iris.urf,iris$Species,palette = rep(1,3),pch = as.numeric(iris$Species))
