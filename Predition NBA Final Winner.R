#######################################################
##巨量資料分析 期末報告 題目：預測2017NBA年度總冠軍 ###
#######################################################
library(rpart)
library(party)
library(rattle)
#載入NBA資料集，資料期間1980年~2017年，總計1,044筆
NBA.Original <- read.csv("NBA19802017.csv",header=T,sep = ",")
#取出1980年~2016年資料
NBA <- NBA.Original[1:1014,]
NBA2017 <- NBA.Original[1015:1044,]
#顯示資料集
View(NBA)

#挑選出需要欄位--選擇要投入的屬性(把隊名去掉）
#NBA.CART <-subset(NBA, select = c(TeamNme,Age,Wins,Losses,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Final))
NBA.CART <-subset(NBA, select = c(Age,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Final))

#隨機取其中10%為測試資料
np <-ceiling(0.1*nrow(NBA.CART))
#顯示測試資料
#View(np)
test.index<-sample(1:nrow(NBA.CART),np)
#測試樣本
NBA.test <-NBA.CART[test.index,]
#訓練樣本
NBA.train <-NBA.CART[-test.index,]
#-----------------------------------------------------------------

#利用CART決策樹找出符合NBA總冠軍的屬性條件
#訓練樣本投入分類
NBA.tree <- rpart(Final~.,data=NBA.train)
NBA.tree
#畫出分類圖
summary(NBA.tree)
fancyRpartPlot(NBA.tree)
#plot(NBA.tree);text(NBA.tree,all=T)

#-----------------------------------------------------------------#
####依據決策樹分類結果比對2017年NBA季賽數據，僅金州勇士隊符合條件-#
#-----------------------------------------------------------------#

#訓練樣本的混淆矩陣與辨識正確率
Stats.train <-NBA$Final [-test.index]
train.pred <-factor(predict(NBA.tree,NBA.train,type = 'class'),level=levels(Stats.train))
table.train <-table(Stats.train,train.pred)
table.train
correct.train <-sum(diag(table.train)/sum(table.train))
correct.train

#測試樣本的混淆矩陣與辨識正確率
Stats.test <-NBA$Final[test.index]
test.pred <-factor(predict(NBA.tree,NBA.test,type = 'class'),levels = levels(Stats.test))
table.test <-table(Stats.test,test.pred)
table.test
correct.test <-sum(diag(table.test)/sum(table.test))
correct.test

NBA.CART.PR<-predict(NBA.tree,NBA2017,type = 'prob')
NBA.CART.PR<-predict(NBA.tree,NBA2017,type = 'class')

NBA.CART.PR
#----------------------------------------------------------------
#利用隨機森林進行預測實做
#----------------------------------------------------------------
#載入隨機森林模組
library(randomForest)

#挑選出需要欄位--選擇要投入的屬性
#NBA.Record <-subset(NBA.Original, select = c(Age,Wins,Losses,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Stats,Final))
NBA.Record <-subset(NBA.Original, select = c(Age,MOV,SOS,SRS,ORTG,DRTG,Pace,FTR,PAR,TS,OFFRFG,OFFTOV,OFFORB,OFFFTFGA,DFFEFG,DFFTOV,DFFDRB,DFFFTFGA,Final))

#指定1980至2016年的資料為訓練資料
NBA.rf.train <- NBA.Record[1:1014,]
#指定2017年的資料為測試資料，先假設2017年沒有總冠軍
#雖然此設定不符賽制，為方便後續實做，先這樣假設
NBA.rf.test <- NBA.Record[1015:1044,]
#設定隨機種子
set.seed(100)

#使用訓練樣本進行建模與評估辨識正確率
#使用預設500顆樹
NBA.rf <-randomForest(Final~.,data=NBA.rf.train,importance=TRUE,proximity=TRUE,do.trace = 100)
plot(NBA.rf)
legend('topright', colnames(NBA.rf$err.rate), col=1:3, fill=1:3)
print(NBA.rf)
#設定1000顆樹
NBA.rf1000 <-randomForest(Final~.,data=NBA.rf.train,importance=TRUE,proximity=TRUE,do.trace = 100,ntree=1000)
plot(NBA.rf1000)
legend('topright', colnames(NBA.rf$err.rate), col=1:3, fill=1:3)
print(NBA.rf1000)

#------------------------------------------------------
#500顆樹與1000顆樹，OOB值均為3.65%，考量運算使用500顆樹
#------------------------------------------------------

#判斷變數重要性並畫圖
varImpPlot(NBA.rf)
round(importance(NBA.rf),2)
#----------------------------------------------
#重要變數為勝場數（Wins)、勝場間隔(MOV）

#----------------------------------------------

#擷取混淆矩陣取得更精確平均辨識率
names(NBA.rf)
sum(diag(NBA.rf$confusion)/sum(NBA.rf$confusion))

#-----------------------------------------------
#調整模型
#-----------------------------------------------

#找出最佳的mtry參數
n=ncol(NBA.Record)-1#扣掉要預測的屬性
model_err_rate=1
for(i in 1:n)
{
  result=randomForest(Final~.,data=NBA.Record,mtry=i,importance=TRUE,ntree=500,do.trace = 100)
  model_err_rate[i]=mean(result$err.rate)
  cat("Model",i,":",model_err_rate[i],"\n")
}

#找出錯誤率最小模型
mtry_value=which.min(model_err_rate)
cat("mtry set:",mtry_value)

#重新設定mtry參數，並使用訓練樣本重新建模與評估辨識正確率
NBA.rf.Opt <-randomForest(Final~.,data=NBA.rf.train,mtry=mtry_value,importance=TRUE,proximity=TRUE,ntree=500,do.trace = 100)
print(NBA.rf.Opt)
#畫圖判別ntree參數設定
plot(NBA.rf.Opt)
legend('topright', colnames(NBA.rf$err.rate), col=1:3, fill=1:3)
print(NBA.rf.Opt)
#重新檢視重要變數並畫圖
varImpPlot(NBA.rf.Opt)
round(importance(NBA.rf.Opt),2)
#----------------------------------------------
#重要變數明顯為勝場數（Wins)、勝場間隔(MOV）
#----------------------------------------------

#第1次預測2017年度總冠軍
NBA.LC.prediction <- predict(NBA.rf.Opt, NBA.rf.test,type = 'prob')
NBA.LC.prediction <- predict(NBA.rf, NBA.rf.test,type = 'prob')

#solution <- data.frame(OrininalPrediction = NBA.rf.test$Final, RF.Prediction = NBA.LC.prediction)
solution <- data.frame(RF.Prediction = NBA.LC.prediction)
#將預測結果寫入檔案
write.csv(solution, file = 'NBA2017LC01.csv', row.names = F)

#----------------------------------------------
#運算結果，2017年真的沒有總冠軍<---當然不可能
#----------------------------------------------

#只提取重要變數
#NBA.newRecord <-subset(NBA.Original, select = c(Wins,MOV,OFFORB,DFFEFG,Final))
#NBA.newRecord <-subset(NBA.Original, select = c(Wins,MOV,OFFORB,Final))
NBA.newRecord <-subset(NBA.Original, select = c(Wins,MOV,Final))

#指定1980至2016年的資料為訓練資料
NBA.newrf.train <- NBA.newRecord[1:1014,]
#指定2017年的資料為測試資料，先假設2017年沒有總冠軍
#雖然此設定不符賽制，為方便後續實做，先這樣假設
NBA.newrf.test <- NBA.newRecord[1015:1044,]

#使用訓練樣本進行建模與評估辨識正確率
#使用預設500顆樹
NBA.newrf <-randomForest(Final~.,data=NBA.newrf.train,importance=TRUE,proximity=TRUE,do.trace = 100)
plot(NBA.newrf)
legend('topright', colnames(NBA.newrf$err.rate), col=1:3, fill=1:3)
print(NBA.newrf)
#設定1000顆樹
NBA.newrf1000 <-randomForest(Final~.,data=NBA.newrf.train,importance=TRUE,proximity=TRUE,do.trace = 100,ntree=1000)
plot(NBA.newrf1000)
legend('topright', colnames(NBA.newrf$err.rate), col=1:3, fill=1:3)
print(NBA.newrf1000)

#------------------------------------------------------
#500顆樹與1000顆樹，OOB值均為3.65%，考量運算使用500顆樹
#------------------------------------------------------

#擷取混淆矩陣取得更精確平均辨識率
names(NBA.newrf)
sum(diag(NBA.newrf$confusion)/sum(NBA.newrf$confusion))

#-----------------------------------------------
#調整模型
#-----------------------------------------------

#找出最佳的mtry參數
n1=ncol(NBA.newRecord)-1#扣掉要預測的屬性
new_model_err_rate=1
for(i in 1:n1)
{
  result=randomForest(Final~.,data=NBA.newRecord,mtry=i,importance=TRUE,ntree=500,do.trace = 100)
  new_model_err_rate[i]=mean(result$err.rate)
  cat("Model",i,":",new_model_err_rate[i],"\n")
}

#找出錯誤率最小模型
new_mtry_value=which.min(new_model_err_rate)
cat("mtry set:",new_mtry_value)

#重新設定mtry參數，並使用訓練樣本重新建模與評估辨識正確率
NBA.newrf.Opt <-randomForest(Final~.,data=NBA.newrf.train,mtry=new_mtry_value,importance=TRUE,proximity=TRUE,ntree=500,do.trace = 100)
print(NBA.newrf.Opt)
#畫圖判別ntree參數設定
plot(NBA.newrf.Opt)
legend('topright', colnames(NBA.newrf$err.rate), col=1:3, fill=1:3)
print(NBA.newrf.Opt)

#第2次預測2017年度總冠軍
#Type參數設定為機率
NBA.LC.newprediction <- predict(NBA.newrf.Opt, NBA.newrf.test,type='prob')
#Type參數設定為預測單一值（老師打槍）
#NBA.LC.newprediction <- predict(NBA.newrf.Opt, NBA.newrf.test,type = 'response')

#solution <- data.frame(OrininalPrediction = NBA.rf.test$Final, RF.Prediction = NBA.LC.newprediction)
newsolution <- data.frame(RF.Prediction = NBA.LC.newprediction)
#將預測結果寫入檔案
write.csv(newsolution, file = 'NBA2017LC02.csv', row.names = F)

#----------------------------------------------
#運算結果，預測2017年總冠軍為金州勇士隊
#----------------------------------------------