library(xgboost)
iris2 = iris

# 增加一個 factor 變數當作解釋變數, 否則 iris 資料檔的
# 解釋變數都是數值變數, 不符合實務上常見的資料形態
# manually add a categorical variable as one of the features
# to reflect real-world situation.
iris2$id = factor(sample(c("A","B"),150,replace=T))

# Use sparse matrix in Matrix package
library(Matrix)
# 把所有解釋變數都轉為矩陣型態
# convert all feature variables to a sparse matrix
xdata = sparse.model.matrix(Species ~ .-1, data = iris2)

# Species 分類數目
# number of categories in response variable
m = nlevels(iris2$Species)

# 把目標變數 Species 三個分類轉成 0,1,2. 必須從 0 開始
# recode Y as 0,1,2,...,m-1
Y = as.integer(iris2$Species) - 1

# set random seed
set.seed(12345)

# xgboost 參數設定 (xgboost parameters setup)
param = list("objective" = "multi:softprob",
             "eval_metric" = "mlogloss",
             "num_class" = m
)

# build the model
result = xgboost(param=param, data=xdata, label=Y, nrounds=20)

# 計算預測值 (get prediction)
Ypred = predict(result,xdata)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
# colnames(Ypred) = levels(iris2$Species)
Ypred = levels(iris2$Species)[max.col(Ypred)]

# 混淆矩陣 (confusion matrix)
t0 = table(iris2$Species,Ypred)
t0

# 預測正確率 (accuracy)
sum(diag(t0))/sum(t0)

# 解釋變數重要性 (variable importance)
imp = xgb.importance(names(iris2[,-5]),model=result)
print(imp)

library(Ckmeans.1d.dp)
xgb.plot.importance(imp)
library(DiagrammeR)
xgb.plot.tree(feature_names=names(iris[,-5]),model=result, n_first_tree=2)
