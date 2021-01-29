rm(list = ls())
library(quantreg) 
library(data.table)
library(chron)
library(ellipsis)
library(forecast)
library(xts)
library(zoo)
library(stats)
library(dplyr)
library(corrr)
library(corrplot)
library(tidyverse)
library(miscTools)
library(cvAUC)
library(pROC)
library(glmnet)
library(jpeg)
require(caret)
library(ggplot2)
library(yardstick)
library(broom)
library(stringr)
library(tidyr)
library(purrr)
library(WRTDStidal)
library(varhandle)
library(parallel)
library(doParallel)
library(ISLR)
library(klaR)
library(e1071)
library(ada)
library(Metrics)
library(ranger)
library(gbm)


setwd("C:/Users/kadir/OneDrive/Masaüstü")

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}


#regression

data1<-read.csv("train1.csv", sep = ",")


data1<-na.omit(data1)
data1<-data.table(data1)
trainregression<-data1[1:15000,]
testregression<-data1[15001:21263,]
testmatrixx1<-as.matrix(testregression[,1:81])
n_folds=10

myGridGlmnet <- expand.grid(alpha= 1, lambda = c(0.20,0.25,0.30,0.35,0.40))
rf_grid=expand.grid(mtry = c(4,6,8,10,12), splitrule = c("variance"),min.node.size = c(5))
tree_grid=expand.grid(cp = c(1:10)/1000)
myGridGbm <- expand.grid(interaction.depth=c(1,3,5), n.trees = (1:3)*100,shrinkage=c(0.01,0.005,0.001), n.minobsinnode=10)

fitControl=trainControl(method = "cv", number = n_folds)     

set.seed(1)
penalized_model<- train(critical_temp~.,trainregression, method="glmnet",tuneGrid=myGridGlmnet ,trControl = fitControl)
penalized_model

rf_fit=train(critical_temp ~ ., data = data.frame(trainregression), method = "ranger",trControl = fitControl, num.trees=500,tuneGrid = rf_grid)
rf_fit

tree_fit1=train(critical_temp~ ., data =data.frame(trainregression),method = "rpart", trControl = fitControl, tuneGrid = tree_grid,tuneLength = 1 )
tree_fit1

gbm_model = train(critical_temp~.,data=data.frame(trainregression), method = "gbm",tuneGrid= myGridGbm ,trControl = fitControl)
gbm_model 

results = resamples(list(penalized_model=penalized_model,dtree=tree_fit1,rf=rf_fit,boosting=gbm_model),metrics='Accuracy')
bwplot(results,metric ="RMSE")
bwplot(results,metrics='Accuracy')


testregression[,predpenalized:=predict(penalized_model, testmatrixx1)]
testregression[,predforest:=predict(rf_fit, testmatrixx1)]
testregression[,predtree:=predict(tree_fit1, testmatrixx1)]
testregression[,predgbm:=predict(gbm_model , testmatrixx1)]

mape(testregression$critical_temp,testregression$predpenalized)
RMSE(testregression$critical_temp,testregression$predpenalized)

mape(testregression$critical_temp,testregression$predforest)
RMSE(testregression$critical_temp,testregression$predforest)

mape(testregression$critical_temp,testregression$predtree)
RMSE(testregression$critical_temp,testregression$predtree)

mape(testregression$critical_temp,testregression$predgbm)
RMSE(testregression$critical_temp,testregression$predgbm)



##Best method is random forest because it has the lowest RMSE and Mape values.



#
library("readxl")

data3<-read_excel("Data_Cortex_Nuclear.xls")
data3<-na.omit(data3)



data3<-na.omit(data3)
data3<-data.table(data3)
newdata3<-cbind(data3[,2:78],data3[,82])


sample_size = floor(0.6*nrow(newdata3))
set.seed(777)

## randomly split data in r
picked = sample(seq_len(nrow(newdata3)),size = sample_size)
trainnuclear =newdata3[picked,]
testnuclear =newdata3[-picked,]

testmatrixx2<-as.matrix(testnuclear[,1:77])
n_folds=10
myGridGlmnet <- expand.grid(alpha= 1, lambda = c(0.20,0.25,0.30,0.35,0.40))
fitControl=trainControl(method = "cv",
                        number = n_folds)                         
set.seed(1)
penalized_model2<- train(class~.,trainnuclear, method="glmnet",tuneGrid=myGridGlmnet ,trControl = fitControl)
penalized_model2




rf_grid=expand.grid(mtry = c(4,6,8,10,12),
                    splitrule = c("gini","extratrees"),
                    min.node.size = c(5))


rf_fit2=train(class ~ ., data = data.frame(trainnuclear), 
              method = "ranger", 
              trControl = fitControl, num.trees=500,
              tuneGrid = rf_grid)

rf_fit2


tree_grid=expand.grid(cp = c(1:10)/1000)


tree_fit2=train(class~ ., data =data.frame(trainnuclear),
                method = "rpart", 
                trControl = fitControl, 
                tuneGrid = tree_grid,tuneLength = 1 )

tree_fit2


myGridGbm <- expand.grid(interaction.depth=c(1,3,5), n.trees = (1:3)*100,shrinkage=c(0.01,0.005,0.001), n.minobsinnode=10)


gbm_model2 = train(class~.,data=data.frame(trainnuclear), method = "gbm",tuneGrid= myGridGbm ,trControl = fitControl)

gbm_model2 

unique(testnuclear$class)
testnuclear[,predpenalized:=predict(penalized_model2, testmatrixx2)]
testnuclear[,predforest:=predict(rf_fit2, testmatrixx2)]
testnuclear[,predtree:=predict(tree_fit2, testmatrixx2)]
testnuclear[,predgbm:=predict(gbm_model2 , testmatrixx2)]


Test_Performances2 <- data.table(
  Glm_Test_ACC=mean(testnuclear$class==testnuclear$predpenalized),
  Dt_Test_ACC =mean(testnuclear$class==testnuclear$predtree) ,
  Forest_Test_ACC =mean(testnuclear$class==testnuclear$predforest),
  SGBM_Test_ACC = mean(testnuclear$class==testnuclear$predgbm)
)

Test_Performances2

##Best method is random forest for this data. Because it has the highest accuracy rate.The worst one is penalized regression.






dataekstra<-read.csv("sat.tst", sep = " ")

for(i in 1:37){
  
  dataekstra[,i]<-as.numeric(as.character(dataekstra[,i]))
  
}

dataekstra<-na.omit(dataekstra)
dataekstra<-data.table(dataekstra)

dataekstra$c37<-as.character(as.numeric(dataekstra$c37))
sample_size = floor(0.8*nrow(dataekstra))
set.seed(777)

picked = sample(seq_len(nrow(dataekstra)),size = sample_size)
trainek =dataekstra[picked,]
testek =dataekstra[-picked,]

testmatrixx4<-as.matrix(testek[,1:36])

n_folds=10
myGridGlmnet <- expand.grid(alpha= 1, lambda = c(0.20,0.25,0.30,0.35,0.40))
fitControl=trainControl(method = "cv",
                        number = n_folds)                         
set.seed(1)
penalized_model4<- train(c37~.,trainek, method="glmnet",tuneGrid=myGridGlmnet ,trControl = fitControl)
penalized_model4


rf_grid=expand.grid(mtry = c(4,6,8),
                    splitrule = c("gini","extratrees"),
                    min.node.size = c(5))


rf_fit4=train(c37 ~ ., data = data.frame(trainek), 
              method = "ranger", 
              trControl = fitControl, num.trees=500,
              tuneGrid = rf_grid)

rf_fit4

tree_grid=expand.grid(cp = c(1:10)/1000)


tree_fit4=train(c37~ ., data =data.frame(trainek),
                method = "rpart", 
                trControl = fitControl, 
                tuneGrid = tree_grid,tuneLength = 1 )
tree_fit4


myGridGbm <- expand.grid(interaction.depth=c(1,3,5), n.trees = (1:3)*100,shrinkage=c(0.01,0.005,0.001), n.minobsinnode=10)


gbm_model4 = train(c37~.,data=data.frame(trainek), method = "gbm",tuneGrid= myGridGbm ,trControl = fitControl)

gbm_model4 

unique(testek$c37)
testek[,predpenalized:=predict(penalized_model4, testmatrixx4)]
testek[,predforest:=predict(rf_fit4, testmatrixx4)]
testek[,predtree:=predict(tree_fit4, testmatrixx4)]
testek[,predgbm:=predict(gbm_model4 , testmatrixx4)]

Test_Performances3 <- data.table(
  Glm_Test_ACC=mean(testek$c37==testek$predpenalized),
  Dt_Test_ACC =mean(testek$c37==testek$predtree) ,
  Forest_Test_ACC =mean(testek$c37==testek$predforest),
  SGBM_Test_ACC =mean(testek$c37==testek$predgbm)
)

Test_Performances3


##Best method is random forest for this data. Because it has the highest accuracy rate.The worst one is penalized regression.


dataci<-read.delim("dataclassimbalance.txt", sep = ",")

dataci<-na.omit(dataci)
dataci<-data.table(dataci)
dataci<-dataci[,-c(1,2,3,8,14,15,16)]
dataci$class<-as.character(as.numeric(dataci$class))



sample_size = floor(0.8*nrow(dataci))
set.seed(777)

# randomly split data in r
picked = sample(seq_len(nrow(dataci)),size = sample_size)
trainci =dataci[picked,]
testci =dataci[-picked,]

testmatrixx3<-as.matrix(testci[,1:11])

n_folds=10
myGridGlmnet <- expand.grid(alpha= 1, lambda = c(0.20,0.25,0.30,0.35,0.40))
fitControl=trainControl(method = "cv",
                        number = n_folds)                         
set.seed(1)
penalized_model3<- train(class~.,trainci, method="glmnet",tuneGrid=myGridGlmnet ,trControl = fitControl)
penalized_model3


rf_grid=expand.grid(mtry = c(4,6,8),
                    splitrule = c("gini","extratrees"),
                    min.node.size = c(5))


rf_fit3=train(class ~ ., data = data.frame(trainci), 
              method = "ranger", 
              trControl = fitControl, num.trees=500,
              tuneGrid = rf_grid)

rf_fit3




tree_grid=expand.grid(cp = c(1:10)/1000)


tree_fit3=train(class~ ., data =data.frame(trainci),
                method = "rpart", 
                trControl = fitControl, 
                tuneGrid = tree_grid,tuneLength = 1 )
tree_fit3





myGridGbm <- expand.grid(interaction.depth=c(1,3,5), n.trees = (1:3)*100,shrinkage=c(0.01,0.005,0.001), n.minobsinnode=10)


gbm_model3 = train(class~.,data=data.frame(trainci), method = "gbm",tuneGrid= myGridGbm ,trControl = fitControl)

gbm_model3 

unique(testci$class)

testci[,predpenalized:=predict(penalized_model3, testmatrixx3)]
testci[,predforest:=predict(rf_fit3, testmatrixx3)]
testci[,predgbm:=predict(gbm_model3 , testmatrixx3)]
testci[,predtree:=predict(tree_fit3, testmatrixx3)]


Test_Performances4 <- data.table(
  Glm_Test_ACC=mean(testci$class==testci$predpenalized),
  Dt_Test_ACC =mean(testci$class==testci$predtree) ,
  Forest_Test_ACC =mean(testci$class==testci$predforest),
  SGBM_Test_ACC =mean(testci$class==testci$predgbm)
)

Test_Performances4

##Best methods are penalized regression and gradient boosting for this data. Because it has the highest accuracy rate.The worst methods are decision tree and random forest.














