rm(list=ls())

library(readr)
train_data=read.csv("train.csv")
test_data=read.csv("test.csv")
train_data=train_data[,c(2,3,5,6,7,8,12)]
test_data=test_data[,c(2,4,5,6,7,11)]
train_data$Sex=factor(train_data$Sex,levels=c('male','female'),labels=c(0,1))
test_data$Sex=factor(test_data$Sex,levels=c('male','female'),labels=c(0,1))
train_data$Age=ifelse(is.na(train_data$Age),ave(train_data$Age, FUN = function(x) mean(x, na.rm=T)),train_data$Age)
test_data$Age=ifelse(is.na(test_data$Age),ave(test_data$Age, FUN = function(x) mean(x, na.rm=T)),test_data$Age)
train_data$Embarked=factor(train_data$Embarked,levels=c('Q','S','C'),labels=c(1,2,3))
test_data$Embarked=factor(test_data$Embarked,levels=c('Q','S','C'),labels=c(1,2,3))
train_data$Age=scale(train_data$Age)
test_data$Age=scale(test_data$Age)
classifier=glm(formula=Survived~.,family=binomial,data=train_data)
prob_pred=predict(classifier,type='response',newdata=test_data)
y_pred=ifelse(prob_pred > 0.5, 1, 0)
survived_pred=as.data.frame(y_pred)

