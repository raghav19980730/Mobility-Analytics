library(caTools)
library(xgboost)
library(tidyverse)
library(caret)
library(car)
setwd("C:/Users/ragha/Desktop/Raghav/Practice Projects/Mobility Analytics")

data <- read.csv('train_Wc8LBpr.csv', na.strings = '')
data <- data[-1]
(colSums(is.na(data))/nrow(data))*100

prop.table(table(data$Surge_Pricing_Type))
str(data)

#data[is.na(data$Type_of_Cab),'Type_of_Cab'] <- 'B'
#data[is.na(data$Confidence_Life_Style_Index),'Confidence_Life_Style_Index'] <- 'B'

#for(i in names(data[c(3,4,9)])){
 # data[is.na(data[,i]),i] <- mean(data[,i],na.rm =T)
#}

data[,c(2,5,6,12)] <- lapply(data[,c(2,5,6,12)],as.numeric)

#Splitting
split <- sample.split(data$Surge_Pricing_Type,SplitRatio = 0.75)
train <- data[split == T,]
train$Surge_Pricing_Type <- as.numeric(train$Surge_Pricing_Type) - 1
test <- data[split == F,]
test$Surge_Pricing_Type <- as.numeric(test$Surge_Pricing_Type) - 1

x_train <- as.matrix(train[,-13])
y_train <- as.matrix(train[,13])
x_test <- as.matrix(test[,-13])
y_test <- as.matrix(test[,13])

accuracy = list()
acc = list()

params = list(eta = 0.1, gamma = 0.8,max_depth = 6, min_child_weight = 10,alpha = 0.2,
              colsample_bytree =1, objective = 'multi:softmax',num_class =3,early_stopping_rounds = 10)
model <- xgboost(x_train,y_train,params = params,nrounds = 150)

y_pred = predict(model,x_train)
confusionMatrix(as.factor(y_pred),as.factor(y_train))

y_pred = predict(model,x_test)
confusionMatrix(as.factor(y_pred),as.factor(y_test))



y_pred = predict(model,x_train)
confusionMatrix(as.factor(y_pred),as.factor(y_train))

y_pred = predict(model,x_test)
confusionMatrix(as.factor(y_pred),as.factor(y_test))


#Final Submission

testing_set <- read.csv('test_VsU9xXK.csv',na.strings = "")
testing_set <- testing_set[,-1]
testing_set[,c(2,5,6,12)] <- lapply(testing_set[,c(2,5,6,12)],as.numeric)


y_pred = predict(model,as.matrix(testing_set))
final_pred = y_pred + 1
final_pred <- as.data.frame(final_pred)

sample <- read.csv('sample_submission_NoPBkjr.csv')
final_result = data.frame(sample[1],final_pred)
colnames(final_result) <- c('Trip_ID','Surge_Pricing_Type')
write.csv(final_result,"C:\\Users\\ragha\\Desktop\\Raghav\\Practice Projects\\Mobility Analytics\\Final_Submission_2.csv",row.names = F)

