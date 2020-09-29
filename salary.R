#loading the required libraries
library(kernlab)
library(caret)
library(caTools)

# load the data set
salary_train <- read.csv(choose.files())
salary_test <- read.csv(choose.files())

#eda
summary(salary_train)
summary(salary_test)

sum(is.na(salary_train))
sum(is.na(salary_test))

salary <- ifelse(salary_train$Salary== ' >50K','high','low')
salar <- ifelse(salary_test$Salary== ' >50K','high','low')

salary_train <- cbind(salary_train,salary)
salary_test <- cbind(salary_test,salar)

salary_train$workclass <- as.numeric(salary_train$workclass)
salary_train$education <- as.numeric(salary_train$education)

salary_test$workclass <- as.numeric(salary_test$workclass)
salary_test$education <- as.numeric(salary_test$education)

#dividing data into train and test
trian_svm <- (salary_train[,c(-(5:9),-(12:14))])
test_svm <- (salary_test[,c(-(5:9),-(12:14))])

# model creation
model <- ksvm(salary~.,data=trian_svm,kernal="vanilladot")
model

pred <- predict(model,test_svm)
confusionMatrix(table(pred,test_svm$salar)) #Accuracy : 0.8183 

## kernel = rbfdot 
model1 <- ksvm(salary~.,data=trian_svm,kernal="rbfdot")
model1

pred1 <- predict(model1,test_svm)
confusionMatrix(table(pred1,test_svm$salar)) #Accuracy : 0.8179

# kernal = besseldot
model2 <- ksvm(salary~.,data=trian_svm,kernal="besseldot")
model2

pred2 <- predict(model2,test_svm)
confusionMatrix(table(pred2,test_svm$salar)) # Accuracy : 0.8183 

# kernel = polydot
model3 <- ksvm(salary~.,data=trian_svm,kernal="polydot")
model3

pred3 <- predict(model3,test_svm)
confusionMatrix(table(pred3,test_svm$salar)) #  Accuracy : 0.8179 
