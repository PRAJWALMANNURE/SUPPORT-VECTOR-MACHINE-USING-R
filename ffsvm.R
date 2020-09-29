#loading the required libraries
library(kernlab) 
library(caret)
library(caTools)

#loading the data 
ff <- read.csv(choose.files())

#EDA
summary(ff)

ff <- data.frame(ff,size)
ff_svm <- (ff[,c(-1,-2)])

sum(is.na(ff))

#dividing data into trian and test
sample <- sample.split(ff_svm,SplitRatio = 0.75)

train <- subset(ff_svm,sample=='TRUE')
test <- subset(ff_svm,sample=='FALSE')

#CREATING THE MODEL
model <- ksvm(size_category~.,data=train,kernel = "vanilladot")
model

pred <- predict(model,test)
table(pred,test$size_category)
confusionMatrix(table(pred,test$size_category)) #Accuracy : 0.8819  

#using  kernel = rbfdot
model1 <- ksvm(size_category~.,data=train,kernel = "rbfdot")
model1

pred1 <- predict(model1,test)
table(pred1,test$size_category)
confusionMatrix(table(pred1,test$size_category)) #Accuracy : 0.7361 

# kernal besseldot
model2 <- ksvm(size_category~.,data=train,kernel = "besseldot")
model2

pred2 <- predict(model2,test)
table(pred2,test$size_category)
confusionMatrix(table(pred2,test$size_category)) #Accuracy : 0.5833 

# kernel polydot
model3 <- ksvm(size_category~.,data=train,kernel = "polydot")
model3

pred3 <- predict(model3,test)
table(pred3,test$size_category)
confusionMatrix(table(pred3,test$size_category)) #Accuracy : 0.8819 


# hence kernal polydot and kernal vanilladot are best, as there accuracies are 88.19.