library('class')
library('caret')
library('e1071')

#reading the training dataset
train_df <- read.csv("C:/Users/Arvind/Desktop/3rd Quarter/Predictive Analytics/week 2/fashion_train.csv")
dim(train_df)

#naming the columns, the 1st column has lables and rest of the columns are pixels 
features <- c("label", sprintf("Pixel%02d", seq(1,784)))
colnames(train_df) <- features

#reading test data
testdf<-read.csv("C:/Users/Arvind/Desktop/3rd Quarter/Predictive Analytics/week 2/fashion_test.csv")
dim(testdf)

#naming columns of test data
features <- c("label", sprintf("Pixel%02d", seq(1,784)))
colnames(testdf) <- features


#selecting the observed labels for tain and test datasets 
train_target <- train_df[,1]
test_target <-as.factor(testdf[,1])

#creating a dataset without the labels to train the model
train <- train_df[,-1]
test <- testdf[,-1]

#Training the model for k=1 and getting its confusion matrix
knn1 <- as.factor(knn(train, test, train_target, k=1))
test$prediction <- knn1
confusionMatrix(test_target, knn1)

#Training the model for k=11 and getting its confusion matrix
knn11 <- as.factor(knn(train, test, train_target, k=11))
test$prediction11 <- knn11
confusionMatrix(test_target, knn11)

#Training the model for k=21 and getting its confusion matrix
knn21 <- as.factor(knn(train, test, train_target, k=21))
test$prediction21 <- knn21
confusionMatrix(test_target, knn21)

#Creating a datset with the predicted values of all the models
x<-as.data.frame(test_target)
x[,2:4] <- test[, c( "prediction", "prediction11", "prediction21")]

View(x)





