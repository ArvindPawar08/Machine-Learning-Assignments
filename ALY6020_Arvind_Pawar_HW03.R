library(caret)   # an aggregator package for performing many machine learning models
library(rpart)   

#sequence generation 
x <- seq(-10, 10, 0.01)
y <-  sin(x) + 5*cos(2*x) - 3*sin(3*x) + (1-exp(-x/3) + 25)
df <- as.data.frame(x)
df[,2] <- as.data.frame(y)
head(df)

#let us check the generated values
df
dim(df)
tail(df)

plot(df$x, df$y, type="line", col="red") #plotting the original function

s <- c() #Creating empty list

alpha = 0.1 #setting the alpha value

tree <- rpart(df$y~df$x, data = df) #creating first tree
df$pred <- predict(tree, data = df$x) #first prediction
p<-df$pred                           #storing the first prediction in variable p
abs_res <- sum(abs(df$y - df$pred))  #calculating sum of residual
s <- c(s,abs_res)  #vector to save error values


p2 <- data.frame() #creating empty list

#Implementing Gradient Boosting

while(abs_res > 10){
  df$res <- df$y - df$pred #calculating residuals
  tree1 <- rpart(df$res~df$x, data = df, control = rpart.control(cp = 0.00000001)) #using above calculated residuals for training the model(growing new trees)
  df$pred1 <- predict(tree1, data = df$x) #new predictions
  df$pred <- df$pred + alpha*df$pred1 #adding new predictions to previous model using learning rate to improve the model
  abs_res<-sum(abs(df$y - df$pred)) #Calculating sum of residuals by taking sum of substituting updated predicted values from our actual observed values
  s <- c(s, abs_res) #creating vector for storing absolute error values in each iteration
  p2 <- rbind(p2,df$pred) #storing predicted values in each iteration so that I can access values of any iteration while visualization

}
s


plot(df$x, df$y, type = 'line', col="Red", lwd=4, xlab = "x", ylab = "y") #plotting the original function
lines(df$x, p2[5,], col="purple", lwd=3)  #plotting the different level (iteration) predicted values
lines(df$x, p2[550,], col="black", lwd=2)  #plotting the different level (iteration) predicted values
#legend and title of the plot
legend("bottomright", legend=c("Original Function","First Accuracy", "Accuracy at Different Level"), lwd=c(2,2), col=c("red","green", "purple"), cex = 0.60)
title(main='Original Function and Different Levels of Accuracies', cex.main = 1,   font.main= 4, col.main= "black", outer = FALSE)