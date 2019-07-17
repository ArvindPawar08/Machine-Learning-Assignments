#load the required library
library(ISLR)
library(caret)

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

#let us visualize the data and see how our images looks like
rotate <- function(x) {
  return(t(apply(x, 2, rev)))
}

plot_matrix <- function(vec) {
  q <- matrix(vec, 28, 28, byrow = TRUE)
  nq <- apply(q, 2, as.numeric)
  image(rotate(nq), col = gray((0:255)/255))
}

#just have a look how clothing items looks: display 8 images
par(mfrow=c(2,4))
plot_matrix(train_df[102, 2:785])
plot_matrix(train_df[103, 2:785])
plot_matrix(train_df[104, 2:785])
plot_matrix(train_df[105, 2:785])
plot_matrix(train_df[106, 2:785])
plot_matrix(train_df[107, 2:785])
plot_matrix(train_df[108, 2:785])
plot_matrix(train_df[109, 2:785])


#function for sampling and relabeling for each clothing item, encoded as digit(0:9) 
#function will return randomly sampled and relabeled dataframe for each digit/clothing item
sampledata<-function(digit, train_df, size=20000){
  sample_df<-train_df[sample(nrow(train_df), size=20000, replace = FALSE),]
  sample_df$label <- ifelse(sample_df$label== digit, 1, 0)
  return(sample_df)
}

#training models using logistic regression binomial classifier

set.seed(1234)

#creating empty list to store 10 models per clothing item
model_list=list()

#for loop to train every model on sampled and relabeled datasets per clothing item by calling sampledata function
for(digit in c(0:9)){
  sample_df=sampledata(digit, train_df, 20000)
  model_list[[digit+1]]=glm(data= sample_df, label~., family = binomial(link = "logit"))
  print(digit)
}

#predicting probabilities of all clothing items in test dataset using predict function
for (digit in c(0:9))
{
  predicted_prob[[digit+1]]<-predict(model_list[[digit+1]], testdf, type = "response")
}

#creating predicted probabilities dataframe from predicted_prob list
predicted_prob_df <- as.data.frame(predicted_prob)

#predicted label based on the probabilities predicted by model
predicted_label <- apply(X = predicted_prob_df,MARGIN = 1,FUN = which.max)-1

#confusion matrix
conf.t <- table(predicted_label, testdf$label)
conf.t
#accuracy
sum(diag(conf.t))/sum(conf.t)
