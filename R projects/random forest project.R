library(ISLR)
head(College)
df <- College
library(ggplot2)
pl <- ggplot(df, aes(Room.Board, Grad.Rate)) + geom_point(aes(color=Private))
print(pl)
pl1 <- ggplot(df, aes(F.Undergrad)) + geom_histogram(aes(fill=Private), color='black') +theme_bw()
print(pl1)
pl2 <- ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private), color='black') 
print(pl2)
# we notice that there are colleges with graduation rate greater than 10 which is not possible
#so we change their graduation rate to 100
subset(df, Grad.Rate > 100)
df["Cazenovia College", "Grad.Rate"] <- 100
df["Cazenovia College",]
#splitting the data
library(caTools)
set.seed(101)
sample <- sample.split(df$Private, SplitRatio = 0.7)
df.train <- subset(df, sample == T)
df.test <- subset(df, sample ==F)
#creating regression tree
install.packages("rpart")
library(rpart)
tree <- rpart(Private~.,method='class', data= df.train)
predict.tree <- predict(tree, df.test)
head(predict.tree)
predict.tree <- as.data.frame(predict.tree)
fpredict <- function(x){
  if(x > 0.5){
    ans <- "No"
  }else{
    ans <- "Yes"
  }
  return(ans)
}

predict.tree$private <- sapply(predict.tree$No, fpredict)
head(predict.tree)
table(predict.tree$private, df.test$Private)
#plotting the decision tree
install.packages("rpart.plot")
library(rpart.plot)
prp(tree)
#RANDOM FORESTS
install.packages("randomForest")
library(randomForest)
set.seed(101)
rf.model <- randomForest(Private~., data= df.train, importance= T)
#to get the confusion matrix
rf.model$confusion
rf.model$importance
#predicting the test data
predict.rf <- predict(rf.model, df.test)
head(predict.rf)
table(predict.rf, df.test$Private)
