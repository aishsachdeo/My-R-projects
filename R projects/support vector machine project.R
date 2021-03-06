loans <- read.csv("loan_data.csv")
summary(loans)
str(loans)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
str(loans)
loans$delinq.2yrs <- factor (loans$delinq.2yrs)
loans$pub.rec <- factor (loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)
str(loans)
#EDA
library(ggplot2)
pl <- ggplot(loans, aes(fico)) + geom_histogram(aes(fill=not.fully.paid), color='black', binwidth = 5)
print(pl)
pl1 <- ggplot(loans, aes(purpose)) + geom_bar(aes(fill=not.fully.paid), position='dodge') + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(pl1)
pl2 <- ggplot(loans, aes(int.rate, fico)) + geom_point(aes(color= not.fully.paid), alpha=0.5)
print(pl2)
#splitting the data
library(caTools)
sample <- sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loans.train <- subset(loans, sample==T)
loans.test <- subset(loans, sample==F)
#using the support vector mahines:
install.packages("e1071")
library(e1071)
model <- svm(not.fully.paid~. , data= loans.train)
summary(model)
predicted.values <- predict(model, loans.test[1:13])
table(predicted.values, loans.test$not.fully.paid)
#tunin the ost and gamma:
tune.results <- tune(svm, train.x = not.fully.paid~., data=loans.train, kernel='radial',ranges=list(cost=c(0.5,1,2),gamma=c(0.5,1,2)))
summary(tune.results)
