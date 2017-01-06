note <- read.csv('bank_note_data.csv')
head(note)
str(note)
#EDA
#library(ggplot2)
#note$Class <- factor(note$Class)
#pl <- ggplot(note, aes(Entropy)) + geom_histogram(aes(fill= Class) , color='black')
#print(pl)
#pl <- ggplot(note, aes(Image.Skew)) + geom_histogram(aes(fill= Class) , color='black')
#pl1 <- ggplot(note, aes(Image.Var, Image.Skew)) + geom_point(aes(color=Class), alpha=0.5)
#print(pl1)
#splitting the data
library(caTools)
set.seed(101)
sample <- sample.split(note$Class , 0.7)
note.train <- subset(note, sample==T)
note.test <- subset(note, sample==F)
#neural net
#install.packages('neuralnet')
library(neuralnet)
note.train$Class <- as.integer(note.train$Class) # Neural net only accepts numeric input
note.test$Class <- as.integer(note.test$Class) # Neural net only accepts numeric input
set.seed((101))
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, note.train, hidden = 10, linear.output = F)
pred.nn <- compute(nn, note.test[, 1:4])  
head(pred.nn$net.result)
predictions <- sapply(pred.nn$net.result, round)
head(predictions)
#confusion matrix
table(predictions, note.test$Class)
#compaing with random forests
library(randomForest)
note$Class <- factor(df$Class)
library(caTools)
set.seed(101)
split = sample.split(note$Class, SplitRatio = 0.70)

train = subset(note, split == TRUE)
test = subset(note, split == FALSE)
model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)
rf.pred <- predict(model,test)
table(rf.pred,test$Class)
