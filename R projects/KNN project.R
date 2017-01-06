## KNN project
install.packages("ISLR")
library(ISLR)
head(iris)
table(iris$Species)
str(iris)
var(iris$Sepal.Length)
var(iris$Sepal.Width)
scaled.iris <- scale(iris[,-5])
head(scaled.iris)
var(scaled.iris[,1])
var(scaled.iris[,2])
head(scaled.iris)
new.iris <- data.frame(scaled.iris, iris[,5])
head(new.iris)
colnames(new.iris)[5] <- "species"
head(new.iris)

##splitting the data
library(caTools)
set.seed(101)
sample <- sample.split(new.iris$species,SplitRatio = 0.7)
iris.train <- subset(new.iris, sample==T)
iris.test <- subset(new.iris, sample==F)
species.train <- iris.train$species
species.test <- iris.test$species
iris.train <- iris.train[,-5]
iris.test <- iris.test[,-5]
head(iris.train)
head(iris.test)

##the KNN model
library(class)
set.seed(101)
predicted.species <- knn(iris.train,iris.test,species.train, k=1)
mserror <- mean(species.test!= predicted.species)
print(mserror)
predicted.species <- NULL
mserror <- NULL
 for(i in 1:15){
   set.seed(101)
   predicted.species <- knn(iris.train,iris.test,species.train, k=i)
   mserror[i]<- mean(species.test != predicted.species)
 }
k <- 1:15
knn.df <- data.frame(k,mserror)
head(knn.df)

## the elbow K
library(ggplot2)
pl <- ggplot(knn.df,aes(k,mserror))+ geom_point()+geom_line(color="red")
print(pl)
