adult <- read.csv("adult_sal.csv")
str(adult)
head(adult)
library(dplyr)
adult <- select(adult, -X)
head(adult)
str(adult)
table(adult$type_employer)
adult$type_employer<- as.character(adult$type_employer)
change <- function(x)
{
  type <- x
  if(x=="Never-worked"){
    type <- "Unemployed"
  }else if (type == "Without-pay"){
    type <- "Unemployed"
  }else if (x== "Local-gov"){
    type <- "SL gov"
  } else if (x== "State-gov"){
    type <- "SL gov"
  }else if( x== "Self-emp-inc"){
    type <- "Self emp"
  }else if(x== "Self-emp-not-inc"){
    type <- "Self emp"
  }else{
    type <- type
  }
  return(type)
}
adult$type_employer <- sapply(adult$type_employer,change)
adult$type_employer<- as.factor(adult$type_employer)
table(adult$type_employer)
table(adult$marital)
adult$marital <- as.character(adult$marital)
mari <- function(status){
  k <- status
  if(status == "Divorced" | status == "Separated" | status == "Widowed"){
    k <- "Not-married"
  }else if(status== "Married-AF-spouse" | status =="Married-civ-spouse" | status == "Married-spouse-absent"){
    k <- "Married" 
  }else{
    k <- k
  }
  return(k)
}
adult$marital <- sapply(adult$marital, mari)
table(adult$marital)
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')
group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country,group_country)
table(adult$country)
install.packages("Amelia")
library(Amelia)
adult[adult == '?']<- NA
table(adult$type_employer)
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)
missmap(adult, main="missing map", col=c("yellow","black"), legend =F)
na.omit(adult)
adult<- na.omit(adult)
missmap(adult, main="missing map", col=c("yellow","black"), legend =F)
str(adult)
library(ggplot2)
pl <- ggplot(adult, aes(age))
pl <- pl + geom_histogram(aes(fill=income), color="black", binwidth=1)
print(pl)
pl1 <- ggplot(adult, aes(hr_per_week))+ geom_histogram(binwidth = 5, bins= 30) 
print(pl1)
pl2 <- ggplot(adult, aes(country)) + geom_bar(aes(fill=income), color="black")
print(pl2)
install.packages("caTools")
library(caTools)
sample <- sample.split(adult$income,SplitRatio = 0.7)
adult.train <- subset(adult, sample==T)
adult.test <- subset(adult, sample==F)
logmodel <- glm(income~.,family=binomial(logit), data= adult.train)
summary(logmodel)
new.logmodel <- step(logmodel)
adult.test$predicted.income <- predict(new.logmodel, newdata = adult.test, type= "response")
table(adult.test$income, adult.test$predicted.income > 0.5) #this will give the confusion matrix
