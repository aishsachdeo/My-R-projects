df1 <- read.csv("winequality-red.csv", sep = ';')
df2 <- read.csv("winequality-white.csv", sep=';')
head(df1)
df1$label <- "red"
df2$label <- "white"
head(df2)
wine <- rbind(df1, df2)
str(wine)
#EDA
library(ggplot2)
pl <- ggplot(wine, aes(residual.sugar)) + geom_histogram(aes(fill= label), color='black', binwidth = 1) + scale_fill_manual(values=c("red", "white"))
print(pl)
pl1 <- ggplot(wine, aes(citric.acid)) + geom_histogram(aes(fill=label),color='black', bins=40) + scale_fill_manual(values=c("red", "white"))
print(pl1)
pl2 <- ggplot(wine, aes(alcohol)) + geom_histogram(aes(fill=label), color= 'black', bins= 40)  + scale_fill_manual(values=c("red", "white")) 
print(pl2)
pl3 <- ggplot(wine, aes(citric.acid, residual.sugar)) + geom_point(aes(color=label), alpha=0.3)+ scale_color_manual(values = c("red",'white')) + theme_dark()
print(pl3)
pl4 <- ggplot(wine, aes(residual.sugar, volatile.acidity)) + geom_point(aes(color=label), alpha=0.3)+ scale_color_manual(values = c("red",'white')) + theme_dark()
print(pl4)
#data without the labels
clus.data <- wine[,-13]
head(clus.data)
str(clus.data)
#clustering the data
wine.cluster <- kmeans(clus.data, 2, nstart=20)
print(wine.cluster)
summary(wine.cluster)
#checking our classification (usually wont have this luxury)
table(wine.cluster$cluster, wine$label)
