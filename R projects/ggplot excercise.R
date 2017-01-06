library(ggplot2)

library(ggthemes)
pl <- ggplot(mpg, aes(x=hwy))
pl + geom_histogram(fill='orange')
#question 2:
pl1<- ggplot(mpg, aes(x=manufacturer))
pl1 + geom_bar(aes(fill=factor(cyl)))
#question 3:
head(txhousing)
pl2<- ggplot(txhousing, aes(x=sales, y= volume))
pl2 + geom_point(color='green', alpha= 0.8)
#question 4:
pl2 + geom_point(color='green', alpha= 0.8) + geom_smooth(color= 'black')
