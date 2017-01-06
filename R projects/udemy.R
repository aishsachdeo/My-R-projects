
head(select(mtcars, mpg, hp))
distinct(mtcars, gear)
head(mutate(mtcars, Performance = hp/wt))
summarise(mtcars, avg_mpg = mean(mpg))
mtcars %>% filter(cyl == 6) %>% summarise(std_hp=mean(hp))
