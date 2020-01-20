# Group_Ciao_Lab01
library(tidyverse)
library(ggplot2)

### Exercise 1a
mpg <- mpg
p1 <- ggplot(data = mpg,
                  mapping = aes(x = displ, y = hwy ))+ geom_point()
p1

p2 <- ggplot(data = mpg,
             mapping = aes(x = class, y = drv ))+ geom_point()
p2

### Exercise 1b
p <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy )) + geom_point(aes(color = class)) + theme(legend.position = "top")
p

### Exercise 2
bank <- read.csv("bank.csv")

MaritalVisual <- bank %>%
  ggplot( aes(x = job, fill = loan))+
  geom_bar(position = "fill")+
  labs( title = "ratio of loans for different jobs", y = "ratio") +
  coord_flip()
MaritalVisual


housingvsjob <- bank %>%
  ggplot( aes( x = housing, y = balance, fill = job))+
  geom_bar( stat = "identity") +
  labs( title = "Housing vs Jobs")
housingvsjob





