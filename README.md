# Group_Ciao_Lab01
library(tidyverse)
library(ggplot2)

mpg <- mpg

p <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy )) +
            geom_point(aes(color = manufacturer))+ 
            theme(legend.position = "top")
p

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





