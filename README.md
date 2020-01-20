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
data <- bank

goodtoloan <- data %>% 
  filter(job != "unemployed", housing == "yes") %>%
  select(loan, job, marital, balance)
goodtoloan

jobVisual <- goodtoloan %>% 
  ggplot( aes(x = marital, fill = loan) ) + 
  geom_bar( position ="fill"") 
jobVisual

housingvsjob <- data %>%
  filter(loan == "yes") %>%
  ggplot( aes( x = housing, y = balance, fill = job))+
  geom_bar( stat = "identity")
housingvsjob



