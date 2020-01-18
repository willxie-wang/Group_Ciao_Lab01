# Group_Ciao_Lab01
library(tidyverse)

bank <- read.csv("bank.csv")
data <- bank
data

goodtoloan <- data %>% 
  filter(job != "unemployed", housing == "yes") %>%
  select(loan, job, marital, balance)
goodtoloan

jobVisual <- goodtoloan %>% 
  filter( loan == "yes") %>%
  ggplot( aes(x = marital, y=balance, fill = job) ) + 
  geom_bar(stat="identity") 
jobVisual


#jobVisual <- goodtoloan %>% 
#  filter( loan == "yes") %>%
#  ggplot( aes(x = loan, y=balance, fill = job) ) + 
#  geom_bar(stat="identity") 
#jobVisual


