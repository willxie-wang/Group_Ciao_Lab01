# Group_Ciao_Lab01
library(tidyverse)

bank <- read.csv("bank.csv")
data <- bank

housingvsjob <- data %>%
  ggplot( aes( x = job, y = "balance", fill = "housing"))+
  geom_bar( stat = "identity")
housingvsjob

goodtoloan <- data %>% 
  filter(job != "unemployed", housing == "yes") %>%
  select(loan, job, marital, balance)
goodtoloan

jobVisual <- goodtoloan %>% 
  filter( loan == "yes") %>%
  ggplot( aes(x = marital, y=balance, fill = job) ) + 
  geom_bar(stat="identity") 
jobVisual




