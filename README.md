# Group_Ciao_Lab01
library(tidyverse)

bank <- read.csv("bank.csv")
data <- bank
data

goodtoloan <- bank %>% 
  filter(job != "unemployed", housing == "yes")
goodtoloan

jobVisual <- goodtoloan %>% 
  filter(loan == "yes") %>%
  ggplot( aes(x = marital, y=balance, fill = job) ) + 
  geom_bar(stat="identity") 
jobVisual


