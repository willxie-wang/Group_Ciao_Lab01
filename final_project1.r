library(readxl)

#load the data

data<-read_excel("/Users/wangpengxie/Downloads/总数据.xlsx")

#first problem

library(ggplot2)

ggplot(data = data) +
  geom_line( aes(x = Year, y = GDP_growth_rate, colour = "GDP_growth_rate"), size = 1) +
  geom_line( aes(x = Year, y = Industry_ratio, colour = "Industry_ratio"), size = 1) +
  geom_line( aes(x = Year, y = manufacturing_ratio, colour = "manufacturing_ratio"), size = 1) +
  geom_line( aes(x = Year, y = Agriculture.Fisheries.Forestry_ratio, colour = "Agriculture.Fisheries.Forestry_ratio"), size = 1)+
  scale_colour_manual("", values = c("GDP_growth_rate" = "red", "Industry_ratio" = "black", "manufacturing_ratio" = "blue", "Agriculture.Fisheries.Forestry_ratio" = "green"))+
  ylab("percentage")


ggplot(data = data) +
  geom_line( aes(x = Year, y = Total_GDP, colour = "Total_GDP"), size = 1) +
  geom_line( aes(x = Year, y = Industry_value, colour = "Industry_value"), size = 1) +
  scale_colour_manual("", values = c("Total_GDP" = "red","Industry_value" = "blue"))+
  ylab("percentage")

