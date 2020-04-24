library(readxl)

#load the data

data<-read_excel("/Users/wangpengxie/Downloads/final_data.xlsx")

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

#second problem

ggplot(data = data) +
  geom_line( aes(x = Year, y = GDP_growth_rate), size = 1)

ggplot(data = data) +
  geom_line( aes(x = Year, y = Total_GDP), size = 1)



#Third problem

ratio<-data[,c("Industry_ratio","manufacturing_ratio","Agriculture.Fisheries.Forestry_ratio")]

summary(ratio)

mean_ratio<-c(mean(ratio$Industry_ratio),mean(ratio$manufacturing_ratio,na.rm=TRUE),mean(ratio$Agriculture.Fisheries.Forestry_ratio))
mean_ratio<-data.frame(
  vb=c("Industry","manufacturing","Agriculture.Fisheries.Forestry"),
  ratio=mean_ratio
)

ggplot(data = mean_ratio,mapping=aes(x=vb,y=ratio)) +
  geom_bar(stat="identity",width=0.5,fill="#DD8888")+
  xlab("")


#fourth problem

ggplot(data = data) +
  geom_line( aes(x = Year, y = Total_GDP, colour = "Total_GDP"), size = 1) +
  geom_line( aes(x = Year, y = Industry_value, colour = "Industry_value"), size = 1) +
  geom_line( aes(x = Year, y = manufacturing_value, colour = "manufacturing_value"), size = 1) +
  geom_line( aes(x = Year, y =Agriculture.Fisheries.Forestry_value, colour = " Agriculture.Fisheries.Forestry_value"), size = 1)+
  ylab("value")



# fifth problem

data_mode1<-data[data$Year<2004,]
mode1<-lm(Total_GDP~Industry_value+Agriculture.Fisheries.Forestry_value,data=data_mode1)
summary(mode1)

data_mode2<-data[data$Year>=2004,]
mode2<-lm(Total_GDP~Industry_value+manufacturing_value+Agriculture.Fisheries.Forestry_value,data=data_mode2)
summary(mode2)