#####Team_12_Ciao#####

###Quention 1 
ameslist <- read.csv("ames.csv", header=T, sep=",")
Ames <- ameslist[, c("LotFrontage", "LotArea", "OverallQual", "OverallCond", "YearBuilt", "MasVnrArea",
                     "GrLivArea", "GarageYrBlt", "GarageArea", "GarageCars", "SalePrice", "YrSold", "Fireplaces", "PoolArea")]

#Quention 2
df <- Ames[, c("LotFrontage", "LotArea", "OverallQual", "OverallCond", "MasVnrArea",
               "GrLivArea", "GarageYrBlt", "GarageArea", "GarageCars", "SalePrice", "Fireplaces", "PoolArea")]
df <- na.omit(df)
pairs(df)

###Quention 3
cor(df)
#We can see from the correlation table, the SalePrice is positive associated 
#LotFrontage, LotArea, OverallQual, MasVnrArea, GrLivArea, GarageYrBlt, GarageArea,
#GarageCars, Fireplaces, PoolArea, and negative associated with OverallCond. 
#It match the prior beliefs.

###Question 4 
plot(SalePrice~GrLivArea, Ames)
fit <- lm(SalePrice~GrLivArea, Ames)
abline(fit)
max(Ames$SalePrice)
Ames$GrLivArea[Ames$SalePrice==max(Ames$SalePrice)]
ameslist[ameslist$SalePrice==max(Ames$SalePrice),]
#The largest outlier is (4316, 755000).

###Question 5 (Bonus)
barplot(table(ameslist$CentralAir), main="Central Air condition count")

rm(list=ls())
setwd("C:/Users/Administrator/Desktop/2020-1-29")
ameslist=read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                    header = TRUE,
                    sep = ",")

# Missing and identical data
listname1=c("PoolArea","ScreenPorch","LowQualFinSF","BsmtFinSF2","CentralAir","Electrical","Functional","SaleType","PoolQC","Fence",
            "MiscFeature","FireplaceQu","KitchenAbvGr","KitchenQual",
            "GarageCond","GarageQual","PavedDrive")

listname2=c("Id","MSZoning","Street","Alley","LotShape",
            "LandContour","Utilities","LotConfig","LandSlope",
            "Neighborhood","Condition1","Condition2","BldgType","HouseStyle")
listname3=c("RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType",
            "ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond")
listname4=c("GarageType","BsmtExposure"," BsmtFinType1","BsmtFinType2",
            "Heating","GarageFinish","SaleCondition","BsmtFinType1","HeatingQC")
listname=c(listname1,listname2,listname3,listname4)
ameslist=ameslist[,-which(names(ameslist)%in%listname)]
ameslist=na.omit(ameslist)
# View(ameslist)
ameslist=as.matrix(ameslist)
ameslist=apply(ameslist,2,as.numeric)
ameslist=as.data.frame(ameslist)

# Exercise 2:
# 1
fit1=lm(SalePrice~GrLivArea,data=ameslist)
summary(fit1)
fit=lm(SalePrice~YearBuilt,data=ameslist)
summary(fit)
# 2
name=colnames(ameslist[,-which(names(ameslist)%in%c("SalePrice"))])
Rhs=paste(name,collapse = "+")
Lhs="SalePrice"
myFormula=paste(Lhs,Rhs,sep = "~")
myFormula=as.formula(myFormula)
fit2=lm(myFormula,data=ameslist)
fit2=lm(SalePrice~.,data=ameslist)
summary(fit2)
# (1) yes
# (2)
# MSSubClass,LotFrontage,LotArea,OverallQual,OverallCond,YearBuilt,MasVnrArea
# BedroomAbvGr,TotRmsAbvGrd,Fireplaces,GarageCars,WoodDeckSF
# (3)
# For each year of growth, sales increased by -264.4
# 3
par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))
# The residuals basically follow the normal hypothesis(except for 10,409,998)
# 409 is the strong point.
# 4
fit3=lm(SalePrice~.+LotArea*LotFrontage+LotArea:LotFrontage,data=ameslist)
summary(fit3)
# The coefficient of the interaction term is significant, indicating that it is meaningful
# 5
# $ ln(x)
data1=ameslist
data1$LotArea=log(data1$LotArea)
data1$X1stFlrSF=log(data1$X1stFlrSF)
data1$GrLivArea=log(data1$GrLivArea)
ln.fit=lm(myFormula,data=data1)
summary(ln.fit)
# The three variables were transformed, and the results of the output model showed that the three variables passed the significance test

# $ x^2
data2=ameslist
data2$LotArea=(data2$LotArea)^2
data2$X1stFlrSF=(data2$X1stFlrSF)^2
data2$GrLivArea=(data2$GrLivArea)^2
fit.2=lm(myFormula,data=data2)
summary(fit.2)
# The three variables were transformed, and the results of the output model showed that the three variables passed the significance test
# $ sqrt(x)
data3=ameslist
data3$LotArea=sqrt(data3$LotArea)
data3$X1stFlrSF=sqrt(data3$X1stFlrSF)
data3$GrLivArea=sqrt(data3$GrLivArea)
sqrt.fit=lm(myFormula,data=data3)
summary(sqrt.fit)
# The three variables were transformed, and the results of the output model showed that the three variables passed the significance test
# Bonus
# stepwise regression analysis 
data5=ameslist
data5$YrSold=sqrt(data5$YrSold)
data5$GrLivArea=log(data5$GrLivArea)
fit5=lm(SalePrice~.+LotArea*LotFrontage+LotArea:LotFrontage,data=data5)
summary(fit5)
par(mfrow=c(2,2))
plot(fit5)
par(mfrow=c(1,1))
fit5=lm(SalePrice~.+LotArea*LotFrontage+LotArea:LotFrontage,data=data5[-c(409,998,242),])
# install.packages("MASS")
library(MASS)
fit6=stepAIC(fit5)
summary(fit6)


