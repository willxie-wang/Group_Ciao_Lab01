# Group_12_Ciao
library(tidyverse)
library(ggplot2)
library(plot3D)

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


# In Class Excercise 1/23

bank <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/bank.csv",
                   header = TRUE,
                   sep = ",")
bank

names(bank)

nullbalance = lm(balance ~ age + duration, data = bank)
fullbalance = lm(balance ~ age + duration +campaign + previous, data = bank)
anova(nullbalance, fullbalance)

hhh <- cbind(nullbalance, fullbalance)
hhh

x = bank$age
y = bank$duration
z = bank$balance

fit <- lm(z ~ x + y)

grid.lines = 25
x.pred     = seq(min(x), max(x), length.out = grid.lines)
y.pred     = seq(min(y), max(y), length.out = grid.lines)
xy         = expand.grid(x = x.pred, y = y.pred)

z.pred = matrix(predict(fit, newdata = xy),
                nrow = grid.lines, ncol = grid.lines)

fitpoints = predict(fit)

scatter3D(x, y, z, pch = 19, cex = 2, col = gg.col(1000), lighting = TRUE,
          theta = 25, phi = 45, ticktype = "detailed",
          xlab = "age", ylab = "duration", zlab = "mpg", zlim = c(0, 40), clim = c(0, 40),
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA, fit = fitpoints), main = "")





