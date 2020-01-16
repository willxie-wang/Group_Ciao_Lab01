# Group_Ciao_Lab01
library(tidyverse)
library(ggplot2)
mpg
p <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy))
p + geom_point()

