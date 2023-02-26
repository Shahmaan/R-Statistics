install.packages("ggforce")

library(ggplot2)
library(psych)
library(tidyverse)
library(dplyr)
library(ggforce)

set.seed(100)
iteration <- 1000

x <- runif(iteration, 0, 1)
y <- runif(iteration, 0, 1)
z <- sqrt(x^2 + y^2)
d <- sqrt(x^2 + y^2 <=1)

df <- data.frame(cbind(x, y, z))

df <- mutate(df, sets = case_when (
  z < 0.4 ~ 'group 1',
  z < 0.8 ~ 'group 2',
  z < 1 ~ 'group 3'))

arc <- data.frame(i = c(0.4, 0.8, 1))
pi_apox <- pi - (4 * d / iteration)

plot <- ggplot(df, aes(x = x, y = y, color = z)) + 
  geom_point(size = 1) +
  labs(x = "X-value", y = "Y-value") +
  print(labs(colour = "Color"))

arc_sets <- data.frame(cb = c(0.4, 0.8, 1))
points_within_circle <- sqrt(x^2 + y^2) <= 1
pi_apox <- pi - (4*(points_within_circle/iterations))

plot <- ggplot() + geom_point(data = df, aes(x = x, y = y, color = sets)) +
  scale_color_manual(values = c("yellow", "red", "blue", "grey")) +
  geom_arc(data = arc_sets, aes(x0 = 0, y0 = 0, r = cb, start = 0,
                                end = 0.5 * pi, size = 1))
plot
