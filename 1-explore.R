library(dplyr)
library(ggplot2)

cocaine <- tbl_df(read.csv("cocaine.csv", stringsAsFactors = FALSE))

# Univariate distributions
qplot(weight, data = cocaine, binwidth = 10)
qplot(potency, data = cocaine, binwidth = 1)
qplot(price, data = cocaine, binwidth = 100)
qplot(state, data = cocaine)
qplot(factor(month), data = cocaine)

# What predicts price?
qplot(weight, price, data = cocaine)
qplot(weight, price, data = cocaine, colour = potency)
qplot(price / weight, data = cocaine, binwidth = 10)

qplot(potency, price, data = cocaine)
qplot(potency, price / weight, data = cocaine)

# Look at top all states with over 200 seizures
top6 <- cocaine %.% group_by(state) %.% tally(sort = T) %.% filter(n > 200)
cocaine2 <- cocaine %.% semi_join(top6)

# Group by month
by_month <- cocaine2 %.% 
  group_by(state, month) %.%
  summarise(n = n(), price = sum(price), weight = sum(weight))

qplot(month, n, data = by_month, geom = "line", colour = state)
qplot(month, weight, data = by_month, geom = "line", colour = state)
qplot(month, weight / n, data = by_month, geom = "line", colour = state)
qplot(month, weight / n, data = by_month, geom = "smooth", colour = state, 
  method = "lm", se = F)
qplot(month, price, data = by_month, geom = "line", colour = state)
qplot(month, price / weight, data = by_month, geom = "line", colour = state)
