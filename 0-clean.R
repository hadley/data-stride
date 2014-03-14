library(dplyr)
library(ggplot2)

cocaine <- read.csv("raw/stride_cocaine.csv", na.string = "-", 
  stringsAsFactors = FALSE)
names(cocaine) <- c("state", "country", "method", "drug", "potency", "weight", 
  "year", "month", "price")

to_num <- function(x) as.numeric(gsub("[^0-9.]*", "", x))

cocaine$price <- to_num(cocaine$price)
cocaine$weight <- to_num(cocaine$weight)
# 98,292 kg seized in 2007 according according to 
# http://www.justice.gov/dea/resource-center/statistics.shtml#seizures
# So unit must be grams
sum(cocaine$weight) / 1e3

# Biggest = 15,000 k = ~40,000 lb
# http://www.youtube.com/watch?v=FXmASAPyT-E
cocaine %.% filter(weight == max(weight))

# Weight is heavily skewed
qplot(weight, data = cocaine, fill = is.na(price))
qplot(weight, data = cocaine, fill = is.na(price)) + scale_x_log10()
# Don't have prices for large hauls

cocaine$potency <- to_num(cocaine$potency)

# Select only US seizures, with known weight, and on cocaine (hydrochloride)
# Remove constant variables
clean <- tbl_df(cocaine) %.% 
  filter(country == "", !is.na(price), drug == "COCAINE", weight > 0) %.%
  select(-c(country, drug, year, method))

write.csv(clean, "cocaine.csv", row.names = FALSE)

