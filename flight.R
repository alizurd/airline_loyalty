install.packages("moments")
library(tidyverse)
library(dplyr)
library(moments)

setwd("/Users/alyssabueno/Desktop/airline_loyalty")
flights <- read.csv("flight_activity.csv", stringsAsFactors = FALSE)
View(flights)

# changing column names
flights <- clean_names(flights)

# checking the data types in each column
str(flights)

# making columns numeric
flights$loyalty_number <- as.numeric(flights$loyalty_number)
flights$month <- as.numeric(flights$month)
flights$total_flights <- as.numeric(flights$total_flights)
flights$distance <- as.numeric(flights$distance)
flights$points_accumulated <- as.numeric(flights$points_accumulated)
flights$points_redeemed <- as.numeric(flights$points_redeemed)
flights$dollar_cost_points_redeemed <- as.numeric(flights$dollar_cost_points_redeemed)
flights$year <- as.numeric(flights$year)

flights <- flights %>%
  filter(points_redeemed <= points_accumulated) # filter out rows where points_redeemed > points_accumulated

# download as csv
write.csv(flights, "flights.csv", na = "")

# check for outliers
summary(flights)
ggplot(flights, aes(y=points_accumulated)) +
  geom_boxplot()
ggplot(flights, aes(y=points_redeemed)) +
  geom_boxplot()
ggplot(flights, aes(y=dollar_cost_points_redeemed)) +
  geom_boxplot()

# checking for skew
skew_points_accumulated <- skewness(flights$points_accumulated)
skew_points_redeemed <- skewness(flights$points_redeemed)
skew_dollar_cost_points_redeemed <- skewness(flights$dollar_cost_points_redeemed)
skew_distance <- skewness(flights$distance)
skew_total_flights <- skewness(flights$skew_total_flights)

# data is right skewed if > 0 

# applying log transformation on the following:
# points_accumulated, points_redeemed, dollar_cost_points_redeemed, distance

# let's see what that looks like
flights$points_accumulated <- log1p(flights$points_accumulated)
flights$points_redeemed <- log1p(flights$points_redeemed)
flights$dollar_cost_points_redeemed <- log1p(flights$dollar_cost_points_redeemed)
flights$distance <- log1p(flights$distance)

# square root transformation on total_flights column
flights$total_flights <- sqrt(log1p(flights$total_flights))

View(flights)

# boxplots still look funky but it's okay, that's normal
ggplot(flights, aes(y=points_accumulated)) +
  geom_boxplot()

# download as csv
write.csv(flights, "flights_transformed.csv", na = "")
