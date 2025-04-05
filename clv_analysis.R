install.packages("janitor")
install.packages("lattice")
install.packages("pROC")
install.packages("party")
library(janitor)
library(moments)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)
library(party)



setwd("/Users/alyssabueno/Desktop/airline_loyalty")
history <- read.csv("loyalty_history.csv", stringsAsFactors = FALSE)
flights <- read.csv("flight_activity.csv", stringsAsFactors = FALSE)


# Cleaning and joining datasets

# change column names to lowercase and snakecase
history <- clean_names(history) 
flights <- clean_names(flights)

# check to see if there are any unmatching rows before join
flights_unmatched <- flights %>% filter(!loyalty_number %in% history$loyalty_number)
history_unmatched <- history %>% filter(!loyalty_number %in% flights$loyalty_number)

nrow(flights_unmatched)
nrow(history_unmatched)

# remove rows with missing join keys
flights <- flights %>% filter(!is.na(loyalty_number))
history <- history %>% filter(!is.na(loyalty_number))

# check for duplicates in primary key
any(duplicated(flights$loyalty_number))
any(duplicated(history$loyalty_number))

# there are duplicates in flights data, so im dropping the month + year and summing up the rest of the columns by loyalty_number
flights <- flights %>%
  select(-month, -year) %>%                             # Drop the 'month' column
  group_by(loyalty_number) %>%                   # Group by unique ID
  summarise(across(everything(), sum), .groups = "drop")  # Sum all numeric columns

# Make sure it's the same type in both dataframes
flights$loyalty_number <- as.numeric(flights$loyalty_number)
history$loyalty_number <- as.numeric(history$loyalty_number)

# Remove whitespace, leading zeros, etc.
flights$loyalty_number <- trimws(flights$loyalty_number)
history$loyalty_number <- trimws(history$loyalty_number)

# removing any NAs in loyalty_number
flights <- flights %>% filter(!is.na(loyalty_number))
history <- history %>% filter(!is.na(loyalty_number))

# now join
joined <- inner_join(flights, history, by = "loyalty_number")

View(joined)


# apply more transformations
joined[is.na(joined)] <- 0 # change NA to 0 or NULL

# drop rows with salary == 0 
joined <- joined %>% 
  filter (salary != 0) %>%
  drop_na()

joined$postal_code <- gsub(" ", "", joined$postal_code) # remove spaces from the postal codes



# Transformation

joined <- joined %>%
  mutate(country = as.factor(country),
         province = as.factor(province),
         city = as.factor(city),
         postal_code = as.factor(postal_code),
         gender = as.factor(gender),
         education = as.factor(education),
         marital_status = as.factor(marital_status),
         loyalty_card = as.factor(loyalty_card),
         enrollment_type = as.factor(enrollment_type))

# making columns numeric
str(joined)
joined$loyalty_number <- as.numeric(joined$loyalty_number)
joined$salary <- as.numeric(joined$salary)
joined$clv <- as.numeric(joined$clv)
joined$enrollment_year <- as.numeric(joined$enrollment_year)
joined$enrollment_month <- as.numeric(joined$enrollment_month)
joined$total_flights <- as.numeric(joined$total_flights)
joined$distance <- as.numeric(joined$distance)
joined$points_accumulated <- as.numeric(joined$points_accumulated)
joined$points_redeemed <- as.numeric(joined$points_redeemed)
joined$dollar_cost_points_redeemed <- as.numeric(joined$dollar_cost_points_redeemed)
joined$cancellation_month <- as.numeric(joined$cancellation_month)
joined$cancellation_year <- as.numeric(joined$cancellation_year)

joined$salary <- abs(joined$salary) # changing the negative salary values positive

joined <- joined %>%
  filter(points_redeemed <= points_accumulated) # filter out rows where points_redeemed > points_accumulated

# [optional] check for outliers
ggplot(joined, aes(y=points_accumulated)) +
  geom_boxplot()
ggplot(joined, aes(y=points_redeemed)) +
  geom_boxplot()
ggplot(joined, aes(y=dollar_cost_points_redeemed)) +
  geom_boxplot()

# checking for skew
skew_points_accumulated <- skewness(joined$points_accumulated)
skew_points_redeemed <- skewness(joined$points_redeemed)
skew_dollar_cost_points_redeemed <- skewness(joined$dollar_cost_points_redeemed)
skew_distance <- skewness(joined$distance)
skew_total_flights <- skewness(joined$skew_total_flights)
# skew_salary <- skewness(joined$skew_salary) # returning errors - might need to investigate
# skew_clv <- skewness(joined$skew_clv)

# data is right skewed if > 0 

# applying log transformation on the following:
# points_accumulated, points_redeemed, dollar_cost_points_redeemed, distance

# let's see what that looks like
joined$log_dollar_cost_points_redeemed <- log1p(joined$dollar_cost_points_redeemed)
joined$log_points_accumulated <- log1p(joined$points_accumulated)
joined$log_points_redeemed <- log1p(joined$points_redeemed)
joined$log_dollar_cost_points_redeemed <- log1p(joined$dollar_cost_points_redeemed)
joined$log_distance <- log1p(joined$distance)
joined$log_salary <- log1p(joined$salary)
joined$log_clv <- log1p(joined$clv)

# square root transformation on total_flights column
joined$sqrt_total_flights <- sqrt(log1p(joined$total_flights))

View(joined)

# check visually - boxplots still look funky but it's okay, that's normal
ggplot(joined, aes(y=points_accumulated_t)) +
  geom_boxplot()





# Model

# linear model with log data
log_model <- lm(
  clv ~ gender + education + marital_status + loyalty_card + enrollment_type + 
    total_flights + distance + enrollment_year + cancellation_year + points_accumulated + points_redeemed
    + salary,
  data = joined
)

summary(log_model)

# lm with untransformed data
model <- lm(
  clv ~ gender + education + province + marital_status + salary+ loyalty_card + enrollment_type + 
    total_flights + distance + enrollment_year + cancellation_year + points_accumulated + points_redeemed
    + cancellation_month + cancellation_year,
  data = joined
)

# model summary
summary(model)

# curious to see the relationship between salary and other variables

salary_model <- lm(salary ~ loyalty_card + marital_status + enrollment_month
                   + total_flights + )


# next steps: continue refining, it's possible i cut too much of the data
# priority though is to move onto the ML part
# question of the hour: how can we predict customer enrollment in the loyalty program?

