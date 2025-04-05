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

# there are duplicates, so im dropping the month column and summing up the rest of the columns by loyalty_number
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



# apply more transformations
joined[is.na(joined)] <- 0 # change NA to 0 or NULL

joined$postal_code <- gsub(" ", "", history$postal_code) # remove spaces from the postal codes

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
joined$loyalty_number <- as.numeric(joined$loyalty_number)
joined$total_flights <- as.numeric(joined$total_flights)
joined$distance <- as.numeric(joined$distance)
joined$points_accumulated <- as.numeric(joined$points_accumulated)
joined$points_redeemed <- as.numeric(joined$points_redeemed)
joined$dollar_cost_points_redeemed <- as.numeric(joined$dollar_cost_points_redeemed)
joined$year <- as.numeric(joined$year)

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
skew_

# data is right skewed if > 0 

# applying log transformation on the following:
# points_accumulated, points_redeemed, dollar_cost_points_redeemed, distance

# let's see what that looks like
joined$points_accumulated <- log1p(joined$points_accumulated)
joined$points_redeemed <- log1p(joined$points_redeemed)
joined$dollar_cost_points_redeemed <- log1p(joined$dollar_cost_points_redeemed)
joined$distance <- log1p(joined$distance)

# square root transformation on total_flights column
joined$total_flights <- sqrt(log1p(joined$total_flights))

View(joined)

# boxplots still look funky but it's okay, that's normal
ggplot(joined, aes(y=points_accumulated)) +
  geom_boxplot()
    
# Transformation

# visual inspection

# history %>% gather(clv, salary) %>%
#   ggplot(aes(x = value)) +
#   geom_histogram(fill = "steelblue", alpha = 0.7) +
#   theme_minimal() + 
#   facet_wrap(~key, scales = "free")

plot(plot)

# check for skew
check_skewness <- function(history) {
  sapply(history, function(col) if (is.numeric(col)) skewness(col, na.rm = TRUE) else NA)
}
skew_values <- check_skewness(history)
print(skew_values)

# clv is positively skewed >3
# salary is positively skewed = 1.2
history$clv <- log1p(history$clv)
history$salary <- sqrt(log1p(history$salary))

zero_rows <- sum(history$salary == 0, na.rm = TRUE)
print(zero_rows)

history <- history %>% 
  filter(salary != 0) # filtering out rows where salary = 0
         
View(history)

# training data 70% train, 30% test
set.seed(123)
train_index <- createDataPartition(history$clv, p = 0.7, list = FALSE)
train_data <- history[train_index, ]
test_data <- history[-train_index]

# train logistic regression model
log_model <- glm(history$clv ~ history$gender + history$education + history$salary + history$marital_status + history$loyalty_card + history$salary)

# model summary
summary(log_model)

# model evaluation
test_data$pred_prob <- predict(log_model, newdata = history, type = "response")

# # Convert to class labels (threshold = 0.5)
# test_data$pred_class <- ifelse(test_data$pred_prob > 0.5, 1, 0)
# 
# # Confusion Matrix
# conf_matrix <- confusionMatrix(as.factor(test_data$pred_class), test_data$purchased)
# print(conf_matrix)
# 
# # AUC-ROC Curve
# roc_obj <- roc(test_data$purchased, test_data$pred_prob)
# auc_val <- auc(roc_obj)
# plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# checks

head(history)
colSums(is.na(history))
str(history)


