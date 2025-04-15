# the purpose of this project is to take dummy airlines data and use it to predict churn
# secondary analyses include eda, and identifying predictors of clv using lm

# install all necessary packages

# install.packages("janitor")
# install.packages("lattice")
# install.packages("PRROC")
# install.packages("pROC")
# install.packages("party")
# install.packages("DT")

library(janitor)
library(moments)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)
library(party)
library(DT)

setwd("/Users/alyssabueno/Desktop/airline_loyalty/data/raw_data")
history_df <- read.csv("loyalty_history.csv", stringsAsFactors = FALSE)
flights <- read.csv("flight_activity.csv", stringsAsFactors = FALSE)

# Cleaning and joining datasets

# change column names to lowercase and snakecase
history_df <- clean_names(history_df) 
flights <- clean_names(flights)

# check to see if there are any unmatching rows before join
# flights_unmatched <- flights %>% filter(!loyalty_number %in% history_df$loyalty_number)
# history_df_unmatched <- history_df %>% filter(!loyalty_number %in% flights$loyalty_number)

# nrow(flights_unmatched)
# nrow(history_df_unmatched)

# remove rows with missing join keys
flights <- flights %>% filter(!is.na(loyalty_number))
history_df <- history_df %>% filter(!is.na(loyalty_number))

# check for duplicates in primary key
# any(duplicated(flights$loyalty_number))
# any(duplicated(history_df$loyalty_number))

history_df <- history_df %>%
mutate(
  salary = ifelse(education == "College" & salary == 0, NA, salary)
)

history_df <- history_df %>%
group_by(city) %>% 
mutate(
  salary = ifelse(is.na(salary),
  median(salary, na.rm = TRUE),
  salary)
) %>%
ungroup()

# there are multiple rows of customer data so i am counting unique month/ year combos, and years, and aggregating all other numeric cols
flights <- flights %>%
  group_by(loyalty_number) %>% # group by loyalty_number
  summarise(
    total_months = n_distinct(paste(year, month)), # counts unique month/ year combos
    total_years = n_distinct(year), # counts years
    across(-c(month, year), sum, na.rm = TRUE), 
            .groups = "drop" # drop grouping
    )  

# View(flights)

# Make sure it's the same type in both dataframes
# flights$loyalty_number <- as.numeric(flights$loyalty_number)
# history_df$loyalty_number <- as.numeric(history_df$loyalty_number)

# Remove whitespace, leading zeros, etc.
flights$loyalty_number <- trimws(flights$loyalty_number)
history_df$loyalty_number <- trimws(history_df$loyalty_number)

# removing any NAs in loyalty_number
flights <- flights %>% filter(!is.na(loyalty_number))
history_df <- history_df %>% filter(!is.na(loyalty_number))

# now join
joined <- inner_join(flights, history_df, by = "loyalty_number")

# apply more transformations
joined[is.na(joined)] <- 0 # change NA to 0 or NULL

# decided to not drop salary rows anymore because there was a pattern in 0s
# drop rows with salary == 0 
# joined <- joined %>% 
#   filter (salary != 0) %>%
#   drop_na()

# summary(joined)

joined$postal_code <- gsub(" ", "", joined$postal_code) # remove spaces from the postal codes


# Transformation

joined <- joined %>%
  mutate(churned = ifelse(cancellation_year > 0, 1, 0))

write.csv(joined, "full_airlines_dataset.csv", row.names = FALSE)

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
# str(joined)

joined <- joined %>%
  mutate(loyalty_number = as.numeric(loyalty_number),
         salary = as.numeric(salary),
         clv = as.numeric(clv),
         enrollment_year = as.factor(enrollment_year),
         enrollment_month = as.numeric(enrollment_month),
         total_flights = as.numeric(total_flights),
         distance = as.numeric(distance),
         points_accumulated = as.numeric(points_accumulated),
         points_redeemed = as.numeric(points_redeemed),
         dollar_cost_points_redeemed = as.numeric(dollar_cost_points_redeemed),
         cancellation_year = as.numeric(cancellation_year),
         cancellation_month = as.numeric(cancellation_month),
         churned = as.numeric(churned)
         )

joined$salary <- abs(joined$salary) # changing the negative salary values positive

joined <- joined %>%
  filter(points_redeemed <= points_accumulated) # filter out rows where points_redeemed > points_accumulated

# [optional] check for outliers
# ggplot(joined, aes(y=points_accumulated)) +
#   geom_boxplot()
# ggplot(joined, aes(y=points_redeemed)) +
#   geom_boxplot()
# ggplot(joined, aes(y=dollar_cost_points_redeemed)) +
#   geom_boxplot()

# checking for skew
# skew_points_accumulated <- skewness(joined$points_accumulated)
# skew_points_redeemed <- skewness(joined$points_redeemed)
# skew_dollar_cost_points_redeemed <- skewness(joined$dollar_cost_points_redeemed)
# skew_distance <- skewness(joined$distance)
# skew_total_flights <- skewness(joined$skew_total_flights)
# skew_salary <- skewness(joined$skew_salary) # returning errors - might need to investigate
# skew_clv <- skewness(joined$skew_clv)

# data is right skewed if > 0 

# applying log transformation on the following:
# points_accumulated, points_redeemed, dollar_cost_points_redeemed, distance

joined <- join %>%
mutate(
      log_dollar_cost_poimts_redeemed = log1p(dollar_cost_points_redeemed),
      log_points_accumulated <- log1p(points_accumulated),
      log_points_redeemed <- log1p(points_redeemed),
      log_distance <- log1p(distance),
      log_salary <- log1p(salary),
      log_clv <- log1p(clv),
      sqrt_total_flights <- sqrt(total_flights)
      ) 

# View(joined)

# check visually - boxplots still look funky but it's okay, that's normal
# ggplot(joined, aes(y=points_accumulated_t)) +
#   geom_boxplot()

# write.csv(joined, "clean_data.csv", row.names = FALSE)

# Model

# linear model with log data
# log_model <- lm(
#   clv ~ gender + education + marital_status + loyalty_card + enrollment_type + 
#     total_flights + distance + enrollment_year + cancellation_year + points_accumulated + points_redeemed
#     + salary,
#   data = joined
# )

# summary(log_model)

# # lm with untransformed data
# model <- lm(
#   clv ~ gender + education + province + marital_status + salary+ loyalty_card + enrollment_type + 
#     total_flights + distance + enrollment_year + cancellation_year + points_accumulated + points_redeemed
#     + cancellation_month + cancellation_year,
#   data = joined
# )

# # model summary
# summary(model)

# as a followup, would be interesting to see the relationship between salary and other variables..

# salary_model <- lm(salary ~ loyalty_card + marital_status + enrollment_month
#                    + total_flights, data = joined
#                    )

# next steps: continue refining, it's possible i cut too much of the data
# priority though is to move onto the ML part
# question of the hour: how can we predict customer churn in the loyalty program?

# churn_model <- glm(churned ~ gender + education + salary + marital_status + loyalty_card + clv 
#                    + enrollment_year + total_flights + distance + points_accumulated + points_redeemed
#                    + dollar_cost_points_redeemed, data = joined,
#                    family = binomial
#                     )

# summary(churn_model)

# this is the glm model with transformed data

set.seed(123)
train_index <- sample(1:nrow(joined), 0.8 * nrow(joined)) # samples 80% of the data to train the model
train_data <- joined [train_index, ] # subsets the original data indicated in train_index
test_data <- joined[-train_index, ] # uses the remaining 30% to use to test the model

# churn_model_tran <- glm(churned ~ gender + education + log_salary + marital_status + loyalty_card + log_clv 
#                    + enrollment_year + sqrt_total_flights + log_distance + log_points_accumulated 
#                    + log_points_redeemed + log_dollar_cost_points_redeemed,
#                    data = train_data,
#                    family = binomial)

# summary(churn_model_tran)

x_train <- model.matrix(churned ~ loyalty_card
                   + sqrt_total_flights + log_distance + log_points_accumulated 
                   + log_dollar_cost_points_redeemed + log_clv
                   + education + marital_status + cancellation_year,
                   data = train_data) [, -1]

y_train <- train_data$churned

weights <- ifelse(y_train == 1, 5, 1)

churn_model_tran <- glmnet(x_train, y_train, family = "binomial"
                    , weights = weights)

x_test <- model.matrix(churned ~ loyalty_card
                   + sqrt_total_flights + log_distance + log_points_accumulated 
                   + log_dollar_cost_points_redeemed + log_clv
                   + education + marital_status + cancellation_year,
                   data = test_data) [, -1]

# evaluate

# 1. Predict on test_data
test_data$predicted_prob <- as.factor(predict(churn_model_tran, newx = x_test, type = "response", s = 0.01))
test_data$predicted_prob <- as.numeric(as.character(test_data$predicted_prob))


# 2. Classify based on threshold
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.3, 1, 0)

coef(churn_model_tran, s = "lambda.min")

# 3. ROC Curve 
roc_obj <- roc(test_data$churned, test_data$predicted_prob)
auc_val <- auc(roc_obj)
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# 4. Confusion Matrix (on test set, not joined)
table(Predicted = test_data$predicted_class, Actual = test_data$churned)

# 5. Accuracy (this measures proportion correctly predicted)
mean(test_data$predicted_class == test_data$churned)

# library(PRROC)
pr <- pr.curve(scores.class0 = test_data$predicted_prob[test_data$churned == 1],
               scores.class1 = test_data$predicted_prob[test_data$churned == 0],
               curve = TRUE)
plot(pr)
