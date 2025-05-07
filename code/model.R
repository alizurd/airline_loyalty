# the purpose of this project is to take dummy airlines data and use it to predict churn
# secondary analyses include eda, and identifying predictors of clv using lm

# install all necessary packages

# install.packages("janitor")
# install.packages("lattice")
# install.packages("PRROC")
# install.packages("pROC")
# install.packages("party")
# install.packages("DT")
# install.packages("corrplot")

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

joined <- joined %>% select(-country)


# Transformation

joined <- joined %>%
  mutate(churned = ifelse(cancellation_year > 0, 1, 0))

# write.csv(joined, "full_airlines_dataset.csv", row.names = FALSE)

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

joined <- joined %>%
mutate(
      log_dollar_cost_points_redeemed = log1p(dollar_cost_points_redeemed),
      log_points_accumulated = log1p(points_accumulated),
      log_points_redeemed = log1p(points_redeemed),
      log_distance = log1p(distance),
      log_salary = log1p(salary),
      log_clv = log1p(clv),
      sqrt_total_flights = sqrt(total_flights)
      ) 

View(joined)

# check visually - boxplots still look funky but it's okay, that's normal
# ggplot(joined, aes(y=points_accumulated_t)) +
#   geom_boxplot()

# write.csv(joined, "clean_data.csv", row.names = FALSE)

install.packages("rlang")
install.packages("tidymodels")
library(tidymodels)
library(glmnet)
library(pROC)
library(PRROC)
library(corrplot)
library(car)

set.seed(123)

# Split data
split <- initial_split(joined, prop = 0.8, strata = churned)
train_data <- training(split)
test_data <- testing(split)

View(train_data)

# Correlation matrix (optional, for numeric predictors)
predictors <- train_data %>%
  select(sqrt_total_flights, log_distance, 
         log_points_accumulated, log_dollar_cost_points_redeemed, log_clv) %>%
  as.data.frame()
corrplot(cor(predictors, use = "complete.obs"), method = "circle", type = "upper")

# dropped log_points_accumulated and sqrt_total_flights due to collinearity

# table(train_data$churned, train_data$education)
# table(train_data$churned, train_data$marital_status)

train_data <- train_data %>%
  mutate(education = fct_lump(education, n = 4),
         marital_status = fct_lump(marital_status, n = 3))

str(train_data)

train_data <- train_data %>%
  mutate(across(c(total_months, total_years), as.numeric))

# # VIF check
# vif_model <- glm(churned ~ ., data = train_data %>%
#                    select(churned, log_distance, 
#                           log_dollar_cost_points_redeemed, 
#                           log_clv, education, marital_status, cancellation_year),
#                  family = binomial)
# print(vif(vif_model))

# Train data
x_train <- model.matrix(churned ~ . -1, data = train_data)
y_train <- train_data$churned

# Test data
x_test <- model.matrix(~ . -1, data = test_data %>% select(-churned))
y_test <- test_data$churned

# Class weights
weights <- ifelse(y_train == 1, 8, 1)

# Train model
set.seed(345)
cv_model <- cv.glmnet(x_train, y_train, family = "binomial", weights = weights, alpha = 0.5)

# Predict on test set
test_data$predicted_prob <- predict(cv_model, newx = x_test, s = "lambda.min", type = "response")[,1]

# Evaluate thresholds
thresholds <- seq(0.1, 0.9, 0.1)
results <- map_dfr(thresholds, function(thresh) {
  preds <- ifelse(test_data$predicted_prob > thresh, 1, 0)
  cm <- table(Pred = preds, Actual = test_data$churned)
  tp <- cm["1", "1"]; tn <- cm["0", "0"]
  fp <- cm["1", "0"]; fn <- cm["0", "1"]
  
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
  recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
  f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)
  acc <- (tp + tn) / sum(cm)
  specificity <- tn / (tn + fp)
  
  tibble(Threshold = thresh, Accuracy = acc, Sensitivity = recall, 
         Specificity = specificity, F1_Score = f1)
})

# Choose best threshold
best_threshold <- results$Threshold[which.max(results$F1_Score)]

# Final classification and metrics
test_data$predicted_class <- ifelse(test_data$predicted_prob > best_threshold, 1, 0)
roc_obj <- roc(test_data$churned, test_data$predicted_prob)
auc_val <- auc(roc_obj)

print(roc_obj)
print(auc_val)

# Confusion matrix
cm <- table(Predicted = test_data$predicted_class, Actual = test_data$churned)
print(cm)

# Feature importance
coef_df <- tidy(cv_model, s = "lambda.min") %>%
  filter(term != "(Intercept)") %>%
  mutate(abs_coef = abs(estimate)) %>%
  arrange(desc(abs_coef))

# Save model
saveRDS(list(model = cv_model, threshold = best_threshold), "churn_model.rds")
