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

# Correlation analysis
library(corrplot)
predictor_vars <- train_data %>% 
  select(loyalty_card, sqrt_total_flights, log_distance, log_points_accumulated,
         log_dollar_cost_points_redeemed, log_clv) %>%
  as.data.frame()

cor_matrix <- cor(predictor_vars, use = "complete.obs")
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, diag = FALSE)

# VIF analysis (if no severe multicollinearity)
library(car)
vif_model <- glm(churned ~ loyalty_card + sqrt_total_flights + log_distance + 
                 log_points_accumulated + log_dollar_cost_points_redeemed + 
                 log_clv + education + marital_status + cancellation_year,
                 data = train_data, family = binomial)
vif_values <- vif(vif_model)
print(vif_values)


# GLM model with transformed data

library(tidymodels)
library(glmnet)
library(pROC)
library(PRROC)

# Set seed for reproducibility
set.seed(123)

# Split data into training and test sets
train_index <- sample(1:nrow(joined), 0.8 * nrow(joined))
train_data <- joined[train_index, ]
test_data <- joined[-train_index, ]

# Create bootstrap samples for potential cross-validation
set.seed(234)
boot_data <- bootstraps(train_data, strata = churned)

# Create model matrix for predictors (excluding intercept)
x_train <- model.matrix(churned ~ loyalty_card
                   + sqrt_total_flights + log_distance + log_points_accumulated 
                   + log_dollar_cost_points_redeemed + log_clv
                   + education + marital_status + cancellation_year,
                   data = train_data)[, -1]

y_train <- train_data$churned

# Apply class weights to address imbalance
weights <- ifelse(y_train == 1, 8, 1)

# Fit regularized logistic regression model using glmnet
# Use cross-validation to choose the optimal lambda
set.seed(345)
cv_model <- cv.glmnet(x_train, y_train, family = "binomial", 
                     weights = weights, alpha = 0.5) # Using elastic net (alpha=0.5)

# Display the optimal lambda values
print(paste("Lambda min:", cv_model$lambda.min))
print(paste("Lambda 1se:", cv_model$lambda.1se))

# Fit final model with optimal lambda
churn_model_tran <- glmnet(x_train, y_train, family = "binomial",
                         weights = weights, alpha = 0.5)

# Create test matrix
x_test <- model.matrix(churned ~ loyalty_card
                   + sqrt_total_flights + log_distance + log_points_accumulated 
                   + log_dollar_cost_points_redeemed + log_clv
                   + education + marital_status + cancellation_year,
                   data = test_data)[, -1]

# Model evaluation
# 1. Predict probabilities on test data
test_data$predicted_prob <- predict(churn_model_tran, newx = x_test, 
                                  type = "response", s = cv_model$lambda.min)[,1]

# 2. Display model coefficients
print("Model coefficients:")
coef_matrix <- as.matrix(coef(churn_model_tran, s = cv_model$lambda.min))
coef_df <- data.frame(
  Variable = rownames(coef_matrix),
  Coefficient = coef_matrix[,1]
)
print(coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE),])

# 3. Evaluate different probability thresholds
thresholds <- seq(0.1, 0.9, by = 0.1)
results <- data.frame(
  Threshold = thresholds,
  Accuracy = numeric(length(thresholds)),
  Sensitivity = numeric(length(thresholds)),
  Specificity = numeric(length(thresholds)),
  F1_Score = numeric(length(thresholds))
)

for (i in 1:length(thresholds)) {
  threshold <- thresholds[i]
  test_data$predicted_class <- ifelse(test_data$predicted_prob > threshold, 1, 0)
  
  # Confusion matrix elements
  cm <- table(Predicted = test_data$predicted_class, Actual = test_data$churned)
  
  # Calculate metrics (handling potential division by zero)
  tp <- ifelse(length(cm) == 4, cm[2,2], 0)
  tn <- ifelse(length(cm) == 4, cm[1,1], ifelse(all(test_data$churned == 0), sum(test_data$predicted_class == 0), 0))
  fp <- ifelse(length(cm) == 4, cm[2,1], sum(test_data$predicted_class == 1))
  fn <- ifelse(length(cm) == 4, cm[1,2], sum(test_data$predicted_class == 0))
  
  results$Accuracy[i] <- (tp + tn) / (tp + tn + fp + fn)
  results$Sensitivity[i] <- ifelse(tp + fn > 0, tp / (tp + fn), 0)  # Recall/True Positive Rate
  results$Specificity[i] <- ifelse(tn + fp > 0, tn / (tn + fp), 0)  # True Negative Rate
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
  results$F1_Score[i] <- ifelse(precision + results$Sensitivity[i] > 0, 
                               2 * precision * results$Sensitivity[i] / (precision + results$Sensitivity[i]), 0)
}

# Display threshold analysis results
print("Threshold Analysis:")
print(results)

# Choose the optimal threshold based on F1 score
best_threshold_idx <- which.max(results$F1_Score)
best_threshold <- results$Threshold[best_threshold_idx]
print(paste("Best threshold based on F1 score:", best_threshold))

# Apply best threshold
test_data$predicted_class <- ifelse(test_data$predicted_prob > best_threshold, 1, 0)

# 4. ROC Curve and AUC
roc_obj <- roc(test_data$churned, test_data$predicted_prob)
auc_val <- auc(roc_obj)

# 5. Confusion Matrix with best threshold
conf_matrix <- table(Predicted = test_data$predicted_class, Actual = test_data$churned)
print("Confusion Matrix:")
print(conf_matrix)

# 6. Comprehensive performance metrics
tp <- conf_matrix[2,2]
tn <- conf_matrix[1,1]
fp <- conf_matrix[2,1]
fn <- conf_matrix[1,2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)  # Same as sensitivity
f1 <- 2 * precision * recall / (precision + recall)
specificity <- tn / (tn + fp)

print(paste("Accuracy:", round(accuracy, 3)))
print(paste("Precision:", round(precision, 3)))
print(paste("Recall/Sensitivity:", round(recall, 3)))
print(paste("Specificity:", round(specificity, 3)))
print(paste("F1 Score:", round(f1, 3)))
print(paste("AUC:", round(auc_val, 3)))

# 7. Visualizations
# ROC Curve
pdf("roc_curve.pdf")
plot(roc_obj, col = "blue", main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()

# PR Curve
pr <- pr.curve(scores.class0 = test_data$predicted_prob[test_data$churned == 1],
               scores.class1 = test_data$predicted_prob[test_data$churned == 0],
               curve = TRUE)
pdf("pr_curve.pdf")
plot(pr, main = paste("PR Curve (AUC =", round(pr$auc.integral, 3), ")"))
dev.off()

# 8. Feature importance visualization
coef_df <- coef_df[order(abs(coef_df$Coefficient), decreasing = TRUE),]
coef_df <- coef_df[coef_df$Variable != "(Intercept)",]  # Remove intercept for visualization

pdf("feature_importance.pdf")
barplot(abs(coef_df$Coefficient), 
        names.arg = coef_df$Variable, 
        horiz = TRUE, 
        las = 1, 
        cex.names = 0.7,
        main = "Feature Importance (Absolute Coefficient Values)",
        xlab = "Absolute Coefficient Value")
dev.off()

# 9. Save the model
saveRDS(list(model = churn_model_tran, 
             lambda = cv_model$lambda.min,
             threshold = best_threshold), 
        "churn_model.rds")