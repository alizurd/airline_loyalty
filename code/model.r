# Libraries
library(janitor)
library(dplyr)
library(tidyverse)
library(caret)
library(pROC)
library(glmnet)
library(tidymodels)
library(broom)

# Load data
setwd("/Users/alyssabueno/Desktop/airline_loyalty/data/raw_data")
history_df <- read.csv("loyalty_history.csv", stringsAsFactors = FALSE)
flights <- read.csv("flight_activity.csv", stringsAsFactors = FALSE)

# Clean and prepare data
history_df <- clean_names(history_df) 
flights <- clean_names(flights)

# Remove rows with missing join keys
flights <- flights %>% filter(!is.na(loyalty_number))
history_df <- history_df %>% filter(!is.na(loyalty_number))

# Handle missing salaries
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

# Aggregate flight data
flights <- flights %>%
  group_by(loyalty_number) %>%
  summarise(
    total_months = n_distinct(paste(year, month)),
    total_years = n_distinct(year),
    across(-c(month, year), sum, na.rm = TRUE), 
    .groups = "drop"
  )  

# Clean join keys
flights$loyalty_number <- trimws(flights$loyalty_number)
history_df$loyalty_number <- trimws(history_df$loyalty_number)

# Join datasets
joined <- inner_join(flights, history_df, by = "loyalty_number")

# Handle missing values
joined[is.na(joined)] <- 0

# Clean postal codes
joined$postal_code <- gsub(" ", "", joined$postal_code)

# Create target variable
joined <- joined %>%
  mutate(churned = ifelse(cancellation_year > 0, 1, 0))

# Convert to appropriate data types
joined <- joined %>%
  mutate(
    # Factors
    province = as.factor(province),
    city = as.factor(city),
    postal_code = as.factor(postal_code),
    gender = as.factor(gender),
    education = as.factor(education),
    marital_status = as.factor(marital_status),
    loyalty_card = as.factor(loyalty_card),
    enrollment_type = as.factor(enrollment_type),
    enrollment_year = as.factor(enrollment_year),
    
    # Numeric
    loyalty_number = as.numeric(loyalty_number),
    salary = abs(as.numeric(salary)), # Make negative salaries positive
    clv = as.numeric(clv),
    enrollment_month = as.numeric(enrollment_month),
    total_flights = as.numeric(total_flights),
    distance = as.numeric(distance),
    points_accumulated = as.numeric(points_accumulated),
    points_redeemed = as.numeric(points_redeemed),
    dollar_cost_points_redeemed = as.numeric(dollar_cost_points_redeemed),
    total_months = as.numeric(total_months),
    total_years = as.numeric(total_years),
    churned = as.factor(churned) # Make target a factor for classification
  )

# Filter out invalid data
joined <- joined %>%
  filter(points_redeemed <= points_accumulated)

# Apply transformations for skewed variables
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

# Split data - ensure stratified sampling on churned
set.seed(123)
split <- initial_split(joined, prop = 0.8, strata = churned)
train_data <- training(split)
test_data <- testing(split)

# IMPORTANT: Remove leakage-prone columns BEFORE modeling
# Remove direct leakage (cancellation columns) and transformed versions of the same data
model_vars <- train_data %>% 
  select(
    # Keep demographic features
    gender, education, marital_status, log_salary,
    
    # Keep loyalty-related features  
    loyalty_card, enrollment_type, enrollment_year, enrollment_month,
    total_months, total_years,
    
    # Keep activity-based metrics - carefully selected to avoid leakage
    total_flights, log_distance, log_points_accumulated,
    
    # Target variable
    churned
  )

# Check class imbalance
table(model_vars$churned)

# Create formula for model
model_formula <- as.formula(churned ~ .)

# Prepare train/test matrices
x_train <- model.matrix(model_formula, data = model_vars)[,-1] # Remove intercept
y_train <- model_vars$churned

# Prepare test data
test_vars <- test_data %>%
  select(names(model_vars))
x_test <- model.matrix(model_formula, data = test_vars)[,-1]
y_test <- test_vars$churned

# Calculate class weights to handle imbalance
class_weights <- ifelse(y_train == 1, 
                        (sum(y_train == 0) / sum(y_train == 1)), # Weight proportional to class imbalance
                        1)

# Train model with cross-validation
set.seed(345)
cv_model <- cv.glmnet(x_train, y_train, 
                      family = "binomial",
                      weights = class_weights, 
                      alpha = 1,
                      nfolds = 5)

# Plot cross-validation results
plot(cv_model)

# Get optimal lambda
lambda_min <- cv_model$lambda.min
lambda_1se <- cv_model$lambda.1se
cat("Lambda min:", lambda_min, "\n")
cat("Lambda 1se:", lambda_1se, "\n")

# Predict probabilities on test set
test_data$predicted_prob <- predict(cv_model, 
                                    newx = x_test, 
                                    s = "lambda.1se", # Using 1se as it's more regularized
                                    type = "response")[,1]

# Look at the distribution of predicted probabilities
hist(test_data$predicted_prob, 
     breaks = 30,
     main = "Distribution of Predicted Probabilities",
     xlab = "Predicted Probability")

# Evaluate performance metrics at different thresholds
thresholds <- seq(0.1, 0.9, 0.05)
results <- map_dfr(thresholds, function(thresh) {
  test_data$predicted_class <- factor(ifelse(test_data$predicted_prob > thresh, 1, 0),
                                      levels = levels(test_data$churned))
  
  cm <- confusionMatrix(test_data$predicted_class, test_data$churned, positive = "1")
  
  tibble(
    Threshold = thresh,
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"], # Recall for positive class
    Specificity = cm$byClass["Specificity"],
    Precision = cm$byClass["Pos Pred Value"],
    F1_Score = cm$byClass["F1"]
  )
})

# Print results
print(results)

# Find best threshold based on F1 score
best_threshold <- results$Threshold[which.max(results$F1_Score)]
cat("Best threshold based on F1 score:", best_threshold, "\n")

# Final classification
test_data$predicted_class <- factor(ifelse(test_data$predicted_prob > best_threshold, 1, 0),
                                    levels = levels(test_data$churned))

# Final confusion matrix
final_cm <- confusionMatrix(test_data$predicted_class, test_data$churned, positive = "1")
print(final_cm)

# ROC curve
roc_obj <- roc(as.numeric(test_data$churned) - 1, test_data$predicted_prob)
auc_val <- auc(roc_obj)
cat("AUC:", auc_val, "\n")

# Plot ROC curve
plot(roc_obj, main = paste("ROC Curve (AUC =", round(auc_val, 3), ")"))

# Feature importance
coef_df <- as.data.frame(as.matrix(coef(cv_model, s = "lambda.1se")))
coef_df$Variable <- rownames(coef_df)
colnames(coef_df)[1] <- "Coefficient"
coef_df <- coef_df[-1,] # Remove intercept
coef_df$AbsCoefficient <- abs(coef_df$Coefficient)
coef_df <- coef_df[order(-coef_df$AbsCoefficient),]

# Print top features
head(coef_df, 10)

# Save model
model <- saveRDS(list(model = cv_model, threshold = best_threshold), "churn_model.rds")

# Save as CSV
model <- readRDS("churn_model.rds")
write.csv(df, "model.csv", row.names = FALSE)

obj <- readRDS("churn_model.rds")
str(obj)


# Visualizations

# feature importance visualization

ggplot(coef_df[1:15,], aes(x = reorder(Variable, AbsCoefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = ifelse(coef_df$Coefficient[1:15] > 0, "steelblue", "firebrick")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Features Influencing Churn Prediction",
       subtitle = paste("Based on LASSO model with lambda =", round(lambda_1se, 5)),
       x = "Variables",
       y = "Coefficient Value")

# Create churn rates by different segments
segment_churn <- joined %>%
  group_by(loyalty_card) %>%
  summarize(
    total = n(),
    churned = sum(churned == 1),
    churn_rate = churned / total
  )

ggplot(segment_churn, aes(x = loyalty_card, y = churn_rate, fill = loyalty_card)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(churn_rate)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Churn Rate by Loyalty Card Type",
       x = "Loyalty Card Type",
       y = "Churn Rate")

# Compare flight activity patterns
ggplot(joined, aes(x = churned, y = log_distance, fill = churned)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Flight Distance Comparison: Churned vs. Non-Churned Customers",
       x = "Churned Status",
       y = "Log(Distance)")

# Similar plots for other flight metrics
ggplot(joined, aes(x = churned, y = sqrt_total_flights, fill = churned)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Flight Frequency Comparison: Churned vs. Non-Churned Customers",
       x = "Churned Status",
       y = "Sqrt(Total Flights)")

# Create correlation matrix for numerical variables
numeric_vars <- joined %>%
  select(total_flights, distance, points_accumulated, points_redeemed, 
         total_months, total_years, salary, clv) %>%
  cor(use = "complete.obs")

library(corrplot)
corrplot(numeric_vars, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Key Metrics")
