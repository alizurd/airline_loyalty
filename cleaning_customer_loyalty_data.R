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
history <- read.csv("customer_loyalty_history.csv", stringsAsFactors = FALSE)

View(history)

history <- clean_names(history) # change column names to lowercase and snakecase

history[is.na(history)] <- 0 # change NA to 0 or NULL

history$salary[is.na(history$salary)] <- median(history$salary, na.rm = TRUE) # change 0 salary values to median

history$postal_code <- gsub(" ", "", history$postal_code) # remove spaces from the postal codes

history <- history %>%
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
str(history)
history$loyalty_number <- as.numeric(history$loyalty_number)
history$salary <- as.numeric(history$salary)
history$clv <- as.numeric(history$clv)
history$enrollment_year <- as.numeric(history$enrollment_year)
history$enrollment_month <- as.numeric(history$enrollment_month)

history$salary <- abs(history$salary) # changing the negative salary values positive
                
# at this point i want to download data and explore in bigquery
write.csv(history, "loyalty_history.csv", na = "")
    
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


