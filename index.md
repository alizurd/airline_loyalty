---
layout: default
title: Welcome
---

# Airline Loyalty Analysis ✈️  
Exploring churn patterns using GLMNET

# Purpose
 
The project looks to find predictors of churn in an airlines loyalty dataset and provide recommendations to improve retention for high risk members.

# Methods

1. Data Cleaning
2. EDA
3. Transformations
4. Creating GLMNET model
5. Evaluating
6. Refining
7. Insights

# Results

Fit a logistic regression model using GLMNET using L1/L2 regularization to predict churners in an airlines loyalty program (dummy data) with 73% recall, 84% precision, and 95% overall accuracy.

## Key Metrics:

Accuracy: 94.92%
Sensitivity (Recall): 72.88%
Specificity: 98.02%
Precision: 83.84%
F1 Score: 78% (at threshold 0.75)
AUC: 0.923

## Insights

People who earn points but never use them aren't seeing value in staying. Flight frequency is another big one. Occasional flyers are more likely to churn. The length of someone's membership makes a difference in their loyalty; people who signed up earlier are less likely to leave

## Recommendations

The good news is we can now predict who's likely to churn before they do. High-risk members can be sent targeted offers that can remind them to use their points and provide additional promotions to help them utilize the benefits of the program!

# Visualizations

![Alt text](images/Corr.jpg)
![Alt text](images/ROC.jpg)
![Alt text](images/features.jpg)