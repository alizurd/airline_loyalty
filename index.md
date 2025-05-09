---
layout: default
title: Welcome
---

# Airline Loyalty Analysis ✈️  
Exploring churn patterns using GLMNET

# Background

Used a dummy airline loyalty program dataset to uncover predictors of churn using a logistic regression machine learning model.

# Executive Summary
 
People who earn points but never use them aren't seeing value in staying. Flight frequency is another big predictor of churn. Occasional flyers are more likely to leave the program. The length of someone's membership makes a difference in their loyalty; people who signed up earlier are less likely to leave.

# Methods

Fit a logistic regression model using GLMNET using L1/L2 regularization to predict churners in an airlines loyalty program (dummy data) with 73% recall, 84% precision, and 95% overall accuracy.

# Key Model Metrics

- Accuracy: 94.92%
- Sensitivity (Recall): 72.88%
- Specificity: 98.02%
- Precision: 83.84%
- F1 Score: 78% (at threshold 0.75)
- AUC: 0.923

## Recommendations

The good news is we can now predict who's likely to churn before they do. High-risk members can be sent targeted offers that can remind them to use their points and provide additional promotions to help them utilize the benefits of the program!

# Visualizations

![Correlation](images/Corr.png)
![ROC](images/ROC.png)
![Features](images/features.png)