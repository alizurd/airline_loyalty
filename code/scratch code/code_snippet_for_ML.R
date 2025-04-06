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
