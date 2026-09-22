library(tidyverse)
library(caret)
library(pROC)

getwd()
list.files()
list.files(recursive = TRUE)

churn_data <- read_csv("../data/Telco-Customer-Churn-Cleaned.csv")
dim(churn_data)

#Check the structure
str(churn_data)
summary(churn_data)

#Check the target variable
table(churn_data$Churn)
prop.table(table(churn_data$Churn)) * 100

#Check missing values again
colSums(is.na(churn_data))

#Check duplicates
sum(duplicated(churn_data$customerID))

#Check the target variable type
class(churn_data$Churn)

churn_data$Churn <- factor(
  churn_data$Churn,
  levels = c("No", "Yes")
)
class(churn_data$Churn)

#Descriptive statistics by churn group
churn_data %>%
  group_by(Churn) %>%
  summarise(
    Count = n(),
    Mean_Tenure = mean(tenure),
    Median_Tenure = median(tenure),
    SD_Tenure = sd(tenure),
    Mean_MonthlyCharges = mean(MonthlyCharges),
    Median_MonthlyCharges = median(MonthlyCharges),
    SD_MonthlyCharges = sd(MonthlyCharges),
    Mean_TotalCharges = mean(TotalCharges),
    Median_TotalCharges = median(TotalCharges),
    SD_TotalCharges = sd(TotalCharges)
  ) %>%
  print(width = Inf, n = )

summary(churn_data[, c(
  "SeniorCitizen",
  "tenure",
  "MonthlyCharges",
  "TotalCharges"
)])


sapply(
  churn_data[, c(
    "SeniorCitizen",
    "tenure",
    "MonthlyCharges",
    "TotalCharges"
  )],
  sd
)

#Distribution and normality analysis
# Distribution of numerical variables



#Tenure
hist(
  churn_data$tenure,
  main = "Distribution of Tenure",
  xlab = "Tenure (Months)",
  ylab = "Number of Customers"
)

#Monthly Charges
hist(
  churn_data$MonthlyCharges,
  main = "Distribution of Monthly Charges",
  xlab = "Monthly Charges",
  ylab = "Number of Customers"
)

#Total Charges
hist(
  churn_data$TotalCharges,
  main = "Distribution of Total Charges",
  xlab = "Total Charges",
  ylab = "Number of Customers"
)


#Monthly Charges Boxplot
boxplot(
  churn_data$MonthlyCharges,
  main = "Boxplot of Monthly Charges",
  ylab = "Monthly Charges"
)

#Hypothesis test: Tenure vs Churn
# Wilcoxon rank-sum test: Tenure vs Churn

wilcox_tenure <- wilcox.test(
  tenure ~ Churn,
  data = churn_data
)

wilcox_tenure

# Wilcoxon rank-sum test: Monthly Charges vs Churn

wilcox_monthly <- wilcox.test(
  MonthlyCharges ~ Churn,
  data = churn_data
)

wilcox_monthly

# Chi-square test: Contract vs Churn

contract_table <- table(
  churn_data$Contract,
  churn_data$Churn
)

contract_table

chisq_contract <- chisq.test(contract_table)

chisq_contract

# Chi-square test: Internet Service vs Churn

internet_table <- table(
  churn_data$InternetService,
  churn_data$Churn
)

internet_table

chisq_internet <- chisq.test(internet_table)

chisq_internet

# Chi-square test: Payment Method vs Churn

payment_table <- table(
  churn_data$PaymentMethod,
  churn_data$Churn
)

payment_table

chisq_payment <- chisq.test(payment_table)

chisq_payment

# Chi-square test: Senior Citizen vs Churn

senior_table <- table(
  churn_data$SeniorCitizen,
  churn_data$Churn
)

senior_table

chisq_senior <- chisq.test(senior_table)

chisq_senior

# Wilcoxon rank-sum test: Total Charges vs Churn

wilcox_total <- wilcox.test(
  TotalCharges ~ Churn,
  data = churn_data
)

wilcox_total


# 8. Correlation Analysis

numeric_data <- churn_data[, c(
  "SeniorCitizen",
  "tenure",
  "MonthlyCharges",
  "TotalCharges"
)]

correlation_matrix <- cor(numeric_data)

correlation_matrix

#Next: Multicollinearity check


library(car)

# 9. Preliminary Logistic Regression Model

churn_data$SeniorCitizen <- factor(churn_data$SeniorCitizen)

preliminary_model <- glm(
  Churn ~ tenure + MonthlyCharges + TotalCharges +
    SeniorCitizen + Contract + InternetService + PaymentMethod,
  data = churn_data,
  family = binomial
)

summary(preliminary_model)
vif(preliminary_model)

# 10. Preparing Data for Predictive Modeling

model_data <- churn_data[, c(
  "Churn",
  "tenure",
  "MonthlyCharges",
  "SeniorCitizen",
  "Contract",
  "InternetService",
  "PaymentMethod"
)]

str(model_data)

# 11. Train-Test Split

set.seed(123)

train_index <- createDataPartition(
  model_data$Churn,
  p = 0.8,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

dim(train_data)
dim(test_data)


# 12. Logistic Regression Model

logistic_model <- glm(
  Churn ~ tenure + MonthlyCharges + SeniorCitizen +
    Contract + InternetService + PaymentMethod,
  data = train_data,
  family = binomial
)

summary(logistic_model)

# 13. Odds Ratios

odds_ratios <- exp(coef(logistic_model))

odds_ratios

#calculate confidence intervals
confint_model <- exp(confint(logistic_model))

confint_model


# 14. 10-Fold Cross-Validation

set.seed(123)

control <- trainControl(
  method = "cv",
  number = 10
)

cv_model <- train(
  Churn ~ tenure + MonthlyCharges + SeniorCitizen +
    Contract + InternetService + PaymentMethod,
  data = train_data,
  method = "glm",
  family = binomial,
  trControl = control
)

cv_model

# 15. Predict Churn on Test Data

test_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

test_pred <- factor(
  ifelse(test_prob >= 0.5, "Yes", "No"),
  levels = c("No", "Yes")
)

head(test_prob)
head(test_pred)

# 16. Confusion Matrix

conf_matrix <- confusionMatrix(
  test_pred,
  test_data$Churn,
  positive = "Yes"
)

conf_matrix

# 17. F1-Score

precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]

f1_score <- 2 * (precision * recall) / (precision + recall)

f1_score

# 18. ROC Curve and AUC
#install.packages("pROC")
library(pROC)

roc_obj <- roc(
  test_data$Churn,
  test_prob,
  levels = c("No", "Yes"),
  direction = "<"
)

auc_value <- auc(roc_obj)

auc_value

#create the ROC curve
plot(
  roc_obj,
  main = "ROC Curve - Logistic Regression",
  xlab = "False Positive Rate",
  ylab = "True Positive Rate"
)

abline(
  a = 0,
  b = 1,
  lty = 2
)

# 19. Model Diagnostics

par(mfrow = c(1, 1))
plot(logistic_model)
dev.off()

# 19. Threshold Optimization

thresholds <- c(0.30, 0.40, 0.50)

threshold_results <- data.frame()

for (threshold in thresholds) {
  
  pred <- factor(
    ifelse(test_prob >= threshold, "Yes", "No"),
    levels = c("No", "Yes")
  )
  
  cm <- confusionMatrix(
    pred,
    test_data$Churn,
    positive = "Yes"
  )
  
  precision <- cm$byClass["Pos Pred Value"]
  recall <- cm$byClass["Sensitivity"]
  specificity <- cm$byClass["Specificity"]
  accuracy <- cm$overall["Accuracy"]
  
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  threshold_results <- rbind(
    threshold_results,
    data.frame(
      Threshold = threshold,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      F1 = f1,
      Specificity = specificity
    )
  )
}

threshold_results
auc_value

# 20. Final Optimized Model

optimized_pred <- factor(
  ifelse(test_prob >= 0.30, "Yes", "No"),
  levels = c("No", "Yes")
)

optimized_cm <- confusionMatrix(
  optimized_pred,
  test_data$Churn,
  positive = "Yes"
)

optimized_cm

# Threshold Optimization Comparison Plot
library(tidyverse)

threshold_plot <- threshold_results %>%
  pivot_longer(
    cols = c(Accuracy, Precision, Recall, F1, Specificity),
    names_to = "Metric",
    values_to = "Value"
  )

ggplot(
  threshold_plot,
  aes(
    x = Threshold,
    y = Value,
    group = Metric,
    color = Metric
  )
) +
  geom_line(
    aes(linetype = Metric),
    linewidth = 1
  ) +
  geom_point(
    aes(shape = Metric),
    size = 3
  ) +
  scale_x_continuous(
    breaks = c(0.30, 0.40, 0.50)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    values = c(
      "Accuracy" = "#1F77B4",
      "Precision" = "#FF7F0E",
      "Recall" = "#2CA02C",
      "F1" = "#9467BD",
      "Specificity" = "#D62728"
    )
  ) +
  labs(
    title = "Classification Threshold Optimization",
    x = "Classification Threshold",
    y = "Performance",
    color = "Metric",
    linetype = "Metric",
    shape = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "right"
  )
ggsave(
  "plots/11_threshold_optimization_comparison.png",
  plot = threshold_plot,
  width = 8,
  height = 6,
  dpi = 300
)


threshold_plot_data <- threshold_results %>%
  pivot_longer(
    cols = c(Accuracy, Precision, Recall, F1, Specificity),
    names_to = "Metric",
    values_to = "Value"
  )
threshold_plot <- ggplot(
  threshold_plot_data,
  aes(
    x = Threshold,
    y = Value,
    group = Metric,
    color = Metric
  )
) +
  geom_line(
    aes(linetype = Metric),
    linewidth = 1
  ) +
  geom_point(
    aes(shape = Metric),
    size = 3
  ) +
  scale_x_continuous(
    breaks = c(0.30, 0.40, 0.50)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    values = c(
      "Accuracy" = "#1F77B4",
      "Precision" = "#FF7F0E",
      "Recall" = "#2CA02C",
      "F1" = "#9467BD",
      "Specificity" = "#D62728"
    )
  ) +
  labs(
    title = "Classification Threshold Optimization",
    x = "Classification Threshold",
    y = "Performance",
    color = "Metric",
    linetype = "Metric",
    shape = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.position = "right"
  )

threshold_plot
ggsave(
  "plots/11_threshold_optimization_comparison.png",
  plot = threshold_plot,
  width = 8,
  height = 6,
  dpi = 300
)


# Model 2 — Decision Tree
# ============================================================
# MODEL 2 — DECISION TREE
# ============================================================

# ------------------------------------------------------------
# Step 1: Load required packages
# ------------------------------------------------------------

if (!requireNamespace("rpart", quietly = TRUE)) {
  install.packages("rpart")
}

library(rpart)
library(caret)
library(pROC)
library(tidyverse)


# ------------------------------------------------------------
# Step 2: Check Train and Test Data
# ------------------------------------------------------------

dim(train_data)
dim(test_data)

# Test data must NOT have 0 rows
if (nrow(train_data) == 0 || nrow(test_data) == 0) {
  stop("Train or Test data is empty. Recreate train_data and test_data first.")
}


# ------------------------------------------------------------
# Step 3: Prepare data for Decision Tree
# ------------------------------------------------------------

tree_train <- train_data
tree_test <- test_data

# Convert categorical variables to factors
tree_train$Contract <- factor(tree_train$Contract)
tree_train$InternetService <- factor(tree_train$InternetService)
tree_train$PaymentMethod <- factor(tree_train$PaymentMethod)

# Use the same factor levels in test data
tree_test$Contract <- factor(
  tree_test$Contract,
  levels = levels(tree_train$Contract)
)

tree_test$InternetService <- factor(
  tree_test$InternetService,
  levels = levels(tree_train$InternetService)
)

tree_test$PaymentMethod <- factor(
  tree_test$PaymentMethod,
  levels = levels(tree_train$PaymentMethod)
)


# ------------------------------------------------------------
# Step 4: Train Decision Tree
# ------------------------------------------------------------

set.seed(123)

decision_tree <- train(
  Churn ~ tenure + MonthlyCharges + SeniorCitizen +
    Contract + InternetService + PaymentMethod,
  data = tree_train,
  method = "rpart",
  trControl = control,
  tuneLength = 5
)

# Display model
decision_tree


# ------------------------------------------------------------
# Step 5: Best Tree Complexity Parameter
# ------------------------------------------------------------

decision_tree$bestTune


# ------------------------------------------------------------
# Step 6: Cross-Validation Results
# ------------------------------------------------------------

decision_tree$results


# ------------------------------------------------------------
# Step 7: Predictions on Test Data
# ------------------------------------------------------------

tree_pred <- predict(
  decision_tree,
  newdata = tree_test
)

tree_prob <- predict(
  decision_tree,
  newdata = tree_test,
  type = "prob"
)[, "Yes"]

# Check predictions
head(tree_pred)
head(tree_prob)

length(tree_pred)
length(tree_prob)
nrow(tree_test)


# ------------------------------------------------------------
# Step 8: Confusion Matrix
# ------------------------------------------------------------

tree_cm <- confusionMatrix(
  tree_pred,
  tree_test$Churn,
  positive = "Yes"
)

tree_cm


# ------------------------------------------------------------
# Step 9: Calculate Performance Metrics
# ------------------------------------------------------------

tree_accuracy <- as.numeric(
  tree_cm$overall["Accuracy"]
)

tree_precision <- as.numeric(
  tree_cm$byClass["Pos Pred Value"]
)

tree_recall <- as.numeric(
  tree_cm$byClass["Sensitivity"]
)

tree_specificity <- as.numeric(
  tree_cm$byClass["Specificity"]
)

tree_f1 <- 2 * (
  tree_precision * tree_recall
) / (
  tree_precision + tree_recall
)


# ------------------------------------------------------------
# Step 10: ROC Curve and ROC-AUC
# ------------------------------------------------------------

tree_roc <- roc(
  response = tree_test$Churn,
  predictor = tree_prob,
  levels = c("No", "Yes"),
  direction = "<"
)

tree_auc <- as.numeric(
  auc(tree_roc)
)

tree_auc


# ------------------------------------------------------------
# Step 11: Display Decision Tree Metrics
# ------------------------------------------------------------

cat("\n===== DECISION TREE PERFORMANCE =====\n")

cat(
  "Accuracy     :",
  round(tree_accuracy * 100, 2),
  "%\n"
)

cat(
  "Precision    :",
  round(tree_precision * 100, 2),
  "%\n"
)

cat(
  "Recall       :",
  round(tree_recall * 100, 2),
  "%\n"
)

cat(
  "Specificity  :",
  round(tree_specificity * 100, 2),
  "%\n"
)

cat(
  "F1-Score     :",
  round(tree_f1 * 100, 2),
  "%\n"
)

cat(
  "ROC-AUC      :",
  round(tree_auc, 4),
  "\n"
)


# ------------------------------------------------------------
# Step 12: Plot ROC Curve
# ------------------------------------------------------------

plot(
  tree_roc,
  main = "ROC Curve - Decision Tree",
  col = "blue"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "red"
)


# ------------------------------------------------------------
# Step 13: Save ROC Curve
# ------------------------------------------------------------

png(
  "plots/13_decision_tree_roc_curve.png",
  width = 1200,
  height = 900,
  res = 150
)

plot(
  tree_roc,
  main = "ROC Curve - Decision Tree",
  col = "blue"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "red"
)

dev.off()


# ------------------------------------------------------------
# Step 14: Plot Decision Tree
# ------------------------------------------------------------

plot(
  decision_tree$finalModel,
  uniform = TRUE,
  margin = 0.1
)

text(
  decision_tree$finalModel,
  use.n = TRUE,
  all = TRUE,
  cex = 0.7
)


# ------------------------------------------------------------
# Step 15: Save Decision Tree Plot
# ------------------------------------------------------------

png(
  "plots/12_decision_tree.png",
  width = 1200,
  height = 900,
  res = 150
)

plot(
  decision_tree$finalModel,
  uniform = TRUE,
  margin = 0.1
)

text(
  decision_tree$finalModel,
  use.n = TRUE,
  all = TRUE,
  cex = 0.7
)

dev.off()
list.files("plots")


# ============================================================
# MODEL 3 — RANDOM FOREST
# ============================================================
# ============================================================
# MODEL 3 — RANDOM FOREST
# ============================================================


# ------------------------------------------------------------
# Step 1: Load required packages
# ------------------------------------------------------------

if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}

library(randomForest)
library(caret)
library(pROC)
library(tidyverse)


# ------------------------------------------------------------
# Step 2: Check Train and Test Data
# ------------------------------------------------------------

dim(train_data)
dim(test_data)

# Test data must NOT have 0 rows
if (nrow(train_data) == 0 || nrow(test_data) == 0) {
  stop("Train or Test data is empty. Recreate train_data and test_data first.")
}


# ------------------------------------------------------------
# Step 3: Prepare data for Random Forest
# ------------------------------------------------------------

rf_train <- train_data
rf_test <- test_data


# Convert categorical variables to factors
rf_train$Contract <- factor(rf_train$Contract)
rf_train$InternetService <- factor(rf_train$InternetService)
rf_train$PaymentMethod <- factor(rf_train$PaymentMethod)


# Use the same factor levels in test data
rf_test$Contract <- factor(
  rf_test$Contract,
  levels = levels(rf_train$Contract)
)

rf_test$InternetService <- factor(
  rf_test$InternetService,
  levels = levels(rf_train$InternetService)
)

rf_test$PaymentMethod <- factor(
  rf_test$PaymentMethod,
  levels = levels(rf_train$PaymentMethod)
)


# Check the structure
str(rf_train)
str(rf_test)


# ------------------------------------------------------------
# Step 4: Train Random Forest
# ------------------------------------------------------------

set.seed(123)

random_forest <- train(
  Churn ~ tenure + MonthlyCharges + SeniorCitizen +
    Contract + InternetService + PaymentMethod,
  data = rf_train,
  method = "rf",
  trControl = control,
  tuneLength = 3,
  importance = TRUE
)


# Display model
random_forest


# ------------------------------------------------------------
# Step 5: Best Random Forest Parameters
# ------------------------------------------------------------

random_forest$bestTune


# ------------------------------------------------------------
# Step 6: Cross-Validation Results
# ------------------------------------------------------------

random_forest$results


# ------------------------------------------------------------
# Step 7: Predictions on Test Data
# ------------------------------------------------------------

rf_pred <- predict(
  random_forest,
  newdata = rf_test
)

rf_prob <- predict(
  random_forest,
  newdata = rf_test,
  type = "prob"
)[, "Yes"]


# Check predictions
head(rf_pred)
head(rf_prob)

length(rf_pred)
length(rf_prob)
nrow(rf_test)


# ------------------------------------------------------------
# Step 8: Confusion Matrix
# ------------------------------------------------------------

rf_cm <- confusionMatrix(
  rf_pred,
  rf_test$Churn,
  positive = "Yes"
)

rf_cm


# ------------------------------------------------------------
# Step 9: Calculate Performance Metrics
# ------------------------------------------------------------

rf_accuracy <- as.numeric(
  rf_cm$overall["Accuracy"]
)

rf_precision <- as.numeric(
  rf_cm$byClass["Pos Pred Value"]
)

rf_recall <- as.numeric(
  rf_cm$byClass["Sensitivity"]
)

rf_specificity <- as.numeric(
  rf_cm$byClass["Specificity"]
)

rf_f1 <- 2 * (
  rf_precision * rf_recall
) / (
  rf_precision + rf_recall
)


# ------------------------------------------------------------
# Step 10: ROC Curve and ROC-AUC
# ------------------------------------------------------------

rf_roc <- roc(
  response = rf_test$Churn,
  predictor = rf_prob,
  levels = c("No", "Yes"),
  direction = "<"
)

rf_auc <- as.numeric(
  auc(rf_roc)
)

rf_auc


# ------------------------------------------------------------
# Step 11: Display Random Forest Metrics
# ------------------------------------------------------------

cat("\n===== RANDOM FOREST PERFORMANCE =====\n")

cat(
  "Accuracy     :",
  round(rf_accuracy * 100, 2),
  "%\n"
)

cat(
  "Precision    :",
  round(rf_precision * 100, 2),
  "%\n"
)

cat(
  "Recall       :",
  round(rf_recall * 100, 2),
  "%\n"
)

cat(
  "Specificity  :",
  round(rf_specificity * 100, 2),
  "%\n"
)

cat(
  "F1-Score     :",
  round(rf_f1 * 100, 2),
  "%\n"
)

cat(
  "ROC-AUC      :",
  round(rf_auc, 4),
  "\n"
)


# ------------------------------------------------------------
# Step 12: Variable Importance
# ------------------------------------------------------------

rf_importance <- varImp(
  random_forest,
  scale = TRUE
)

rf_importance


# ------------------------------------------------------------
# Step 13: Plot Variable Importance
# ------------------------------------------------------------

plot(
  rf_importance,
  top = 10,
  main = "Variable Importance - Random Forest"
)



# ------------------------------------------------------------
# Step 14: Save Variable Importance Plot
# ------------------------------------------------------------

png(
  "plots/14_random_forest_variable_importance.png",
  width = 1200,
  height = 900,
  res = 150
)

plot(
  rf_importance,
  top = 10,
  main = "Variable Importance - Random Forest"
)

dev.off()
file.exists("plots/14_random_forest_variable_importance.png")

# ------------------------------------------------------------
# Step 15: Plot ROC Curve
# ------------------------------------------------------------

plot(
  rf_roc,
  main = "ROC Curve - Random Forest",
  col = "blue"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "red"
)


# ------------------------------------------------------------
# Step 16: Save ROC Curve
# ------------------------------------------------------------

png(
  "plots/15_random_forest_roc_curve.png",
  width = 1200,
  height = 900,
  res = 150
)

plot(
  rf_roc,
  main = "ROC Curve - Random Forest",
  col = "blue"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "red"
)

dev.off()


# ------------------------------------------------------------
# Step 17: Random Forest Model Summary
# ------------------------------------------------------------

print(random_forest$finalModel)


# ------------------------------------------------------------
# Step 18: Final Random Forest Results
# ------------------------------------------------------------

cat("\n============================================\n")
cat("       FINAL RANDOM FOREST RESULTS\n")
cat("============================================\n")

cat(
  "Best mtry       :",
  random_forest$bestTune$mtry,
  "\n"
)

cat(
  "Accuracy        :",
  round(rf_accuracy * 100, 2),
  "%\n"
)

cat(
  "Precision       :",
  round(rf_precision * 100, 2),
  "%\n"
)

cat(
  "Recall          :",
  round(rf_recall * 100, 2),
  "%\n"
)

cat(
  "Specificity     :",
  round(rf_specificity * 100, 2),
  "%\n"
)

cat(
  "F1-Score        :",
  round(rf_f1 * 100, 2),
  "%\n"
)

cat(
  "ROC-AUC         :",
  round(rf_auc, 4),
  "\n"
)

cat("============================================\n")



# ============================================================
# MODEL 4 — GRADIENT BOOSTING
# ============================================================


# ------------------------------------------------------------
# Step 1: Load required packages
# ------------------------------------------------------------

if (!requireNamespace("gbm", quietly = TRUE)) {
  install.packages("gbm")
}

library(gbm)
library(caret)
library(pROC)
library(tidyverse)


# ------------------------------------------------------------
# Step 2: Check Train and Test Data
# ------------------------------------------------------------

dim(train_data)
dim(test_data)

# Test data must NOT have 0 rows
if (nrow(train_data) == 0 || nrow(test_data) == 0) {
  stop("Train or Test data is empty. Recreate train_data and test_data first.")
}


# ------------------------------------------------------------
# Step 3: Prepare data for Gradient Boosting
# ------------------------------------------------------------

gb_train <- train_data
gb_test <- test_data


# Convert categorical variables to factors
gb_train$Contract <- factor(gb_train$Contract)
gb_train$InternetService <- factor(gb_train$InternetService)
gb_train$PaymentMethod <- factor(gb_train$PaymentMethod)


# Use the same factor levels in test data
gb_test$Contract <- factor(
  gb_test$Contract,
  levels = levels(gb_train$Contract)
)

gb_test$InternetService <- factor(
  gb_test$InternetService,
  levels = levels(gb_train$InternetService)
)

gb_test$PaymentMethod <- factor(
  gb_test$PaymentMethod,
  levels = levels(gb_train$PaymentMethod)
)


# Check the structure
str(gb_train)
str(gb_test)


# ------------------------------------------------------------
# Step 4: Train Gradient Boosting Model
# ------------------------------------------------------------

set.seed(123)

gradient_boosting <- train(
  Churn ~ tenure + MonthlyCharges + SeniorCitizen +
    Contract + InternetService + PaymentMethod,
  data = gb_train,
  method = "gbm",
  trControl = control,
  tuneLength = 3,
  verbose = FALSE
)


# Display model
gradient_boosting


# ------------------------------------------------------------
# Step 5: Best Gradient Boosting Parameters
# ------------------------------------------------------------

gradient_boosting$bestTune


# ------------------------------------------------------------
# Step 6: Cross-Validation Results
# ------------------------------------------------------------

gradient_boosting$results


# ------------------------------------------------------------
# Step 7: Predictions on Test Data
# ------------------------------------------------------------

gb_pred <- predict(
  gradient_boosting,
  newdata = gb_test
)

gb_prob <- predict(
  gradient_boosting,
  newdata = gb_test,
  type = "prob"
)[, "Yes"]


# Check predictions
head(gb_pred)
head(gb_prob)

length(gb_pred)
length(gb_prob)
nrow(gb_test)


# ------------------------------------------------------------
# Step 8: Confusion Matrix
# ------------------------------------------------------------

gb_cm <- confusionMatrix(
  gb_pred,
  gb_test$Churn,
  positive = "Yes"
)

gb_cm


# ------------------------------------------------------------
# Step 9: Calculate Performance Metrics
# ------------------------------------------------------------

gb_accuracy <- as.numeric(
  gb_cm$overall["Accuracy"]
)

gb_precision <- as.numeric(
  gb_cm$byClass["Pos Pred Value"]
)

gb_recall <- as.numeric(
  gb_cm$byClass["Sensitivity"]
)

gb_specificity <- as.numeric(
  gb_cm$byClass["Specificity"]
)

gb_f1 <- 2 * (
  gb_precision * gb_recall
) / (
  gb_precision + gb_recall
)


# ------------------------------------------------------------
# Step 10: ROC Curve and ROC-AUC
# ------------------------------------------------------------

gb_roc <- roc(
  response = gb_test$Churn,
  predictor = gb_prob,
  levels = c("No", "Yes"),
  direction = "<"
)

gb_auc <- as.numeric(
  auc(gb_roc)
)

gb_auc


# ------------------------------------------------------------
# Step 11: Display Gradient Boosting Metrics
# ------------------------------------------------------------

cat("\n===== GRADIENT BOOSTING PERFORMANCE =====\n")

cat(
  "Accuracy     :",
  round(gb_accuracy * 100, 2),
  "%\n"
)

cat(
  "Precision    :",
  round(gb_precision * 100, 2),
  "%\n"
)

cat(
  "Recall       :",
  round(gb_recall * 100, 2),
  "%\n"
)

cat(
  "Specificity  :",
  round(gb_specificity * 100, 2),
  "%\n"
)

cat(
  "F1-Score     :",
  round(gb_f1 * 100, 2),
  "%\n"
)

cat(
  "ROC-AUC      :",
  round(gb_auc, 4),
  "\n"
)


# ------------------------------------------------------------
# Step 12: Variable Importance
# ------------------------------------------------------------

gb_importance <- varImp(
  gradient_boosting,
  scale = TRUE
)

gb_importance


# ------------------------------------------------------------
# Step 13: Plot Variable Importance
# ------------------------------------------------------------

plot(
  gb_importance,
  top = 10,
  main = "Variable Importance - Gradient Boosting"
)


# ------------------------------------------------------------
# Step 14: Save Variable Importance Plot
# ------------------------------------------------------------

png(
  "plots/16_gradient_boosting_variable_importance.png",
  width = 1200,
  height = 900,
  res = 150
)

plot(
  gb_importance,
  top = 10,
  main = "Variable Importance - Gradient Boosting"
)

dev.off()


# ------------------------------------------------------------
# Step 15: Plot ROC Curve
# ------------------------------------------------------------

plot(
  gb_roc,
  main = "ROC Curve - Gradient Boosting",
  col = "blue"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "red"
)


# ------------------------------------------------------------
# Step 16: Save ROC Curve
# ------------------------------------------------------------

png(
  "plots/17_gradient_boosting_roc_curve.png",
  width = 1200,
  height = 900,
  res = 150
)

plot(
  gb_roc,
  main = "ROC Curve - Gradient Boosting",
  col = "blue"
)

abline(
  a = 0,
  b = 1,
  lty = 2,
  col = "red"
)

dev.off()


# ------------------------------------------------------------
# Step 17: Gradient Boosting Model Summary
# ------------------------------------------------------------

print(gradient_boosting$finalModel)


# ------------------------------------------------------------
# Step 18: Final Gradient Boosting Results
# ------------------------------------------------------------

cat("\n============================================\n")
cat("       FINAL GRADIENT BOOSTING RESULTS\n")
cat("============================================\n")

cat(
  "Best n.trees   :",
  gradient_boosting$bestTune$n.trees,
  "\n"
)

cat(
  "Best interaction.depth :",
  gradient_boosting$bestTune$interaction.depth,
  "\n"
)

cat(
  "Best shrinkage :",
  gradient_boosting$bestTune$shrinkage,
  "\n"
)

cat(
  "Best n.minobsinnode :",
  gradient_boosting$bestTune$n.minobsinnode,
  "\n"
)

cat(
  "Accuracy        :",
  round(gb_accuracy * 100, 2),
  "%\n"
)

cat(
  "Precision       :",
  round(gb_precision * 100, 2),
  "%\n"
)

cat(
  "Recall          :",
  round(gb_recall * 100, 2),
  "%\n"
)

cat(
  "Specificity     :",
  round(gb_specificity * 100, 2),
  "%\n"
)

cat(
  "F1-Score        :",
  round(gb_f1 * 100, 2),
  "%\n"
)

cat(
  "ROC-AUC         :",
  round(gb_auc, 4),
  "\n"
)

cat("============================================\n")


# ============================================================
# FINAL MODEL COMPARISON
# ============================================================


# ------------------------------------------------------------
# Step 1: Create consolidated model comparison table
# ------------------------------------------------------------

model_comparison <- data.frame(
  
  Model = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest",
    "Gradient Boosting"
  ),
  
  Accuracy = c(
    79.32,
    78.39,
    80.24,
    79.67
  ),
  
  Precision = c(
    64.24,
    65.90,
    68.77,
    65.93
  ),
  
  Recall = c(
    49.60,
    38.34,
    46.65,
    48.26
  ),
  
  Specificity = c(
    90.04,
    92.84,
    92.36,
    91.01
  ),
  
  F1_Score = c(
    55.98,
    48.47,
    55.59,
    55.73
  ),
  
  ROC_AUC = c(
    0.8282,
    0.7037,
    0.8110,
    0.8342
  )
)


# Display table
print(model_comparison)

# ------------------------------------------------------------
# Step 2: Round values for reporting
# ------------------------------------------------------------

model_comparison_report <- model_comparison

model_comparison_report$Accuracy <- round(
  model_comparison_report$Accuracy,
  2
)

model_comparison_report$Precision <- round(
  model_comparison_report$Precision,
  2
)

model_comparison_report$Recall <- round(
  model_comparison_report$Recall,
  2
)

model_comparison_report$Specificity <- round(
  model_comparison_report$Specificity,
  2
)

model_comparison_report$F1_Score <- round(
  model_comparison_report$F1_Score,
  2
)

model_comparison_report$ROC_AUC <- round(
  model_comparison_report$ROC_AUC,
  4
)

print(model_comparison_report)
# ------------------------------------------------------------
# Step 3: Save model comparison as CSV
# ------------------------------------------------------------

write_csv(
  model_comparison_report,
  "reports/final_model_comparison.csv"
)

file.exists(
  "reports/final_model_comparison.csv"
)
getwd()
setwd("C:/Users/ACER1/Documents/Customer_Churn_Analysis")
getwd()
dir.exists("reports")
# ------------------------------------------------------------
# Step 4: Prepare data for comparison chart
# ------------------------------------------------------------

model_comparison_long <- model_comparison %>%
  pivot_longer(
    cols = c(
      Accuracy,
      Precision,
      Recall,
      Specificity,
      F1_Score,
      ROC_AUC
    ),
    names_to = "Metric",
    values_to = "Score"
  )

print(model_comparison_long, n = 24)

# ------------------------------------------------------------
# Step 5: Create final model comparison chart
# ------------------------------------------------------------

model_comparison_plot <- ggplot(
  model_comparison_long,
  aes(
    x = Model,
    y = Score,
    fill = Metric
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Comparison of Churn Prediction Models",
    x = "Model",
    y = "Performance Score (%)",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

model_comparison_plot

# ------------------------------------------------------------
# Step 6: Convert ROC-AUC to percentage for chart
# ------------------------------------------------------------

model_comparison_chart <- model_comparison %>%
  mutate(
    ROC_AUC = ROC_AUC * 100
  ) %>%
  pivot_longer(
    cols = c(
      Accuracy,
      Precision,
      Recall,
      Specificity,
      F1_Score,
      ROC_AUC
    ),
    names_to = "Metric",
    values_to = "Score"
  )

print(model_comparison_chart,n = 24)

# ------------------------------------------------------------
# Step 7: Create final comparison chart
# ------------------------------------------------------------

model_comparison_plot <- ggplot(
  model_comparison_chart,
  aes(
    x = Model,
    y = Score,
    fill = Metric
  )
) +
  geom_col(
    position = "dodge"
  ) +
  labs(
    title = "Comparison of Churn Prediction Models",
    x = "Model",
    y = "Performance (%)",
    fill = "Metric"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

model_comparison_plot

# ------------------------------------------------------------
# Step 8: Save final model comparison chart
# ------------------------------------------------------------

png(
  "plots/18_model_comparison.png",
  width = 1600,
  height = 1000,
  res = 150
)

print(model_comparison_plot)

dev.off()


# Check file
file.exists(
  "plots/18_model_comparison.png"
)

# ------------------------------------------------------------
# Step 9: Best model for each performance metric
# ------------------------------------------------------------

cat("\n===== BEST MODEL BY METRIC =====\n")

cat(
  "Best Accuracy    :",
  model_comparison$Model[
    which.max(model_comparison$Accuracy)
  ],
  "\n"
)

cat(
  "Best Precision   :",
  model_comparison$Model[
    which.max(model_comparison$Precision)
  ],
  "\n"
)

cat(
  "Best Recall      :",
  model_comparison$Model[
    which.max(model_comparison$Recall)
  ],
  "\n"
)

cat(
  "Best Specificity :",
  model_comparison$Model[
    which.max(model_comparison$Specificity)
  ],
  "\n"
)

cat(
  "Best F1-Score    :",
  model_comparison$Model[
    which.max(model_comparison$F1_Score)
  ],
  "\n"
)

cat(
  "Best ROC-AUC     :",
  model_comparison$Model[
    which.max(model_comparison$ROC_AUC)
  ],
  "\n"
)
