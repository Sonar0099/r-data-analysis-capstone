# Week 4 Capstone: Comprehensive Data Analysis Reporting & Pipeline
# Focus: End-to-End Pipeline (Cleaning, Visualization, Modeling & Evaluation)

library(dplyr)
library(ggplot2)

# 1. Dataset Loading (Cleaned In-Memory Titanic Subset)
titanic_data <- data.frame(
  PassengerId = 1:20,
  Survived = c(0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1),
  Pclass = c(3, 1, 3, 1, 3, 3, 1, 3, 3, 2, 3, 1, 3, 3, 3, 2, 3, 2, 3, 3),
  Sex = c("male", "female", "female", "female", "male", "male", "male", "male", 
          "female", "female", "female", "female", "male", "male", "female", 
          "female", "male", "male", "female", "female"),
  Age = c(22, 38, 26, 35, 35, 28, 54, 2, 27, 14, 4, 58, 20, 39, 14, 55, 2, 28, 31, 28),
  Fare = c(7.25, 71.28, 7.92, 53.1, 8.05, 8.45, 51.86, 21.07, 11.13, 30.07, 
           16.7, 26.55, 8.05, 31.27, 7.85, 16.0, 29.12, 13.0, 18.0, 65.0),
  stringsAsFactors = FALSE
)

# Factor Encodings
titanic_data$Survived <- factor(titanic_data$Survived, levels = c(0, 1), labels = c("No", "Yes"))
titanic_data$Pclass <- factor(titanic_data$Pclass, levels = c(1, 2, 3), labels = c("1st", "2nd", "3rd"))
titanic_data$Sex <- factor(titanic_data$Sex)

cat("=== 1. Data Cleaning Verification ===\n")
print(summary(titanic_data[, c("Age", "Fare", "Pclass", "Sex", "Survived")]))

# 2. Exploratory Insight Summary
cat("\n=== 2. Cross-Tabulation: Survival by Gender & Class ===\n")
print(round(prop.table(table(titanic_data$Sex, titanic_data$Survived), margin = 1) * 100, 2))

# 3. Model Training & Evaluation
set.seed(42)
train_idx <- sample(1:nrow(titanic_data), size = 0.75 * nrow(titanic_data))
train_set <- titanic_data[train_idx, ]
test_set  <- titanic_data[-train_idx, ]

# Fit Logistic Regression Model
final_model <- glm(Survived ~ Sex + Pclass + Age, data = train_set, family = binomial())
cat("\n=== 3. Final Predictive Model Coefficients ===\n")
print(summary(final_model)$coefficients)

# Model Performance on Test Split
test_set$prob <- predict(final_model, newdata = test_set, type = "response")
test_set$pred <- ifelse(test_set$prob > 0.5, "Yes", "No")
conf_matrix <- table(Actual = test_set$Survived, Predicted = test_set$pred)

cat("\n=== 4. Final Evaluation Matrix ===\n")
print(conf_matrix)

# 4. Final Visualization
p_cap <- ggplot(titanic_data, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill", color = "white", width = 0.6) +
  facet_wrap(~Sex) +
  scale_fill_manual(values = c("No" = "#E74C3C", "Yes" = "#2ECC71")) +
  labs(title = "Comprehensive Survival Landscape (Class & Gender)",
       subtitle = "Capstone Synthesis: Evaluating Intersectional Demographic Survival",
       x = "Passenger Class", y = "Survival Proportion", fill = "Survived") +
  theme_minimal()

print(p_cap)
