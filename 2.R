library(foreign)
library(haven)
library(glmnet)
library(caret)
library(pROC)
library(ROCR)

# Replace with the actual path of your SPSS file
data<- read_sav("C:/Users/Lenovo/Documents/桌面原本/TBI.sav")

# Step 2: Isolate 'd.unfav' and remove 'd.gos' and 'd.mort'
data <- data[, !(names(data) %in% c('d.gos', 'd.mort'))]

# Step 3: Remove redundant predictors
data <- data[, !(names(data) %in% c('hb', 'sodium', 'glucose', 'pupil.i'))]

# Step 4: Format categorical variables as factors
data$trial <- as.factor(data$trial)
data$d.unfav <- as.factor(data$d.unfav)
data$cause <- as.factor(data$cause)

# Step 5: Remove cases with missing data
data <- na.omit(data)

# Step 6: Present summary statistics
summary(data)


#2

# Set seed for reproducibility
set.seed(456)

# Split the data into training and test sets based on the trial variable
# Training data from the International trial (trial 74)
# Test data from the US trial (trial 75)
train_data <- subset(data, trial == 74)[, -which(names(data) == "trial")]
test_data <- subset(data, trial == 75)[, -which(names(data) == "trial")]

# Prepare training data for the model
X_train <- as.matrix(train_data[, -which(names(train_data) == "d.unfav")])
X_train <- na.omit(X_train)
y_train <- train_data$d.unfav

# Prepare test data for the model
X_test <- as.matrix(test_data[, -which(names(test_data) == "d.unfav")])
X_test<- na.omit(X_test)
y_test <- test_data$d.unfav

# Fit the Lasso regression model using glmnet package
lasso_model <- glmnet(X_train, y_train,nlambda=100, family = "binomial", alpha = 1)

# Determine the optimal lambda value using cross-validation
cv_model <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)
optimal_lambda <- cv_model$lambda.min

#Extract coefficients
lasso_coefficients <- coef(lasso_model, s = optimal_lambda)
print(lasso_coefficients)

#3.1
# Predict on the test data using the Lasso model
predicted <- predict(lasso_model, newx = X_test, s = optimal_lambda, type = "response")
pred.class <- as.integer(predicted > 0.5) 

# Create and print a confusion matrix of the predictions
cft1 <- table(pred.class, y_test) #先prediction value 后label
confusion<-confusionMatrix(cft1, positive = "1") #使用上面的table

# Calculate AUC for the model using pROC package
roc_apparent <- roc(y_test, pred.class)
auc_apparent <- auc(roc_apparent)

# Create a confusion matrix for the apparent performance
confusion_apparent <- table(y_test, as.integer( predicted >= 0.5))
confusioned_apparent<-confusionMatrix(confusion_apparent) #使用上面的table

# Extract and store various performance metrics from the confusion matrix
accuracy_apparent <- confusioned_apparent$overall['Accuracy']
sensitivity_apparent <- confusioned_apparent$byClass['Sensitivity']
specificity_apparent <- confusioned_apparent$byClass['Specificity']
ppv_apparent <- confusioned_apparent$byClass['Pos Pred Value']
npv_apparent <- confusioned_apparent$byClass['Neg Pred Value']

# Create a data frame to hold the performance metrics for apparent validation
table0 <- data.frame(
  Metric = c("AUC", "Overall Accuracy", "Sensitivity", "Specificity", "Positive Predictive Value", "Negative Predictive Value"),
  Value = c(auc_apparent, accuracy_apparent, sensitivity_apparent, specificity_apparent, ppv_apparent, npv_apparent)
)

#3.2
# Internal Validation using 10-fold cross-validation with 10 repeats
control <- trainControl(method="repeatedcv", number=10, repeats=10, savePredictions = "final",classProbs=TRUE, summaryFunction=twoClassSummary)

# Convert y_train to a factor and set levels for binary classification
y_train_factor <- as.factor(y_train)
levels(y_train_factor) <- c("Class0", "Class1")

# Train the model using caret package, which will automatically perform cross-validation
fit <- train(X_train, y_train_factor, method="glmnet", trControl=control, tuneLength=10, metric="ROC")
results <- fit$results


# Initialize vectors to store metrics from each fold
auc_values <- c()
accuracy_values <- c()
sensitivity_values <- c()
specificity_values <- c()
ppv_values <- c()  # Positive Predictive Values
npv_values <- c()  # Negative Predictive Values

# Extract predictions from each fold
prediction_matrics <- fit$pred

# Initialize a list
confusion_matrices <- list()



# Calculate metrics for each fold
for(i in unique(prediction_matrics$Resample)){
  subset_pred <- prediction_matrics[prediction_matrics$Resample == i,]
  obs <- factor(subset_pred$obs, levels = levels(subset_pred$pred))
  
  # Calculate AUC for each fold
  roc_result <- roc(obs, as.numeric(subset_pred$pred))
  auc_values <- c(auc_values, auc(roc_result))
  
  # Calculate confusion matrix and other metrics for each fold
  cm <- confusionMatrix(subset_pred$pred, obs)
  accuracy_values <- c(accuracy_values, cm$overall['Accuracy'])
  sensitivity_values <- c(sensitivity_values, cm$byClass['Sensitivity'])
  specificity_values <- c(specificity_values, cm$byClass['Specificity'])
  ppv_values <- c(ppv_values, cm$byClass['Pos Pred Value'])
  npv_values <- c(npv_values, cm$byClass['Neg Pred Value'])
}

# Calculate the mean of the metrics
mean_auc <- mean(auc_values)
mean_accuracy <- mean(accuracy_values)
mean_sensitivity <- mean(sensitivity_values)
mean_specificity <- mean(specificity_values)
mean_ppv <- mean(ppv_values)
mean_npv <- mean(npv_values)

# Create a data frame 
table1 <- data.frame(
  Metric = c("AUC", "Overall Accuracy", "Sensitivity", "Specificity", "Positive Predictive Value", "Negative Predictive Value"),
  Value = c(mean_auc, mean_accuracy, mean_sensitivity, mean_specificity, mean_ppv, mean_npv)
)

# External Validation
# Predict on the test set using the fitted model
predictions <- predict(fit, X_test,type = "prob")
pred.class2 <- as.integer(predictions[,2] > 0.5) #这里假设prediciton>0.5的预测为1，其余为0.
print(cft2 <- table(pred.class2, y_test)) #先prediction value 后label

# Calculate the confusion matrix for the external validation
confusion2<-confusionMatrix(cft2, positive = "1") #使用上面的table

# Calculate AUC 
roc_result <- roc(response = y_test, predictor = as.numeric(predictions[,2]))
auc <- auc(roc_result)

accuracy <- confusion2$overall['Accuracy']
sensitivity <- confusion2$byClass['Sensitivity']
specificity <- confusion2$byClass['Specificity']
ppv <- confusion2$byClass['Pos Pred Value']
npv <- confusion2$byClass['Neg Pred Value']

table2 <- data.frame(
  Metric = c("AUC", "Overall Accuracy", "Sensitivity", "Specificity", "Positive Predictive Value", "Negative Predictive Value"),
  Value = c(auc, accuracy, sensitivity, specificity, ppv, npv)
)


combined_table <- Reduce(function(x, y) merge(x, y, by = "Metric", all = TRUE), list(table0, table1, table2))
names(combined_table)[names(combined_table) == "Value.x"] <- "Apparent"
names(combined_table)[names(combined_table) == "Value.y"] <- "Internal"
names(combined_table)[names(combined_table) == "Value"] <- "External"

# Print the combined table with all metrics for Apparent, Internal, and External validation
print(combined_table)


