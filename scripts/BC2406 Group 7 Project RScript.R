library(data.table)
library(ggplot2)
library(caTools)
library(rpart)
library(rpart.plot)
library(pROC)
library(car)

setwd("C:/Users/junha/OneDrive/Documents/BC2406 Analytics I Visual & Predictive Techniques/Group Project")

##1. Data =====
set.seed(1)

dt <- fread("diabetes_prediction_dataset.csv")
dt$diabetes <- factor(dt$diabetes)
dt$gender <- factor(dt$gender)
dt$hypertension <- factor(dt$hypertension)
dt$heart_disease <- factor(dt$heart_disease)
dt$smoking_history[dt$smoking_history %in% c("ever", "former", "not current")] <- "past"
dt$smoking_history <- factor(dt$smoking_history)

summary(dt)
# 91500 without diabetes, only 8500 with diabetes

colSums(is.na(dt))
dt1 <- dt[dt$smoking_history != "No Info",]
dt1$smoking_history <- droplevels(dt1$smoking_history)
summary(dt1)
# No missing value in all columns

sum(duplicated(dt1))
# 4032 duplicate data

dt2 <- dt1[!duplicated(dt1),]
sum(duplicated(dt2))
summary(dt2)
# All duplicate data removed

ggplot(dt2, aes(y = age)) + geom_boxplot() + labs(title = "Age")
ggplot(dt2, aes(y = bmi)) + geom_boxplot() + labs(title = "BMI")
ggplot(dt2, aes(y = HbA1c_level)) + geom_boxplot() + labs(title = "HbA1c Level")
ggplot(dt2, aes(y = blood_glucose_level)) + geom_boxplot() + labs(title = "Blood Glucose Level")
# bmi, HbA1c and blood glucose level have outliers

dt3 <- subset(
  dt2,
  bmi >= quantile(bmi, 0.25) - 1.5 * IQR(bmi) &
  bmi <= quantile(bmi, 0.75) + 1.5 * IQR(bmi) &
  HbA1c_level >= quantile(HbA1c_level, 0.25) - 1.5 * IQR(HbA1c_level) &
  HbA1c_level <= quantile(HbA1c_level, 0.75) + 1.5 * IQR(HbA1c_level) &
  blood_glucose_level >= quantile(blood_glucose_level, 0.25) - 1.5 * IQR(blood_glucose_level) &
  blood_glucose_level <= quantile(blood_glucose_level, 0.75) + 1.5 * IQR(blood_glucose_level)
)

##2. Outliers =====
ggplot(dt3, aes(y = age)) + geom_boxplot() + labs(title = "Age")
ggplot(dt3, aes(y = bmi)) + geom_boxplot() + labs(title = "BMI")
ggplot(dt3, aes(y = HbA1c_level)) + geom_boxplot() + labs(title = "HbA1c Level")
ggplot(dt3, aes(y = blood_glucose_level)) + geom_boxplot() + labs(title = "Blood Glucose Level")
summary(dt3)
# Most outliers removed

diabetes <- dt3[dt3$diabetes == 1,]
no_diabetes <- dt3[dt3$diabetes == 0,]
no_diabetes_sample <- no_diabetes[sample(nrow(no_diabetes), 3974),]
sample_dt <- rbind(diabetes, no_diabetes_sample)
summary(sample_dt)

prop.table(table(sample_dt$gender, sample_dt$diabetes))
prop.table(table(sample_dt$hypertension, sample_dt$diabetes))
prop.table(table(sample_dt$heart_disease, sample_dt$diabetes))
prop.table(table(sample_dt$smoking_history, sample_dt$diabetes))

sample_dt[, .(avg_age = mean(age), avg_bmi = mean(bmi), avg_HbA1c = mean(HbA1c_level), avg_blood_glucose = mean(blood_glucose_level)), diabetes]



#3. GGPLOT VISUALISATION ====

#univariate analysis visualising
ggplot(sample_dt, aes(x = smoking_history, y = diabetes))+geom_jitter()+labs(title = "Jitter plot of diabetes against smoking history")

ggplot(sample_dt, aes(x = gender, y = diabetes))+geom_jitter()+labs(title = "Jitter plot of diabetes against gender")

ggplot(sample_dt, aes(x = age, y = diabetes))+geom_boxplot()+labs(title = "Boxplot of diabetes against age")

ggplot(sample_dt, aes(x = bmi, y = diabetes))+geom_boxplot()+labs(title = "Boxplot of diabetes against bmi")

ggplot(sample_dt, aes(x = hypertension, y = diabetes))+geom_jitter()+labs(title = "Jitter plot of diabetes against hypertension ")

ggplot(sample_dt, aes(x= heart_disease, y = diabetes))+geom_jitter()+labs(title = "Jitter plot of diabetes against heart disease")

ggplot(sample_dt, aes(x = HbA1c_level, y = diabetes))+geom_boxplot()+labs(title = "Boxplot of diabetes against HbA1c level")

ggplot(sample_dt, aes(x = blood_glucose_level, y = diabetes))+geom_boxplot()+labs(title = "Boxplot of diabetes against blood glucose level")

#bivariate analysis visualisation
#smoking history
ggplot(sample_dt, aes(x = age, y = diabetes, col = smoking_history ))+geom_boxplot()+labs(title = "Boxplot of diabetes against age by smoking_history")

ggplot(sample_dt, aes(x = bmi, y = diabetes, col = smoking_history ))+geom_boxplot()+labs(title = "Boxplot of diabetes against bmi by smoking_history")

ggplot(sample_dt, aes(x = HbA1c_level, y = diabetes, col = smoking_history ))+geom_boxplot()+labs(title = "Boxplot of diabetes against HbA1c_level by smoking_history")


ggplot(sample_dt, aes(x = blood_glucose_level, y = diabetes, col = smoking_history ))+geom_boxplot()+labs(title = "Boxplot of diabetes against blood glucose level by smoking_history")
#interesting?

#gender
ggplot(sample_dt, aes(x = age, y = diabetes, col = gender ))+geom_boxplot()+labs(title = "Boxplot of diabetes against age by gender")

ggplot(sample_dt, aes(x = bmi, y = diabetes, col = gender ))+geom_boxplot()+labs(title = "Boxplot of diabetes against bmi by gender")

ggplot(sample_dt, aes(x = HbA1c_level, y = diabetes, col = gender ))+geom_boxplot()+labs(title = "Boxplot of diabetes against HbA1c_level by gender")


ggplot(sample_dt, aes(x= blood_glucose_level, y = diabetes, col = gender))+geom_boxplot()+labs(title="Boxplot of diabetes against blood glucose level by gender")

#hypertension
ggplot(sample_dt, aes(x = age, y = diabetes, col = hypertension ))+geom_boxplot()+labs(title = "Boxplot of diabetes against age by hypertension")

ggplot(sample_dt, aes(x = bmi, y = diabetes, col = hypertension ))+geom_boxplot()+labs(title = "Boxplot of diabetes against bmi by hypertension")

ggplot(sample_dt, aes(x = HbA1c_level, y = diabetes, col = hypertension ))+geom_boxplot()+labs(title = "Boxplot of diabetes against HbA1c_level by hypertension")


ggplot(sample_dt, aes(x= blood_glucose_level, y = diabetes, col = hypertension))+geom_boxplot()+labs(title="Boxplot of diabetes against blood glucose level by hypertension")

#heartdisease
ggplot(sample_dt, aes(x = age, y = diabetes, col = heart_disease ))+geom_boxplot()+labs(title = "Boxplot of diabetes against age by heart_disease")

ggplot(sample_dt, aes(x = bmi, y = diabetes, col = hypertension ))+geom_boxplot()+labs(title = "Boxplot of diabetes against bmi by heart_disease")

ggplot(sample_dt, aes(x = HbA1c_level, y = diabetes, col = hypertension ))+geom_boxplot()+labs(title = "Boxplot of diabetes against HbA1c_level by heart_disease")


ggplot(sample_dt, aes(x= blood_glucose_level, y = diabetes, col = heart_disease))+geom_boxplot()+labs(title="Boxplot of diabetes against blood glucose level by heart_disease")


##4. CART =====



# Split the dataset into 70% training and 30% testing
set.seed(1)
train <- sample.split(sample_dt$diabetes, SplitRatio = 0.7)
trainset <- subset(sample_dt, train == TRUE)
testset <- subset(sample_dt, train == FALSE)

# Build the CART model on the training set
cart <- rpart(diabetes ~ ., data = trainset, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
plotcp(cart)

# Plot the CART model
rpart.plot(cart, cex = 0.7, extra = 101)


# Store the cp table for pruning
cp.table <- data.table(cart$cptable)



# Create an index column for easy reference
cp.table[, index := .I]

# Find the index of the minimum cross-validation error
mini <- min(cp.table[xerror == min(xerror), index])

# Calculate the error cap
errorcap <- cp.table[index == mini, xerror + xstd]

# Find the optimal cp index where xerror is below the error cap
optimal_cp_index <- min(cp.table[xerror < errorcap, index])

# Compute the geometric mean of cp for that optimal index and the one before it
cp.opt <- sqrt(cp.table[index == optimal_cp_index, CP] * cp.table[index == optimal_cp_index - 1, CP])

# Prune the CART model using the optimal cp
m.opt <- prune(cart, cp.opt)

# Plot the pruned model
rpart.plot(m.opt)

# Predict on the Test Data using the pruned model
opt.pred <- predict(m.opt, newdata = testset, type = 'class')

# Create the confusion matrix for the pruned tree
conf_matrix_cart <- table(actual = testset$diabetes, model.pred = opt.pred)
print(conf_matrix_cart)

# Calculate the error rates
false_positive_rate_cart <- conf_matrix_cart[2, 1] / sum(conf_matrix_cart[2, 1], conf_matrix_cart[1, 1])
false_negative_rate_cart <- conf_matrix_cart[1, 2] / sum(conf_matrix_cart[1, 2], conf_matrix_cart[2, 2])
overall_error_rate_cart <- sum(conf_matrix_cart[1, 2], conf_matrix_cart[2, 1]) / sum(conf_matrix_cart)

cat("CART Model - False Positive Rate:", false_positive_rate_cart, "\n")
cat("CART Model - False Negative Rate:", false_negative_rate_cart, "\n")
cat("CART Model - Overall Error Rate:", overall_error_rate_cart, "\n")

## 5. Logistic ====
# Step 1: Split the data into a 70-30 train-test split
set.seed(1)
train <- sample.split(sample_dt$diabetes, SplitRatio = 0.7)
trainset <- subset(sample_dt, train == TRUE)
testset <- subset(sample_dt, train == FALSE)

# Step 2: Full Logistic Regression Model
log_model <- glm(diabetes ~ ., data = trainset, family = "binomial")
vif(log_model)

# Step 3: Predictions on the Test Data for Full Model
log_pred_full <- predict(log_model, newdata = testset, type = "response")
log_pred_full_class <- ifelse(log_pred_full > 0.5, 1, 0)
summary(log_model)
# Step 4: Confusion Matrix and Evaluation Metrics for Full Model
full_conf_matrix <- table(Predicted = log_pred_full_class, Actual = testset$diabetes)
print("Confusion Matrix (Full Model):")
print(full_conf_matrix)

# Calculate False Positive Rate, False Negative Rate, and Overall Error for Full Model
full_false_positive_rate <- full_conf_matrix[2, 1] / sum(full_conf_matrix[2, 1], full_conf_matrix[1, 1])
full_false_negative_rate <- full_conf_matrix[1, 2] / sum(full_conf_matrix[1, 2], full_conf_matrix[2, 2])
full_overall_error_rate <- sum(full_conf_matrix[1, 2], full_conf_matrix[2, 1]) / sum(full_conf_matrix)

cat("Full Model - False Positive Rate:", full_false_positive_rate, "\n")
cat("Full Model - False Negative Rate:", full_false_negative_rate, "\n")
cat("Full Model - Overall Error Rate:", full_overall_error_rate, "\n")

# Step 5: Reduced Logistic Regression Model using stepwise selection (backward elimination)
log_model_step <- step(log_model, direction = "both")
vif(log_model_step)
# Step 6: Summary of Reduced Model
cat("\n=== REDUCED MODEL SUMMARY ===\n")
summary(log_model_step)

# Step 7: Predictions on the Test Data for Reduced Model
log_pred_reduced <- predict(log_model_step, newdata = testset, type = "response")
log_pred_reduced_class <- ifelse(log_pred_reduced > 0.5, 1, 0)

# Step 8: Confusion Matrix and Evaluation Metrics for Reduced Model
reduced_conf_matrix <- table(Predicted = log_pred_reduced_class, Actual = testset$diabetes)
print("Confusion Matrix (Reduced Model):")
print(reduced_conf_matrix)

# Calculate False Positive Rate, False Negative Rate, and Overall Error for Reduced Model
reduced_false_positive_rate <- reduced_conf_matrix[2, 1] / sum(reduced_conf_matrix[2, 1], reduced_conf_matrix[1, 1])
reduced_false_negative_rate <- reduced_conf_matrix[1, 2] / sum(reduced_conf_matrix[1, 2], reduced_conf_matrix[2, 2])
reduced_overall_error_rate <- sum(reduced_conf_matrix[1, 2], reduced_conf_matrix[2, 1]) / sum(reduced_conf_matrix)

cat("Reduced Model - False Positive Rate:", reduced_false_positive_rate, "\n")
cat("Reduced Model - False Negative Rate:", reduced_false_negative_rate, "\n")
cat("Reduced Model - Overall Error Rate:", reduced_overall_error_rate, "\n")

# Step 9: Compare Full vs Reduced Models
cat("\n=== VARIABLES COMPARISON ===\n")
cat("Full Model Variables:", length(coef(log_model)) - 1, "\n")
cat("Reduced Model Variables:", length(coef(log_model_step)) - 1, "\n")
cat("Variables Removed:", length(coef(log_model)) - length(coef(log_model_step)), "\n")

# Step 10: AUC Comparison for Full and Reduced Models
roc_full <- roc(testset$diabetes, log_pred_full, quiet = TRUE)
roc_reduced <- roc(testset$diabetes, log_pred_reduced, quiet = TRUE)

cat("Full Model AUC:", round(auc(roc_full), 4), "\n")
cat("Reduced Model AUC:", round(auc(roc_reduced), 4), "\n")
cat("AUC Difference:", round(auc(roc_full) - auc(roc_reduced), 4), "\n")

# Step 11: McFadden's R-Squared for Full and Reduced Models
mcfadden_full <- 1 - (log_model$deviance / log_model$null.deviance)
mcfadden_reduced <- 1 - (log_model_step$deviance / log_model_step$null.deviance)

cat("\n=== McFADDEN'S R-SQUARED ===\n")
cat("Full Model McFadden's R²:", round(mcfadden_full, 4), "\n")
cat("Reduced Model McFadden's R²:", round(mcfadden_reduced, 4), "\n")
cat("Difference:", round(mcfadden_full - mcfadden_reduced, 4), "\n")

# Step 12: Additional Metrics Comparison
comparison <- data.frame(
  Metric = c("Variables", "AIC", "BIC", "Deviance", "McFadden's R²", "AUC"),
  Full_Model = c(
    length(coef(log_model)) - 1,
    round(log_model$aic, 2),
    round(log_model$aic + (length(coef(log_model)) - 2) * (log(nrow(trainset)) - 2), 2),
    round(log_model$deviance, 2),
    round(mcfadden_full, 4),
    round(auc(roc_full), 4)
  ),
  Reduced_Model = c(
    length(coef(log_model_step)) - 1,
    round(log_model_step$aic, 2),
    round(log_model_step$aic + (length(coef(log_model_step)) - 2) * (log(nrow(trainset)) - 2), 2),
    round(log_model_step$deviance, 2),
    round(mcfadden_reduced, 4),
    round(auc(roc_reduced), 4)
  )
)

cat("\n=== MODEL COMPARISON TABLE ===\n")
print(comparison)


#6. Random Forest ====
# Load the randomForest package if not loaded
library(randomForest)

# Split the dataset into 70% training and 30% testing
set.seed(1)
train <- sample.split(sample_dt$diabetes, SplitRatio = 0.7)
trainset <- subset(sample_dt, train == TRUE)
testset <- subset(sample_dt, train == FALSE)

# Build the Random Forest model with adjusted parameters
# Adjusting mtry and nodesize, trying with ntree = 200 trees
rf_model_pruned <- randomForest(
  diabetes ~ ., 
  data = trainset, 
  ntree = 200,       # Increased number of trees
  mtry = 4,          # Try 4 features at each split (tune based on performance)
  nodesize = 10,     # Larger nodesize to avoid overly complex trees
  importance = TRUE  # Get feature importance
)

# Print the Random Forest model summary
print(rf_model_pruned)

# Make predictions on the test set
rf_pred_pruned <- predict(rf_model_pruned, newdata = testset)

# Confusion Matrix for Pruned Random Forest
conf_matrix_rf_pruned <- table(actual = testset$diabetes, model.pred = rf_pred_pruned)
print(conf_matrix_rf_pruned)

# Calculate performance metrics for Pruned Random Forest
false_positive_rate_rf_pruned <- conf_matrix_rf_pruned[2, 1] / sum(conf_matrix_rf_pruned[2, 1], conf_matrix_rf_pruned[1, 1])
false_negative_rate_rf_pruned <- conf_matrix_rf_pruned[1, 2] / sum(conf_matrix_rf_pruned[1, 2], conf_matrix_rf_pruned[2, 2])
overall_error_rate_rf_pruned <- sum(conf_matrix_rf_pruned[1, 2], conf_matrix_rf_pruned[2, 1]) / sum(conf_matrix_rf_pruned)

cat("Pruned Random Forest - False Positive Rate:", false_positive_rate_rf_pruned, "\n")
cat("Pruned Random Forest - False Negative Rate:", false_negative_rate_rf_pruned, "\n")
cat("Pruned Random Forest - Overall Error Rate:", overall_error_rate_rf_pruned, "\n")

# Calculate Accuracy for Pruned Random Forest
accuracy_rf_pruned <- sum(diag(conf_matrix_rf_pruned)) / sum(conf_matrix_rf_pruned)
cat("Pruned Random Forest - Accuracy:", accuracy_rf_pruned, "\n")

# Plot Feature Importance for the Random Forest Model
importance(rf_model_pruned)
varImpPlot(rf_model_pruned)

#7. Clustering====
library(ggplot2)
library(cluster)

# Select relevant numerical variables for clustering
# (We exclude categorical variables and the target variable 'diabetes')
cluster_data <- sample_dt[, c("age", "bmi", "HbA1c_level", "blood_glucose_level")]

# Scale the data (important so that variables are on the same scale)
cluster_data_scaled <- scale(cluster_data)

# Determine the optimal number of clusters using the Elbow Method
set.seed(123)
wcss <- sapply(1:10, function(k){
  kmeans(cluster_data_scaled, centers = k, nstart = 25)$tot.withinss
})

# Plot the Elbow Method
plot(1:10, wcss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal Number of Clusters")

# Based on the elbow plot, let's assume the optimal number of clusters is 3 (you can change this)
set.seed(123)
kmeans_model <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)

# Add the cluster assignment back to the dataset
sample_dt$cluster <- as.factor(kmeans_model$cluster)

# View basic info
cat("Cluster sizes:\n")
print(kmeans_model$size)

cat("\nCluster centers (scaled):\n")
print(kmeans_model$centers)

# Visualize the clusters (Age vs BMI example)
ggplot(sample_dt, aes(x = age, y = bmi, color = cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "K-Means Clustering of Sample Data (Age vs BMI)",
       x = "Age",
       y = "BMI") +
  theme_minimal()

# Compare cluster averages to understand group characteristics
cluster_summary <- aggregate(
  cbind(age, bmi, HbA1c_level, blood_glucose_level) ~ cluster,
  data = sample_dt,
  FUN = mean
)

cat("\nCluster Summary (Average values for each cluster):\n")
print(cluster_summary)

# OPTIONAL: Compare diabetes proportion in each cluster
cluster_diabetes <- table(sample_dt$cluster, sample_dt$diabetes)
cat("\nDiabetes distribution across clusters:\n")
print(cluster_diabetes)

# Normalize proportions
cat("\nProportion of diabetic vs non-diabetic individuals per cluster:\n")
print(prop.table(cluster_diabetes, 1))

# Calculate pseudo accuracy
cluster_diabetes <- table(sample_dt$cluster, sample_dt$diabetes)

# Define cluster 1 and 3 as diabetic, cluster 2 as non-diabetic
predicted <- ifelse(sample_dt$cluster %in% c(1, 3), 1, 0)

# Compute accuracy
accuracy <- mean(predicted == as.numeric(as.character(sample_dt$diabetes)))
cat("Clustering alignment accuracy:", round(accuracy * 100, 2), "%\n")

