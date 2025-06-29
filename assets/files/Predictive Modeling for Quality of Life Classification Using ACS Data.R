library(dplyr)
library(tidyr)
library(glmnet)
library(caret)
library(rsample)
library(pROC) # performance measures
library(mltools) # For MCC

library(e1071)   # For Naïve Bayes
library(class)   # For kNN
library(nnet)    # For Neural Networks
library(caTools) # for log reg
library(randomForest)
library(rpart) # Decision Tree
library(car)
library(performanceEstimation)  # For random oversampling
library(kernlab)


# Seed number
# sample(1:10000, 1)


# Read in data
df <- read.csv("project_data.csv")
df
a <- df["Class"]
# Check number of missing values
temp <- df
dim(temp)

sum(is.na(temp)) # number of missing values in the whole dataset
t1 <- sapply(temp, function(x) sum(is.na(x))) # missing values in individual columns


# Remove columns with more than 10% missing values
df <- df[, colMeans(is.na(df)) <= 0.1]


# Remove rows with multiple missing values
df_no_missing <- df %>% drop_na()
sapply(df_no_missing, class)
sapply(df_no_missing, function(x) sum(is.na(x)))


# Remove columns with only 1 unique values
df_filtered <- df_no_missing[, sapply(df, function(x) length(unique(x)) >= 2)]


# convert relevant integer variables to numeric
df_numeric <- df_filtered %>% mutate_at(c('SPORDER','PWGTP','INTP','OIP','PAP',
                                          'RETP','SEMP','SSIP','SSP',
                                          'WAGP','PERNP','PINCP','POVPIP'),
                                        as.numeric)

# Test removing outliers
cols <- c('SPORDER','PWGTP','INTP','OIP','PAP', 'RETP','SEMP','SSIP','SSP',
          'WAGP','PERNP','PINCP','POVPIP')

remove_outliers_multi <- function(df, cols) {
  for (col in cols) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower_bound <- Q1 - 6.0 * IQR_value
    upper_bound <- Q3 + 6.0 * IQR_value
    
    df <- df[df[[col]] >= lower_bound & df[[col]] <= upper_bound, ]
  }
  return(df)
}

df_cleaned <- remove_outliers_multi(df_numeric, cols)


# Since that removed nearly 50% of the rows we will cap them instead of remove
cap_outliers_multi <- function(df, cols) {
  for (col in cols) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    # Cap values within bounds
    df[[col]] <- pmax(lower_bound, pmin(df[[col]], upper_bound))
  }
  return(df)
}

df_capped <- cap_outliers_multi(df_numeric, cols)

df_capped$Class <- factor(df_capped$Class, levels = c("No", "Yes"))
df_processed <- df_capped[,-1]

# Remove zero-variance predictors
nzv <- nearZeroVar(df_processed)
if (length(nzv) > 0) df_processed <- df_processed[, -nzv]

# Remove highly correlated features
cor_matrix <- cor(df_processed[, sapply(df_processed, is.numeric)], use = "pairwise.complete.obs")
high_corr <- findCorrelation(cor_matrix, cutoff = 0.9, exact = TRUE)
if (length(high_corr) > 0) df_processed <- df_processed[, -high_corr]

write.csv(df_processed, "preprocessed_data.csv",row.names = FALSE)

#############################################################

TPR <- numeric(3)
FPR <- numeric(3)
Precision <- numeric(3)
Recall <- numeric(3)
F1 <- numeric(3)
MCC <- numeric(3)
Kappa <- numeric(3)
auc_value <- numeric(3)

# Balanced dataset methods(2): Class weights & random oversampling
# Attribute selection methods(3): Lasso, random forest, & recursive feature elimination
# Classification algorithms(6): SVM, logistic regression, KNN, naive bayes, neural network, Decision tree


## 1: Class weights, Lasso ,logisitic regression

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Get class weights
class_freq <- table(trainData$Class)
class_weights <- ifelse(trainData$Class == names(class_freq)[1], 
                        1 / class_freq[1], 
                        1 / class_freq[2])
class_weights <- ceiling(class_weights / min(class_weights)) # convert to whole numbers so it works with log reg

# Fit Lasso model (alpha = 1 for Lasso)
x <- as.matrix(trainData[, -which(names(trainData) == "Class")])
y <- trainData$Class
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_red <- trainData[, c(feature_names, "Class")]
test_red <- testData[, c(feature_names, "Class")]

# Scale data
train_reduced <- data.frame(scale(train_red[, -dim(train_red)[2]]))
test_reduced <- data.frame(scale(test_red[, -dim(train_red)[2]]))
train_reduced$Class <- train_red$Class
test_reduced$Class <- test_red$Class

grid <- expand.grid(
  alpha = c(0, 0.5, 1),         # Ridge, Elastic Net, Lasso
  lambda = 10^seq(-3, 1, length = 30)  # Log scale from 0.001 to 10
)

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

grid <- expand.grid(
  alpha = c(0, 0.5, 1),         # Ridge, Elastic Net, Lasso
  lambda = 10^seq(-3, 1, length = 30)  # Log scale from 0.001 to 10
)

model <- train(Class ~ ., 
               data = train_reduced, 
               method = "glmnet", 
               trControl = ctrl, 
               tuneGrid = grid, 
               metric = "F1", 
               family = "binomial",
               weights=class_weights)

print(model)
predictions <- predict(model, newdata = test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)


## 2: Class weights, Lasso , svm
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Get class weights
class_freq <- table(trainData$Class)
total_samples <- sum(class_freq)
class_weights <- total_samples / (length(class_freq) * class_freq)

x <- as.matrix(trainData[, -which(names(trainData) == "Class")])
y <- trainData$Class

# Fit Lasso model (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_red <- trainData[, c(feature_names, "Class")]
test_red <- testData[, c(feature_names, "Class")]

# Scale data
train_reduced <- data.frame(scale(train_red[, -dim(train_red)[2]]))
test_reduced <- data.frame(scale(test_red[, -dim(train_red)[2]]))
train_reduced$Class <- train_red$Class
test_reduced$Class <- test_red$Class


# Run SVM with features
svm_model <- svm(
  Class ~ ., 
  data = train_reduced,
  kernel = "linear",
  class.weights = class_weights
)
predictions <- predict(svm_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)


## 3: Class weights, Lasso , Random Forest
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Get class weights
class_freq <- table(trainData$Class)
total_samples <- sum(class_freq)
class_weights <- total_samples / (length(class_freq) * class_freq)

x <- as.matrix(trainData[, -which(names(trainData) == "Class")])
y <- trainData$Class

# Fit Lasso model (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- trainData[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]

model <- randomForest(Class ~ ., data=train_reduced, classwt=class_weights)
predictions <- predict(model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)


## 4: Class weights, Lasso , NB

# Naive Bayes doesn't require class weights since it automatically corrects for it
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

x <- as.matrix(trainData[, -which(names(trainData) == "Class")])
y <- trainData$Class

# Fit Lasso model (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- trainData[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]

nb_model <- naiveBayes(Class ~ ., data = train_reduced)
predictions <- predict(nb_model, test_reduced)


# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



## 5: Class weights, Lasso , NN

# Split train and test
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Class Weights
class_freq <- table(trainData$Class)
class_weights <- ifelse(trainData$Class == names(class_freq)[1], 
                        1 / class_freq[1], 
                        1 / class_freq[2])


x <- as.matrix(trainData[, -which(names(trainData) == "Class")])
y <- trainData$Class

# Fit Lasso model (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- trainData[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]

nn_model <- nnet(Class ~ ., data = train_reduced, size = 6, decay=0.15, maxit = 200, trace = FALSE,weights=class_weights)
nn_pred <- predict(nn_model, test_reduced, type = "class")
nn_pred <- as.factor(nn_pred)

# Measures for positive class
cm <- confusionMatrix(nn_pred, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = nn_pred, actuals = test_reduced$Class)
roc_obj <- roc(nn_pred, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(nn_pred, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = nn_pred, actuals = test_reduced$Class)
roc_obj <- roc(nn_pred, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 6: Class weights, Lasso , Decision tree

# Split train and test
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Class Weights
class_freq <- table(trainData$Class)
class_weights <- ifelse(trainData$Class == names(class_freq)[1], 
                        1 / class_freq[1], 
                        1 / class_freq[2])


x <- as.matrix(trainData[, -which(names(trainData) == "Class")])
y <- trainData$Class

# Fit Lasso model (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- trainData[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]

tree_model = rpart(Class ~ ., data = train_reduced, method = "class",weights =class_weights)
predictions <- predict(tree_model, test_reduced, type="class")

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 7: Class weights, recursive feature elimination ,log reg

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)


# Get class weights
class_freq <- table(trainData$Class)
class_weights <- ifelse(trainData$Class == names(class_freq)[1], 
                        1 / class_freq[1], 
                        1 / class_freq[2])
class_weights <- ceiling(class_weights / min(class_weights)) # convert to whole numbers so it works with log reg

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(trainData[, -ncol(trainData)], trainData$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run logistic regression with features
log_reg <- glm(Class ~ ., data = train_reduced, family=binomial, weights=class_weights)
pred_probs <- predict(log_reg, test_reduced, type = "response")
predictions <- ifelse(pred_probs > 0.5, 1, 0)

# Measures for positive class
cm <- confusionMatrix(factor(predictions,levels = c(0,1) ,labels=c("No","Yes")), test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = as.numeric(test_reduced$Class)-1)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(factor(predictions,levels = c(0,1) ,labels=c("No","Yes")), test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = as.numeric(test_reduced$Class)-1)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



#### 8: Class weights, recursive feature elimination , SVM

# Split data
set.seed(2281)


split <- initial_split(df_processed, prop = 0.66, strata = Class)

trainData <- training(split)
testData <- testing(split)

# Get class weights
class_freq <- table(trainData$Class)
total_samples <- sum(class_freq)
class_weights <- total_samples / (length(class_freq) * class_freq)

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(trainData[, -ncol(trainData)], trainData$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run svm
svm_model <- svm(
  Class ~ ., 
  data = train_reduced,
  kernel = "linear",
  class.weights = class_weights
)
predictions <- predict(svm_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




# 9: Class weights, recursive feature elimination , Random Forest

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)


# Get class weights
class_freq <- table(trainData$Class)
total_samples <- sum(class_freq)
class_weights <- total_samples / (length(class_freq) * class_freq)

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(trainData[, -ncol(trainData)], trainData$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

model <- randomForest(Class ~ ., data=train_reduced, classwt=class_weights)
predictions <- predict(model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)

# 10: Class weights,recursive feature elimination , NB

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(trainData[, -ncol(trainData)], trainData$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run NB
nb_model <- naiveBayes(Class ~ ., data = train_reduced)
predictions <- predict(nb_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 11: Class weights, recursive feature elimination , NN

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)


# Get class weights
class_freq <- table(trainData$Class)
class_weights <- ifelse(trainData$Class == names(class_freq)[1], 
                        1 / class_freq[1], 
                        1 / class_freq[2])
class_weights <- ceiling(class_weights / min(class_weights)) # convert to whole numbers so it works with log reg

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(trainData[, -ncol(trainData)], trainData$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run nnet
nn_model <- nnet(Class ~ ., data = train_reduced, size = 6, decay=0.15, maxit = 200, trace = FALSE,weights=class_weights)
nn_pred <- predict(nn_model, test_reduced, type = "class")
nn_pred <- as.factor(nn_pred)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




### 12: Class weights, recursive feature elimination , Decision tree

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)


# Get class weights
class_freq <- table(trainData$Class)
class_weights <- ifelse(trainData$Class == names(class_freq)[1], 
                        1 / class_freq[1], 
                        1 / class_freq[2])
class_weights <- ceiling(class_weights / min(class_weights)) # convert to whole numbers so it works with log reg

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(trainData[, -ncol(trainData)], trainData$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

tree_model = rpart(Class ~ ., data = train_reduced, method = "class",weights =class_weights)
predictions <- predict(tree_model, test_reduced, type="class")

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 13: Class weights, random forest ,log reg

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)


# Get class weights
class_freq <- table(trainData$Class)
class_weights <- ifelse(trainData$Class == names(class_freq)[1], 
                        1 / class_freq[1], 
                        1 / class_freq[2])
class_weights <- ceiling(class_weights / min(class_weights)) # convert to whole numbers so it works with log reg

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = trainData, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run logistic regression with features
log_reg <- glm(Class ~ ., data = train_reduced, family=binomial, weights=class_weights)
pred_probs <- predict(log_reg, test_reduced, type = "response")
predictions <- ifelse(pred_probs > 0.5, 1, 0)

# Measures for positive class
cm <- confusionMatrix(factor(predictions,levels = c(0,1) ,labels=c("No","Yes")), test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = as.numeric(test_reduced$Class)-1)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(factor(predictions,levels = c(0,1) ,labels=c("No","Yes")), test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = as.numeric(test_reduced$Class)-1)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




# 14: Class weights, random forest , SVM

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Get class weights
class_freq <- table(trainData$Class)
total_samples <- sum(class_freq)
class_weights <- total_samples / (length(class_freq) * class_freq)

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = trainData, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run SVM with features
svm_model <- svm(
  Class ~ ., 
  data = train_reduced,
  kernel = "linear",
  class.weights = class_weights
)

predictions <- predict(svm_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)

# 15: Class weights, random forest , Random Forest

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Get class weights
class_freq <- table(trainData$Class)
total_samples <- sum(class_freq)
class_weights <- total_samples / (length(class_freq) * class_freq)

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = trainData, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

model <- randomForest(Class ~ ., data=train_reduced, classwt=as.numeric(class_weights))
pred_probs <- predict(model, newdata = test_reduced, type = "prob")
predictions <- as.factor(ifelse(pred_probs[, "Yes"] > 0.07, "Yes", "No"))

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 16: Class weights, random forest , NB

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Get class weights
class_freq <- table(trainData$Class)
total_samples <- sum(class_freq)
class_weights <- total_samples / (length(class_freq) * class_freq)

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = trainData, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

nb_model <- naiveBayes(Class ~ ., data = train_reduced)
predictions <- predict(nb_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




### 17: Class weights, random forest , NN

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Get class weights
class_freq <- table(trainData$Class)
class_weights <- ifelse(trainData$Class == names(class_freq)[1], 
                        1 / class_freq[1], 
                        1 / class_freq[2])
class_weights <- ceiling(class_weights / min(class_weights)) 

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = trainData, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run nnet
nn_model <- nnet(Class ~ ., data = train_reduced, size = 6, decay=0.15, maxit = 200, trace = FALSE,weights=class_weights)
nn_pred <- predict(nn_model, test_reduced, type = "class")
nn_pred <- as.factor(nn_pred)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



# 18: Class weights, random forest , Decision tree

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Get class weights
class_freq <- table(trainData$Class)
class_weights <- ifelse(trainData$Class == names(class_freq)[1], 
                        1 / class_freq[1], 
                        1 / class_freq[2])
class_weights <- ceiling(class_weights / min(class_weights)) 

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = trainData, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- trainData[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

tree_model = rpart(Class ~ ., data = train_reduced, method = "class",weights =class_weights)
predictions <- predict(tree_model, test_reduced, type="class")

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)


### 19: Random oversampling, Lasso ,log reg

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)


# Fit Lasso model (alpha = 1 for Lasso)
x <- as.matrix(train_balanced[, -which(names(train_balanced) == "Class")])
y <- train_balanced$Class
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- train_balanced[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]


grid <- expand.grid(
  alpha = c(0, 0.5, 1),         # Ridge, Elastic Net, Lasso
  lambda = 10^seq(-3, 1, length = 30)  # Log scale from 0.001 to 10
)

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

grid <- expand.grid(
  alpha = c(0, 0.5, 1),         # Ridge, Elastic Net, Lasso
  lambda = 10^seq(-3, 1, length = 30)  # Log scale from 0.001 to 10
)

model <- train(Class ~ ., 
               data = train_reduced, 
               method = "glmnet", 
               trControl = ctrl, 
               tuneGrid = grid, 
               metric = "ROC", 
               family = "binomial")

print(model)
predictions <- predict(model, newdata = test_reduced)

# Run logistic regression with features
#log_reg <- glm(Class ~ ., data = train_reduced, family=binomial)
#pred_probs <- predict(log_reg, test_reduced, type = "response")
#predictions <- ifelse(pred_probs > 0.5, 1, 0)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 20: Random oversampling, Lasso , svm


# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Fit Lasso model (alpha = 1 for Lasso)
x <- as.matrix(train_balanced[, -which(names(train_balanced) == "Class")])
y <- train_balanced$Class
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- train_balanced[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

grid <- expand.grid(
  C = c(0.1, 1, 10)
)

model <- train(Class ~ ., 
               data = train_reduced, 
               method = "svmLinear", 
               trControl = ctrl, 
               tuneGrid = grid, 
               metric = "ROC", 
               family = "binomial")

print(model)
predictions <- predict(model, newdata = test_reduced)


# 
# # Run svm
# svm_model <- svm(
#   Class ~ .,
#   data = train_reduced,
#   kernel = "linear",
# )
# predictions <- predict(svm_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 21: Random oversampling, Lasso , Random Forest

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Fit Lasso model (alpha = 1 for Lasso)
x <- as.matrix(train_balanced[, -which(names(train_balanced) == "Class")])
y <- train_balanced$Class
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- train_balanced[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]


grid <- expand.grid(
  mtry = c(2, 3,4,5, 6,7, 8,9, 10)   # Try different numbers of variables per split
)

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

model <- train(Class ~ ., 
               data = train_reduced, 
               method = "rf", 
               trControl = ctrl, 
               tuneGrid = grid, 
               metric = "ROC", 
               family = "binomial")

print(model)
predictions <- predict(model, newdata = test_reduced)


# model <- randomForest(Class ~ ., data=train_reduced)
# predictions <- predict(model, test_reduced)


# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




### 22: Random oversampling, Lasso , NB

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Fit Lasso model (alpha = 1 for Lasso)
x <- as.matrix(train_balanced[, -which(names(train_balanced) == "Class")])
y <- train_balanced$Class
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- train_balanced[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]

grid <- expand.grid(
  laplace = c(0, 1),
  usekernel = c(TRUE, FALSE),
  adjust = c(0.5, 1, 2)
)

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

model <- train(Class ~ ., 
               data = train_reduced, 
               method = "naive_bayes", 
               trControl = ctrl, 
               tuneGrid = grid, 
               metric = "ROC")

print(model)
predictions <- predict(model, newdata = test_reduced)

# # Run NB
# nb_model <- naiveBayes(Class ~ ., data = train_reduced)
# predictions <- predict(nb_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




### 23: Random oversampling, Lasso , NN

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Fit Lasso model (alpha = 1 for Lasso)
x <- as.matrix(train_balanced[, -which(names(train_balanced) == "Class")])
y <- train_balanced$Class
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- train_balanced[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]

# Run nnet
nn_model <- nnet(Class ~ ., data = train_reduced, size = 6, decay=0.15, maxit = 200, trace = FALSE)
nn_pred <- predict(nn_model, test_reduced, type = "class")
nn_pred <- as.factor(nn_pred)

# Measures for positive class
cm <- confusionMatrix(nn_pred, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(nn_pred, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = nn_pred, actuals = test_reduced$Class)
roc_obj <- roc(nn_pred, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




### 24: Random oversampling, Lasso , Decision tree

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Fit Lasso model (alpha = 1 for Lasso)
x <- as.matrix(train_balanced[, -which(names(train_balanced) == "Class")])
y <- train_balanced$Class
lasso_model <- cv.glmnet(x, y, alpha = 1, family = "binomial")

# Select features from Lasso
coef_matrix <- coef(lasso_model, s = "lambda.min")
non_zero_indices <- which(coef_matrix[-1] != 0)  # Exclude intercept
feature_names <- colnames(x)[non_zero_indices]

cat("Selected feature names:", paste(feature_names, collapse=", "), "\n")
train_reduced <- train_balanced[, c(feature_names, "Class")]
test_reduced <- testData[, c(feature_names, "Class")]

tree_model = rpart(Class ~ ., data = train_reduced, method = "class")
predictions <- predict(tree_model, test_reduced, type="class")

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 25: Random oversampling, recursive feature elimination ,log reg

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(train_balanced[, -ncol(train_balanced)], train_balanced$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

grid <- expand.grid(
  alpha = c(0, 0.5, 1),         # Ridge, Elastic Net, Lasso
  lambda = 10^seq(-3, 1, length = 30)  # Log scale from 0.001 to 10
)

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

grid <- expand.grid(
  alpha = c(0, 0.5, 1),         # Ridge, Elastic Net, Lasso
  lambda = 10^seq(-3, 1, length = 30)  # Log scale from 0.001 to 10
)

model <- train(Class ~ ., 
               data = train_reduced, 
               method = "glmnet", 
               trControl = ctrl, 
               tuneGrid = grid, 
               metric = "ROC", 
               family = "binomial")

print(model)
predictions <- predict(model, newdata = test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")
cm

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight

# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




### 26: Random oversampling, recursive feature elimination , svm

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(train_balanced[, -ncol(train_balanced)], train_balanced$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run SVM with features
svm_model <- svm(
  Class ~ ., 
  data = train_reduced,
  kernel = "linear"
)

predictions <- predict(svm_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




### 27: Random oversampling, recursive feature elimination , Random Forest


# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(train_balanced[, -ncol(train_balanced)], train_balanced$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

model <- randomForest(Class ~ ., data=train_reduced)
predictions <- predict(model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 28: Random oversampling,recursive feature elimination , NB

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(train_balanced[, -ncol(train_balanced)], train_balanced$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

nb_model <- naiveBayes(Class ~ ., data = train_reduced)
predictions <- predict(nb_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




### 29: Random oversampling, recursive feature elimination , NN

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(train_balanced[, -ncol(train_balanced)], train_balanced$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run nnet
nn_model <- nnet(Class ~ ., data = train_reduced, size = 6, decay=0.15, maxit = 200, trace = FALSE)
nn_pred <- predict(nn_model, test_reduced, type = "class")
nn_pred <- as.factor(nn_pred)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



### 30: Random oversampling, recursive feature elimination , Decision tree

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use recursive feature elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
rfe_model <- rfe(train_balanced[, -ncol(train_balanced)], train_balanced$Class, sizes = c(1:20), rfeControl = control)
selected_features <- predictors(rfe_model)
train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

tree_model = rpart(Class ~ ., data = train_reduced, method = "class")
predictions <- predict(tree_model, test_reduced, type="class")

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)


### 31: Random oversampling, random forest , log reg

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = train_balanced, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

grid <- expand.grid(
  alpha = c(0, 0.5, 1),         # Ridge, Elastic Net, Lasso
  lambda = 10^seq(-3, 1, length = 30)  # Log scale from 0.001 to 10
)

ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

grid <- expand.grid(
  alpha = c(0, 0.5, 1),         # Ridge, Elastic Net, Lasso
  lambda = 10^seq(-3, 1, length = 10)  # Log scale from 0.001 to 10
)

model <- train(Class ~ ., 
               data = train_reduced, 
               method = "glmnet", 
               trControl = ctrl, 
               tuneGrid = grid, 
               metric = "F1", 
               family = "binomial")

print(model)
predictions <- predict(model, newdata = test_reduced)


# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




# 32: Random oversampling, random forest , svm

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = train_balanced, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run SVM with features
svm_model <- svm(
  Class ~ ., 
  data = train_reduced,
  kernel = "linear"
)

predictions <- predict(svm_model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




# 33: Random oversampling, random forest , Random Forest

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = train_balanced, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]


model <- randomForest(Class ~ ., data=train_reduced)
predictions <- predict(model, test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



# 34: Random oversampling, random forest , NB

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = train_balanced, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]


grid <- expand.grid(
  laplace = c(0, 1),
  usekernel = c(TRUE, FALSE),
  adjust = c(0.5, 1, 2)
)

ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

model <- train(
  Class ~ ., 
  data = train_reduced, 
  method = "naive_bayes", 
  trControl = ctrl,
  tuneGrid = grid,
  metric = "ROC"
)
predictions <- predict(model, newdata = test_reduced)

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)



# 35: Random oversampling, random forest , NN

# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = train_balanced, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

# Run nnet
nn_model <- nnet(Class ~ ., data = train_reduced, size = 6, decay=0.15, maxit = 200, trace = FALSE)
nn_pred <- predict(nn_model, test_reduced, type = "class")
nn_pred <- as.factor(nn_pred)

# Measures for positive class
cm <- confusionMatrix(nn_pred, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = nn_pred, actuals = test_reduced$Class)
roc_obj <- roc(nn_pred, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(nn_pred, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = nn_pred, actuals = test_reduced$Class)
roc_obj <- roc(nn_pred, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)




# 36: Random oversampling, random forest , Decision tree


# Split data
set.seed(2281)
split <- initial_split(df_processed, prop = 0.66, strata = Class)
trainData <- training(split)
testData <- testing(split)

# Random oversampling using SMOTE
train_balanced <- smote(Class ~ ., data = trainData, perc.under = 2)

# Use random forest feature selection
rf_model <- randomForest(Class ~ ., data = train_balanced, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance_scores), importance_scores)
threshold <- mean(importance_df$MeanDecreaseGini)  # Select above average importance
selected_features <- importance_df$Feature[importance_df$MeanDecreaseGini > threshold]

train_reduced <- train_balanced[, c(selected_features, "Class")]
test_reduced <- testData[, c(selected_features, "Class")]

tree_model = rpart(Class ~ ., data = train_reduced, method = "class")
predictions <- predict(tree_model, test_reduced, type="class")

# Measures for positive class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "Yes")
cm

TPR[1] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[1] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[1] <- cm$byClass["Precision"]  
Recall[1] <- cm$byClass["Recall"]  # Same as TPR
F1[1] <- cm$byClass["F1"]  # F-measure
Kappa[1] <- cm$overall["Kappa"]  # Kappa
MCC[1] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[1] <- auc(roc_obj)

# Measures for Negative class
cm <- confusionMatrix(predictions, test_reduced$Class, positive = "No")

TPR[2] <- cm$byClass["Sensitivity"]   # True Positive Rate (Recall)
FPR[2] <- 1 - cm$byClass["Specificity"]  # False Positive Rate
Precision[2] <- cm$byClass["Precision"]  
Recall[2] <- cm$byClass["Recall"]  # Same as TPR
F1[2] <- cm$byClass["F1"]  # F-measure
Kappa[2] <- cm$overall["Kappa"]  # Kappa
MCC[2] <- mltools::mcc(preds = predictions, actuals = test_reduced$Class)
roc_obj <- roc(predictions, as.numeric(test_reduced$Class)-1)
auc_value[2] <- auc(roc_obj)

# Weighted average
t <- table(test_reduced$Class)
pos_weight = t["Yes"]/(t["Yes"] + t["No"])
neg_weight = t["No"]/(t["Yes"] + t["No"])

TPR[3] = TPR[1]*pos_weight + TPR[2]*neg_weight
FPR[3] = FPR[1]*pos_weight + FPR[2]*neg_weight
Precision[3] = Precision[1]*pos_weight + Precision[2]*neg_weight
Recall[3] = Recall[1]*pos_weight + Recall[2]*neg_weight
F1[3] = F1[1]*pos_weight + F1[2]*neg_weight
MCC[3] = MCC[1]*pos_weight + MCC[2]*neg_weight
Kappa[3] = Kappa[1]*pos_weight + Kappa[2]*neg_weight
auc_value[3] = auc_value[1]*pos_weight + auc_value[2]*neg_weight


# Print results
data.frame(TPR, FPR, Precision, Recall, F1, MCC, Kappa,auc_value)


