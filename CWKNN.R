install.packages("palmerpenguins")
install.packages("tidyverse")
install.packages('class')
library(class)
install.packages('caret')
library(caret)
library(tidyverse)
library(palmerpenguins)
str(penguins)
your_dataset <- penguins

# Check for missing values and remove them
any(is.na(your_dataset))
your_dataset <- na.omit(your_dataset)

# Split the data into 70% training and 30% testing
set.seed(12345)
trainrows <- sample(1:nrow(your_dataset), replace = FALSE, size = nrow(your_dataset) * 0.7)

# Training set
train_data <- your_dataset[trainrows, 3:6]  # 70% training data
train_label <- your_dataset[trainrows, 1]  # Dataframe for the class labels
table(train_label)
# Test dataset
test_data <- your_dataset[-trainrows, 3:6]  # Remaining 30% test data
test_label <- your_dataset[-trainrows, 1]  # Dataframe for class labels
table(test_label)

NROW(train_label)
NROW(test_label)

# K-Nearest Neighbors with k=9
knn.9 <- knn(train = train_data, test = test_data, cl = train_label$species, k = 9)
knn.10 <- knn(train = train_data, test = test_data, cl = train_label$species, k = 10)


# Calculate the proportion of correct classification for k = 9
ACC.9 <- 100 * sum(test_label$species == knn.9) / NROW(test_label)
ACC.10 <- 100 * sum(test_label$species == knn.10) / NROW(test_label)
ACC.9
ACC.10

# Check prediction against actual value in tabular form for k=9
table(knn.9, test_label$species)
table(knn.10, test_label$species)

# Confusion matrix
confusionMatrix(table(knn.9, test_label$species))
confusionMatrix(table(knn.10, test_label$species))

# Hyperparameter tuning for k
k.optm <- numeric(100)
for (i in 1:100) {
  knn.mod <- knn(train = train_data, test = test_data, cl = train_label$species, k = i)
  k.optm[i] <- 100 * sum(test_label$species == knn.mod) / NROW(test_label)
  cat(i, '=', k.optm[i], ' ')
}

# Accuracy plot without K-fold
plot(1:100, k.optm, type = "b", xlab = "K-Value", ylab = "Accuracy level")

# Cross validation
# Define the number of folds (e.g., 10-fold cross-validation)
num_folds <- 10

# Create a data frame to store cross-validation results
cv_results <- data.frame(K = 1:10, Accuracy = numeric(10))

# Perform k-fold cross-validation
for (k in 1:10) {
  set.seed(12345)  # Set a seed for reproducibility
  folds <- createFolds(train_label$species, k = num_folds, list = TRUE)
  accuracy <- numeric(num_folds)
  
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[i])
    test_indices <- setdiff(1:NROW(train_label), train_indices)
    
    train_data_fold <- train_data[train_indices, ]
    test_data_fold <- train_data[test_indices, ]
    train_label_fold <- train_label$species[train_indices]
    test_label_fold <- train_label$species[test_indices]
    
    knn_mod <- knn(train = train_data_fold, test = test_data_fold, cl = train_label_fold, k = k)
    
    accuracy[i] <- 100 * sum(test_label_fold == knn_mod) / length(test_indices)
  }
  
  cv_results[k, "Accuracy"] <- mean(accuracy)
}

# Plot cross-validation results
plot(cv_results$K, cv_results$Accuracy, type = "b", xlab = "K-Value", ylab = "Cross-Validated Accuracy")




#Applying PCA
set.seed(12345)
pca_model <- preProcess(train_data, method = c("center", "scale", "pca"))
train_data_pca <- predict(pca_model, train_data)
test_data_pca <- predict(pca_model, test_data)

# K-Nearest Neighbors with PCA and k=9
knn.9_pca <- knn(train = train_data_pca, test = test_data_pca, cl = train_label$species, k = 9)
knn.10_pca <- knn(train = train_data_pca, test = test_data_pca, cl = train_label$species, k = 10)

# Calculate the proportion of correct classification for k = 9 with PCA
ACC.9_pca <- 100 * sum(test_label$species == knn.9_pca) / NROW(test_label)
ACC.10_pca <- 100 * sum(test_label$species == knn.10_pca) / NROW(test_label)
ACC.9_pca
ACC.10_pca

# Check prediction against actual value in tabular form for k=9 with PCA
table(knn.9_pca, test_label$species)
table(knn.10_pca, test_label$species)

# Confusion matrix with PCA
confusionMatrix(table(knn.9_pca, test_label$species))
confusionMatrix(table(knn.10_pca, test_label$species))

# Hyperparameter tuning for k with PCA
k.optm_pca <- numeric(100)
for (i in 1:100) {
  knn.mod_pca <- knn(train = train_data_pca, test = test_data_pca, cl = train_label$species, k = i)
  k.optm_pca[i] <- 100 * sum(test_label$species == knn.mod_pca) / NROW(test_label)
  cat(i, '=', k.optm_pca[i], ' ')
}

# Accuracy plot with PCA with K-fold
plot(1:100, k.optm_pca, type = "b", xlab = "K-Value", ylab = "Accuracy level")

num_folds <- 10

# Create a data frame to store cross-validation results with PCA
cv_results_pca <- data.frame(K = 1:10, Accuracy = numeric(10))
                             
# Perform k-fold cross-validation with PCA
for (k in 1:10) {
  set.seed(12345)  # Set a seed for reproducibility
  folds <- createFolds(train_label$species, k = num_folds, list = TRUE)
  accuracy_pca <- numeric(num_folds)
                               
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[i])
    test_indices <- setdiff(1:NROW(train_label), train_indices)
                                 
    train_data_fold_pca <- train_data_pca[train_indices, ]
    test_data_fold_pca <- train_data_pca[test_indices, ]
    train_label_fold_pca <- train_label$species[train_indices]
    test_label_fold_pca <- train_label$species[test_indices]
                                 
    knn_mod_pca <- knn(train = train_data_fold_pca, test = test_data_fold_pca, cl = train_label_fold_pca, k = k)
                                 
    accuracy_pca[i] <- 100 * sum(test_label_fold_pca == knn_mod_pca) / length(test_indices)
                               }
                               
  cv_results_pca[k, "Accuracy"] <- mean(accuracy_pca)
                             }
                             
# Plot cross-validation results with PCA and K-fold
plot(cv_results_pca$K, cv_results_pca$Accuracy, type = "b", xlab = "K-Value", ylab = "Cross-Validated Accuracy")

# Hyperparameter tuning and cross-validation
k.optm_pca <- numeric(100)
k.optm <- numeric(100)

# Create a data frame to store cross-validation results for PCA and non-PCA
cv_results <- data.frame(K = 1:10, Accuracy = numeric(10))
cv_results_pca <- data.frame(K = 1:10, Accuracy = numeric(10))

for (k in 1:10) {
  set.seed(12345) 
  folds <- createFolds(train_label$species, k = num_folds, list = TRUE)
  accuracy <- numeric(num_folds)
  accuracy_pca <- numeric(num_folds)
  
  for (i in 1:num_folds) {
    train_indices <- unlist(folds[i])
    test_indices <- setdiff(1:NROW(train_label), train_indices)
    
    train_data_fold <- train_data[train_indices, ]
    test_data_fold <- train_data[test_indices, ]
    train_data_fold_pca <- train_data_pca[train_indices, ]
    test_data_fold_pca <- train_data_pca[test_indices, ]
    
    train_label_fold <- train_label$species[train_indices]
    test_label_fold <- train_label$species[test_indices]
    
    knn_mod <- knn(train = train_data_fold, test = test_data_fold, cl = train_label_fold, k = k)
    knn_mod_pca <- knn(train = train_data_fold_pca, test = test_data_fold_pca, cl = train_label_fold, k = k)
    
    accuracy[i] <- 100 * sum(test_label_fold == knn_mod) / length(test_indices)
    accuracy_pca[i] <- 100 * sum(test_label_fold == knn_mod_pca) / length(test_indices)
  }
  
  cv_results[k, "Accuracy"] <- mean(accuracy)
  cv_results_pca[k, "Accuracy"] <- mean(accuracy_pca)
}
par(mfrow=c(1,2))  

plot(cv_results$K, cv_results$Accuracy, type = "b", xlab = "K-Value", ylab = "Cross-Validated Accuracy (Non-PCA)", col = "blue")

plot(cv_results_pca$K, cv_results_pca$Accuracy, type = "b", xlab = "K-Value", ylab = "Cross-Validated Accuracy (PCA)", col = "red")

par(mfrow=c(1,1))

train_errors_no_pca <- numeric(100)
test_errors_no_pca <- numeric(100)

for (i in 1:100) {
  knn_mod_no_pca <- knn(train = train_data, test = train_data, cl = train_label$species, k = i)
  train_errors_no_pca[i] <- 100 * sum(train_label$species != knn_mod_no_pca) / NROW(train_label)
  
  knn_mod_no_pca <- knn(train = train_data, test = test_data, cl = train_label$species, k = i)
  test_errors_no_pca[i] <- 100 * sum(test_label$species != knn_mod_no_pca) / NROW(test_label)
}

plot(1:100, train_errors_no_pca, type = "b", xlab = "K-Value", ylab = "Error Rate (%)", col = "blue", pch = 16, main = "Training and Testing Errors without PCA")
points(1:100, test_errors_no_pca, type = "b", col = "red", pch = 16)
legend("topright", legend = c("Training Error", "Testing Error"), col = c("blue", "red"), lty = 1)

best_k_no_pca <- which.min(test_errors_no_pca)
abline(v = best_k_no_pca, col = "blue", lty = 2)
legend("topright", legend = c("Best K without PCA"), col = c("blue"), lty = c(2))

train_errors_pca <- numeric(100)
test_errors_pca <- numeric(100)

for (i in 1:100) {
  knn_mod_pca <- knn(train = train_data_pca, test = train_data_pca, cl = train_label$species, k = i)
  train_errors_pca[i] <- 100 * sum(train_label$species != knn_mod_pca) / NROW(train_label)
  
  knn_mod_pca <- knn(train = train_data_pca, test = test_data_pca, cl = train_label$species, k = i)
  test_errors_pca[i] <- 100 * sum(test_label$species != knn_mod_pca) / NROW(test_label)
}

plot(1:100, train_errors_pca, type = "b", xlab = "K-Value", ylab = "Error Rate (%)", col = "blue", pch = 16, main = "Training and Testing Errors with PCA")
points(1:100, test_errors_pca, type = "b", col = "red", pch = 16)
legend("topright", legend = c("Training Error", "Testing Error"), col = c("blue", "red"), lty = 1)

best_k_pca <- cv_results_pca$K[which.max(cv_results_pca$Accuracy)]
abline(v = best_k_pca, col = "blue", lty = 2)
legend("topright", legend = c("Best K with PCA"), col = c("blue"), lty = c(2))

