install.packages("palmerpenguins")
install.packages("tidyverse")
install.packages('class')
library(class)
install.packages('caret')
library(caret)
library(tidyverse)
library(palmerpenguins)

# Load the dataset
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

# Test dataset
test_data <- your_dataset[-trainrows, 3:6]  # Remaining 30% test data
test_label <- your_dataset[-trainrows, 1]  # Dataframe for class labels

# Perform PCA on training data
set.seed(12345)
pca_model <- preProcess(train_data, method = c("center", "scale", "pca"))
train_data_pca <- predict(pca_model, train_data)
test_data_pca <- predict(pca_model, test_data)

# K-Nearest Neighbors with PCA and k=9
knn.9 <- knn(train = train_data_pca, test = test_data_pca, cl = train_label$species, k = 9)
knn.10 <- knn(train = train_data_pca, test = test_data_pca, cl = train_label$species, k = 10)

# Calculate the proportion of correct classification for k = 9
ACC.9 <- 100 * sum(test_label$species == knn.9) / NROW(test_label)
ACC.10 <- 100 * sum(test_label$species == knn.10) / NROW(test_label)

# Check prediction against actual value in tabular form for k=9
table(knn.9, test_label$species)
table(knn.10, test_label$species)

# Confusion matrix
confusionMatrix(table(knn.9, test_label$species))
confusionMatrix(table(knn.10, test_label$species))

# Hyperparameter tuning for k
k.optm <- numeric(100)
for (i in 1:100) {
  knn.mod <- knn(train = train_data_pca, test = test_data_pca, cl = train_label$species, k = i)
  k.optm[i] <- 100 * sum(test_label$species == knn.mod) / NROW(test_label)
  cat(i, '=', k.optm[i], ' ')
}

# Accuracy plot
plot(1:100, k.optm, type = "b", xlab = "K-Value", ylab = "Accuracy level")

# Cross-validation with PCA
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
    
    train_data_fold <- train_data_pca[train_indices, ]
    test_data_fold <- train_data_pca[test_indices, ]
    train_label_fold <- train_label$species[train_indices]
    test_label_fold <- train_label$species[test_indices]
    
    knn_mod <- knn(train = train_data_fold, test = test_data_fold, cl = train_label_fold, k = k)
    
    accuracy[i] <- 100 * sum(test_label_fold == knn_mod) / length(test_indices)
  }
  
  cv_results[k, "Accuracy"] <- mean(accuracy)
}

# Plot cross-validation results
plot(cv_results$K, cv_results$Accuracy, type = "b", xlab = "K-Value", ylab = "Cross-Validated Accuracy")
