install.packages("palmerpenguins")
install.packages("tidyverse")
install.packages("caret")
install.packages("readr")
install.packages("ggplot2")

library(readr)
library(ggplot2)
library(palmerpenguins)
library(tidyverse)
library(caret)

# Perceptron function definition
Perceptron <- function(learning_rate) {
  weights <- NULL
  
  initialize_weights <- function(n) {
    weights <<- runif(n + 1, -1, 1)  # Including bias, weights initialized randomly
  }
  
  predict <- function(features) {
    sum(weights * c(1, features))  # Bias is the first weight
  }
  
  train <- function(features, label) {
    n <- ncol(features)  # Get the number of features
    initialize_weights(n)
    for (epoch in 1:100) {  # Assuming 100 epochs
      for (i in 1:nrow(features)) {
        net_input <- predict(features[i, , drop = FALSE])
        output <- ifelse(net_input >= 0, 1, -1)
        error <- label[i] - output
        weights <<- weights + learning_rate * error * c(1, features[i, ])
      }
    }
  }
  
  get_weights <- function() {
    weights
  }
  
  list(predict = predict, train = train, get_weights = get_weights)
}

# Load and preprocess the Palmer Penguins dataset
data(penguins)
penguins <- na.omit(penguins)
penguins$species <- ifelse(penguins$species == "Adelie", -1, 1)  # Binary classification

# Visualize the dataset (Optional)
ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm)) +
  geom_point(aes(color = as.factor(species), shape = as.factor(species)), size = 3) +
  xlab("Bill Length (mm)") +
  ylab("Flipper Length (mm)") +
  ggtitle("Penguin Species vs Bill and Flipper Lengths")

X <- as.matrix(penguins[, c("bill_length_mm", "flipper_length_mm")])
Y <- matrix(penguins$species, ncol = 1)

Perceptron <- function(learning_rate) {
  # Prepare data
  X <- as.matrix(penguins[, c("bill_length_mm", "flipper_length_mm")])
  Y <- matrix(ifelse(penguins$species == "Adelie", -1, 1), ncol = 1)
  
  # Sigmoid function and its derivative
  sigmoid <- function(x) {
    1 / (1 + exp(-x))
  }
  derivatives_sigmoid <- function(x) {
    x * (1 - x)
  }
  
  # Neural network parameters
  inputlayer_neurons <- ncol(X)
  hiddenlayer_neurons <- 3
  output_neurons <- 1
  
  # Initialize weights and biases
  set.seed(123)  # For reproducibility
  wh <- matrix(rnorm(inputlayer_neurons * hiddenlayer_neurons), inputlayer_neurons, hiddenlayer_neurons)
  bh <- matrix(runif(hiddenlayer_neurons), nrow = 1)
  wout <- matrix(rnorm(hiddenlayer_neurons * output_neurons), hiddenlayer_neurons, output_neurons)
  bout <- matrix(runif(output_neurons), nrow = 1)
  
  # Training loop
  for(i in 1:5000) {
    # Forward propagation
    hidden_layer_input <- X %*% wh + matrix(rep(bh, nrow(X), each = TRUE), nrow(X), ncol(bh))
    hidden_layer_activations <- sigmoid(hidden_layer_input)
    output_layer_input <- hidden_layer_activations %*% wout + matrix(rep(bout, nrow(X), each = TRUE), nrow(X), ncol(bout))
    output <- sigmoid(output_layer_input)
    
    # Backpropagation
    E <- Y - output
    d_output <- E * derivatives_sigmoid(output)
    Error_at_hidden_layer <- d_output %*% t(wout)
    d_hiddenlayer <- Error_at_hidden_layer * derivatives_sigmoid(hidden_layer_activations)
    
    # Update weights and biases
    wout <- wout + t(hidden_layer_activations) %*% d_output * 0.1
    bout <- bout + colSums(d_output) * 0.1
    wh <- wh + t(X) %*% d_hiddenlayer * 0.1
    bh <- bh + colSums(d_hiddenlayer) * 0.1
  }
  
  # Check output
  output
  
}

# Split the data into training and testing sets
set.seed(12345)
trainIndex <- createDataPartition(penguins$species, p = 0.7, list = FALSE)
train_data <- penguins[trainIndex, ]
test_data <- penguins[-trainIndex, ]

# Initialize the Perceptron model
p_model <- Perceptron(0.01)

# Prepare the data for training
train_features <- as.matrix(train_data[, c("bill_length_mm", "flipper_length_mm")])
train_labels <- train_data$species

for (i in 1:nrow(train_data)) {
  # Ensuring that each row is kept as a matrix
  feature_row <- train_features[i, , drop = FALSE]
  p_model$train(feature_row, train_labels[i])
}

# Predicting and Evaluating Accuracy
test_features <- as.matrix(test_data[, c("bill_length_mm", "flipper_length_mm")])
test_labels <- test_data$species
correct_predictions <- 0

for (i in 1:nrow(test_data)) {
  prediction <- ifelse(p_model$predict(test_features[i, , drop = FALSE]) >= 0, 1, -1)
  if (prediction == test_labels[i]) correct_predictions <- correct_predictions + 1
}

accuracy <- correct_predictions / nrow(test_data)
print(paste("Test Accuracy: ", accuracy))

# Tuning Learning Rates
learning_rates <- c(0.1, 0.05, 0.01, 0.005, 0.001)
accuracies <- numeric(length(learning_rates))

for (lr in learning_rates) {
  p_model <- Perceptron(lr)
  for (i in 1:nrow(train_data)) {
    p_model$train(train_features[i, , drop = FALSE], train_labels[i])
  }
  
  correct_predictions <- 0
  for (i in 1:nrow(test_data)) {
    prediction <- ifelse(p_model$predict(test_features[i, , drop = FALSE]) >= 0, 1, -1)
    if (prediction == test_labels[i]) correct_predictions <- correct_predictions + 1
  }
  
  accuracies[which(lr == learning_rates)] <- correct_predictions / nrow(test_data)
}

# Plotting learning rate vs accuracy
plot(learning_rates, accuracies, type = "b", xlab = "Learning Rate", ylab = "Accuracy")
