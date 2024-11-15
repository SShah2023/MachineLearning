import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, LabelEncoder

def load_and_preprocess_data(filepath):
    data = pd.read_csv(filepath)
    le = LabelEncoder()
    categorical_columns = ['sex', 'island']  # Replace with actual categorical columns
    for col in categorical_columns:
        data[col] = le.fit_transform(data[col])

    data = pd.get_dummies(data, columns=['species'])
    scaler = StandardScaler()
    feature_columns = [col for col in data.columns if col != 'target_column']
    data[feature_columns] = scaler.fit_transform(data[feature_columns])

    return data

# dataset = load_and_preprocess_data('path_to_your_dataset.csv')
class NeuralNetwork:
    def __init__(self, input_size, hidden_sizes, output_size):
        self.weights_hidden = [np.random.randn(input_size, hidden_sizes[0])]
        self.biases_hidden = [np.zeros((1, hidden_sizes[0]))]
        self.weights_output = np.random.randn(hidden_sizes[-1], output_size)
        self.biases_output = np.zeros((1, output_size))

    def sigmoid(self, x):
        return 1 / (1 + np.exp(-x))

    def sigmoid_derivative(self, x):
        return x * (1 - x)

    def forward(self, X):
        self.hidden_layer_input = np.dot(X, self.weights_hidden[0]) + self.biases_hidden[0]
        self.hidden_layer_output = self.sigmoid(self.hidden_layer_input)
        self.output_layer_input = np.dot(self.hidden_layer_output, self.weights_output) + self.biases_output
        return self.sigmoid(self.output_layer_input)

    def backward(self, X, y, y_pred):
        output_error = y - y_pred
        output_delta = output_error * self.sigmoid_derivative(y_pred)

        hidden_error = output_delta.dot(self.weights_output.T)
        hidden_delta = hidden_error * self.sigmoid_derivative(self.hidden_layer_output)

        self.weights_output += self.hidden_layer_output.T.dot(output_delta)
        self.weights_hidden[0] += X.T.dot(hidden_delta)

    def train(self, X, y, epochs, learning_rate):
        for epoch in range(epochs):
            y_pred = self.forward(X)
            self.backward(X, y, y_pred)

# Split the dataset
X = dataset.drop('target_column', axis=1)
y = dataset['target_column']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Initialize and train the neural network
nn = NeuralNetwork(input_size=X_train.shape[1], hidden_sizes=[10], output_size=1)
nn.train(X_train, y_train, epochs=100, learning_rate=0.01)

# Evaluate the model
def evaluate_model(model, X, y):
    y_pred = model.forward(X)
    accuracy = np.mean((y_pred > 0.5).astype(int) == y)
    return accuracy

train_accuracy = evaluate_model(nn, X_train, y_train)
test_accuracy = evaluate_model(nn, X_test, y_test)
print(f"Training Accuracy: {train_accuracy}")
print(f"Testing Accuracy: {test_accuracy}")

from sklearn.model_selection import KFold

def cross_validation_score(model, X, y, num_folds=5):
    kf = KFold(n_splits=num_folds)
    accuracies = []
    for train_index, test_index in kf.split(X):
        X_train, X_test = X.iloc[train_index], X.iloc[test_index]
        y_train, y_test = y[train_index], y[test_index]
        model.train(X_train, y_train, epochs=100, learning_rate=0.01)
        accuracies.append(evaluate_model(model, X_test, y_test))
    return np.mean(accuracies)

cv_accuracy = cross_validation_score(NeuralNetwork(input_size=X.shape[1], hidden_sizes=[10], output_size=1), X, y)
print(f"Cross-Validation Accuracy: {cv_accuracy}")



def plot_learning_curves(model, X, y, epochs=100, learning_rate=0.01):
    train_accuracies = []
    test_accuracies = []
    for epoch in range(epochs):
        model.train(X_train, y_train, epochs=1, learning_rate=learning_rate)
        train_accuracies.append(evaluate_model(model, X_train, y_train))
        test_accuracies.append(evaluate_model(model, X_test, y_test))

    plt.plot(train_accuracies, label='Train Accuracy')
    plt.plot(test_accuracies, label='Test Accuracy')
    plt.xlabel('Epochs')
    plt.ylabel('Accuracy')
    plt.legend()
    plt.show()

plot_learning_curves(nn, X, y)
