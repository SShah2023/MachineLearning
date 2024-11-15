install.packages("caret")
install.packages("klaR")
install.packages("naivebayes")
install.packages("palmerpenguins")
library(lattice)
library(caret)
library(klaR)
library(palmerpenguins)
library(naivebayes)
library(e1071)

data("penguins")
x = penguins[,-5]
y = penguins$species

your_dataset <- na.omit(penguins)

set.seed(12345)
splitIndex <- sample(1:nrow(your_dataset), 0.7 * nrow(your_dataset))
train_data <- your_dataset[splitIndex, ]
test_data <- your_dataset[-splitIndex, ]

